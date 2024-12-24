use std::collections::{HashMap, HashSet};
use std::fmt;

use anyhow::bail;
use hashlink::LinkedHashMap;
use heck::{ToKebabCase, ToPascalCase};
use itertools::Itertools;
use weedle::{Definition, Definitions as WebIdlDefinitions};
use wit_encoder::{Ident, Interface, InterfaceItem, StandaloneFunc, World};

/// conversion options.
#[derive(Clone, Debug)]
pub struct ConversionOptions {
    /// Name of package for generated wit.
    ///
    /// When using the outputted wit in a JS environment, it is recommended to use the "webidl:"
    /// namespace.
    ///
    /// This lets tools like Jco know that this wit represents bindings to built in functions.
    ///
    /// Example
    /// ```
    /// # use webidl2wit::PackageName;
    /// PackageName::new("webidl", "my-package", None);
    /// ```
    ///
    pub package_name: crate::PackageName,
    /// Interface to hold the generated types and functions.
    pub interface_name: String,
    /// When set, treats the given interface name as a singleton, flattening
    /// its functions as top-level interface functions of an interface of the given name.
    ///
    /// When using the outputted wit in a JS environment, it is recommended to make this name
    /// match the global name of the interface, with a `global-` prefix, for transparent runtime
    /// support in Jco.
    ///
    /// For example in Jco, `globalThis.console` or `globalThis.navigator.gpu` can be reflected as
    /// global-console or global-navigator-gpu respectively to automatically bind these globals.
    pub singleton_interface: Option<String>,
    /// Skip unsupported features.
    pub unsupported_features: HandleUnsupported,
    /// Items - usually global singletons - that if encountered should get a get-[self] func, and get a dedicated world.
    pub global_singletons: HashSet<String>,
    /// Add empty interfaces with these names.
    /// Useful if the WebIDL references interfaces not defined in the input.
    pub phantom_interface: Vec<String>,
    /// Add empty dictionary with these names.
    /// Useful if the WebIDL references dictionaries not defined in the input.
    pub phantom_dictionaries: Vec<String>,
    /// How to handle resource inheritance.
    pub resource_inheritance: ResourceInheritance,
}

#[derive(Clone, Debug, Default)]
pub enum HandleUnsupported {
    /// Bail on unsupported features (default)
    #[default]
    Bail,
    /// Skip unsupported features
    Skip,
    /// Skip and warn unsupported features
    Warn,
}

#[derive(Clone, Debug, Default)]
pub enum ResourceInheritance {
    /// Add an `as-[base]` method to the resource, and add an as-[child] method to the base.
    #[default]
    AsMethods,
    /// Duplicate all the method from the base on the resource.
    DuplicateMethods,
    /// Combine solutions from `AsMethods` and `DuplicateMethods`.
    Both,
}

impl Default for ConversionOptions {
    fn default() -> Self {
        Self {
            package_name: wit_encoder::PackageName::new("webidl", "my-package-idl", None),
            interface_name: "my-interface".into(),
            unsupported_features: HandleUnsupported::default(),
            global_singletons: [
                "Window",
                "WorkerGlobalScope",
                "SharedWorkerGlobalScope",
                "ServiceWorkerGlobalScope",
                "DedicatedWorkerGlobalScope",
            ]
            .into_iter()
            .map(|x| x.into())
            .collect(),
            singleton_interface: None,
            phantom_interface: Vec::new(),
            phantom_dictionaries: Vec::new(),
            resource_inheritance: ResourceInheritance::default(),
        }
    }
}

pub(super) struct State<'a> {
    pub unsupported_features: HandleUnsupported,
    pub interface: wit_encoder::Interface,
    pub mixins: HashMap<String, Vec<weedle::interface::InterfaceMember<'a>>>,
    // Resource names do know what needs to be borrowed.
    pub resource_names: HashSet<Ident>,
    // Add these methods to the resource once they're found.
    // Used when partial interface is found before the main, or include is found before main declaration.
    pub waiting_resource_methods: HashMap<Ident, Vec<wit_encoder::ResourceFunc>>,
    // Mixin includes that were found before the mixin declaration.
    pub waiting_includes: HashMap<String, Vec<Ident>>,
    // TODO: don't treat any as special. Have a set for defined types instead.
    pub any_found: bool,
    pub resource_inheritance: ResourceInheritance,
    pub inheritors_waiting_for_base: HashMap<Ident, HashSet<Ident>>,
    // TODO: remove pollables in preview 3
    pub import_pollable: bool,
}

fn handle_unsupported(
    name: impl fmt::Display,
    feature: &str,
    handle_unsupported: &HandleUnsupported,
) {
    match handle_unsupported {
        HandleUnsupported::Bail => {
            todo!("WebIDL feature [{feature}] for item [{name}] is unsupported")
        }
        HandleUnsupported::Skip => {}
        HandleUnsupported::Warn => {
            eprintln!("WARN: Skipping {} as {} is unsupported", name, feature);
        }
    }
}

pub fn webidl_to_wit(
    mut webidl: WebIdlDefinitions,
    options: ConversionOptions,
) -> anyhow::Result<wit_encoder::Package> {
    let mut prepent = Vec::new();
    prepent.extend(options.phantom_interface.into_iter().map(|name| {
        // TODO: find a better way around lifetimes than leaking.
        let name = Box::leak(Box::new(name));
        Definition::Interface(weedle::InterfaceDefinition {
            attributes: None,
            interface: weedle::term::Interface,
            identifier: weedle::common::Identifier(name),
            inheritance: None,
            members: Default::default(),
            semi_colon: weedle::term::SemiColon,
        })
    }));
    prepent.extend(options.phantom_dictionaries.into_iter().map(|name| {
        // TODO: find a better way around lifetimes than leaking.
        let name = Box::leak(Box::new(name));
        Definition::Dictionary(weedle::DictionaryDefinition {
            attributes: None,
            dictionary: weedle::term::Dictionary,
            identifier: weedle::common::Identifier(name),
            inheritance: None,
            members: Default::default(),
            semi_colon: weedle::term::SemiColon,
        })
    }));
    webidl.splice(0..0, prepent);

    let mut package = wit_encoder::Package::new(options.package_name);

    // We generate a world or every global singleton.
    // These include Window, WorkerGlobalScope, SharedWorkerGlobalScope, ServiceWorkerGlobalScope,
    // DedicatedWorkerGlobalScope, whenever they are defined.
    let global_world_singletons = webidl
        .iter()
        .filter_map(|item| match item {
            Definition::Interface(wi_interface) => {
                if options
                    .global_singletons
                    .contains(wi_interface.identifier.0)
                {
                    Some(ident_name(wi_interface.identifier.0))
                } else {
                    None
                }
            }
            _ => None,
        })
        .collect::<Vec<Ident>>();

    let mut state = State {
        unsupported_features: options.unsupported_features,
        interface: Interface::new(options.interface_name.clone()),
        mixins: HashMap::new(),
        resource_names: webidl
            .iter()
            .filter_map(|item| match item {
                Definition::Interface(wi_interface) => {
                    if options
                        .singleton_interface
                        .as_ref()
                        .map(|singleton_iface| singleton_iface == wi_interface.identifier.0)
                        .unwrap_or(false)
                    {
                        None
                    } else {
                        Some(ident_name(wi_interface.identifier.0))
                    }
                }
                _ => None,
            })
            .collect(),
        waiting_resource_methods: HashMap::new(),
        waiting_includes: HashMap::new(),
        any_found: false,
        resource_inheritance: options.resource_inheritance,
        inheritors_waiting_for_base: HashMap::new(),
        import_pollable: false,
    };

    for item in webidl {
        match item {
            Definition::Callback(c) => {
                handle_unsupported(c.identifier.0, "callback", &state.unsupported_features);
                continue;
            }
            Definition::CallbackInterface(c) => {
                handle_unsupported(
                    c.identifier.0,
                    "callback interface",
                    &state.unsupported_features,
                );
                continue;
            }
            Definition::InterfaceMixin(mixin) => {
                let mixin_name = mixin.identifier.0.to_string();
                let members = mixin
                    .members
                    .body
                    .into_iter()
                    .map(|member| mixin_to_interface_member(member))
                    .collect_vec();
                state.mixins.insert(mixin_name.clone(), members.clone());
                if let Some(resources) = state.waiting_includes.remove(&mixin_name) {
                    for resource in resources {
                        state.interface_members_to_functions(&resource, &members.clone(), false)?;
                    }
                }
            }
            Definition::Namespace(namespace) => {
                let members = namespace
                    .members
                    .body
                    .into_iter()
                    .map(|member| namespace_to_interface_member(member))
                    .collect_vec();
                let is_singleton = options
                    .singleton_interface
                    .as_ref()
                    .map(|singleton_iface| singleton_iface == namespace.identifier.0)
                    .unwrap_or(false);
                let interface_name = ident_name(namespace.identifier.0);
                if !is_singleton {
                    let resource = wit_encoder::Resource::empty();
                    let type_def = wit_encoder::TypeDef::new(
                        interface_name.clone(),
                        wit_encoder::TypeDefKind::Resource(resource),
                    );
                    state.interface.type_def(type_def);
                }
                state.interface_members_to_functions(&interface_name, &members, is_singleton)?;
            }
            Definition::PartialInterface(wi_interface) => {
                let resource_name = ident_name(wi_interface.identifier.0);
                state.interface_members_to_functions(
                    &resource_name,
                    &wi_interface.members.body,
                    false,
                )?;
            }
            Definition::PartialInterfaceMixin(m) => {
                handle_unsupported(
                    m.identifier.0,
                    "partial interface mixin",
                    &state.unsupported_features,
                );
                continue;
            }
            Definition::PartialDictionary(d) => {
                handle_unsupported(
                    d.identifier.0,
                    "partial dictionary",
                    &state.unsupported_features,
                );
                continue;
            }
            Definition::PartialNamespace(n) => {
                handle_unsupported(
                    n.identifier.0,
                    "partial namespace",
                    &state.unsupported_features,
                );
                continue;
            }
            Definition::IncludesStatement(mixin) => {
                let mixin_name = mixin.rhs_identifier.0.to_string();
                let resource_name = ident_name(mixin.lhs_identifier.0);
                let mixin = state.mixins.get(&mixin_name).cloned();
                match mixin {
                    Some(mixin) => {
                        state.interface_members_to_functions(&resource_name, &mixin, false)?;
                    }
                    None => match state.waiting_includes.get_mut(&mixin_name) {
                        Some(v) => {
                            v.push(resource_name);
                        }
                        None => {
                            state
                                .waiting_includes
                                .insert(mixin_name, vec![resource_name]);
                        }
                    },
                }
            }
            Definition::Implements(i) => {
                handle_unsupported(
                    i.lhs_identifier.0,
                    "implements",
                    &state.unsupported_features,
                );
                continue;
            }
            Definition::Typedef(wi_type) => {
                let type_ = state.wi2w_type(&wi_type.type_.type_, false)?;
                let type_def = wit_encoder::TypeDef::type_(ident_name(wi_type.identifier.0), type_);
                state.interface.type_def(type_def);
            }
            Definition::Interface(wi_interface) => {
                let is_singleton = options
                    .singleton_interface
                    .as_ref()
                    .map(|singleton_iface| singleton_iface == wi_interface.identifier.0)
                    .unwrap_or(false);
                let interface_name = ident_name(wi_interface.identifier.0);
                if is_singleton {
                    if let Some(_inheritance) = wi_interface.inheritance {
                        handle_unsupported(
                            wi_interface.identifier.0,
                            "singleton inheritance",
                            &state.unsupported_features,
                        );
                    }
                } else {
                    let mut resource = wit_encoder::Resource::empty();
                    if let Some(inheritance) = wi_interface.inheritance {
                        let base_name = ident_name(inheritance.identifier.0);
                        let mut base =
                            state
                                .interface
                                .items_mut()
                                .iter_mut()
                                .find_map(|t| match t {
                                    InterfaceItem::TypeDef(type_def) => {
                                        let type_def_name = type_def.name().clone();
                                        match type_def.kind_mut() {
                                            wit_encoder::TypeDefKind::Resource(resource)
                                                if &type_def_name == &base_name =>
                                            {
                                                Some(resource)
                                            }
                                            _ => None,
                                        }
                                    }
                                    InterfaceItem::Function(_) => None,
                                });
                        if matches!(
                            state.resource_inheritance,
                            ResourceInheritance::AsMethods | ResourceInheritance::Both
                        ) {
                            resource.func({
                                let mut func = wit_encoder::ResourceFunc::method(format!(
                                    "as-{}",
                                    base_name.raw_name()
                                ));
                                func.set_results(wit_encoder::Type::named(base_name.clone()));
                                func
                            });
                            match &mut base {
                                Some(ref mut base) => {
                                    base.funcs_mut().push({
                                        let mut func = wit_encoder::ResourceFunc::method(format!(
                                            "as-{}",
                                            interface_name.raw_name()
                                        ));
                                        func.set_results(wit_encoder::Type::option(
                                            wit_encoder::Type::named(interface_name.clone()),
                                        ));
                                        func
                                    });
                                }
                                None => {
                                    state
                                        .inheritors_waiting_for_base
                                        .entry(base_name.clone())
                                        .or_insert(HashSet::new())
                                        .insert(interface_name.clone());
                                }
                            }
                        }
                        if matches!(
                            state.resource_inheritance,
                            ResourceInheritance::DuplicateMethods | ResourceInheritance::Both
                        ) {
                            match &mut base {
                                Some(ref mut base) => {
                                    let mut base_funcs = base
                                        .funcs()
                                        .iter()
                                        .filter(|f| match f.kind() {
                                            wit_encoder::ResourceFuncKind::Method(name, _) => {
                                                name.raw_name()
                                                    != format!("as-{}", interface_name.raw_name())
                                            }
                                            _ => true,
                                        })
                                        .cloned()
                                        .collect::<Vec<_>>();
                                    resource.funcs_mut().append(&mut base_funcs);
                                }
                                None => {
                                    state
                                        .inheritors_waiting_for_base
                                        .entry(base_name.clone())
                                        .or_insert(HashSet::new())
                                        .insert(interface_name.clone());
                                }
                            }
                        }
                    }
                    if let Some(inheritors) =
                        state.inheritors_waiting_for_base.remove(&interface_name)
                    {
                        for inheritor in inheritors {
                            state.interface_members_to_functions(
                                &inheritor,
                                &wi_interface.members.body,
                                is_singleton,
                            )?;
                            if matches!(
                                state.resource_inheritance,
                                ResourceInheritance::AsMethods | ResourceInheritance::Both
                            ) {
                                let as_child_method = {
                                    let mut func = wit_encoder::ResourceFunc::method(format!(
                                        "as-{}",
                                        inheritor.raw_name()
                                    ));
                                    func.set_results(wit_encoder::Type::option(
                                        wit_encoder::Type::named(inheritor.clone()),
                                    ));
                                    func
                                };
                                resource.func(as_child_method);
                            }
                        }
                    }
                    let type_def = wit_encoder::TypeDef::new(
                        interface_name.clone(),
                        wit_encoder::TypeDefKind::Resource(resource),
                    );
                    state.interface.type_def(type_def);
                }
                state.interface_members_to_functions(
                    &interface_name,
                    &wi_interface.members.body,
                    is_singleton,
                )?;
            }
            Definition::Dictionary(dict) => {
                let mut fields = dict
                    .members
                    .body
                    .iter()
                    .map(|mem| {
                        let name = ident_name(mem.identifier.0);
                        let ty = state.wi2w_type(&mem.type_, mem.required.is_none()).unwrap();
                        let ty = state.borrow_resources(ty);
                        (name, ty).into()
                    })
                    .collect::<Vec<_>>();

                let record_name = ident_name(dict.identifier.0);

                if let Some(inheritance) = dict.inheritance {
                    let base_name = ident_name(inheritance.identifier.0);
                    let base = state.interface.items().iter().find_map(|t| match t {
                        InterfaceItem::TypeDef(type_def) => match type_def.kind() {
                            wit_encoder::TypeDefKind::Record(record)
                                if type_def.name() == &base_name =>
                            {
                                Some(record)
                            }
                            _ => None,
                        },
                        InterfaceItem::Function(_) => None,
                    });
                    match base {
                        Some(base) => {
                            let mut base_fields = base.fields().to_vec();
                            fields.append(&mut base_fields);
                        }
                        None => {
                            state
                                .inheritors_waiting_for_base
                                .entry(base_name.clone())
                                .or_insert(HashSet::new())
                                .insert(record_name.clone());
                        }
                    }
                }

                if let Some(inheritors) = state.inheritors_waiting_for_base.remove(&record_name) {
                    for inheritor in inheritors {
                        let inheritor = state
                            .interface
                            .items_mut()
                            .iter_mut()
                            .find_map(|t| match t {
                                InterfaceItem::TypeDef(type_def) => {
                                    if type_def.name() == &inheritor {
                                        match type_def.kind_mut() {
                                            wit_encoder::TypeDefKind::Record(record) => {
                                                Some(record)
                                            }
                                            _ => panic!("Inheritor {} not a record", inheritor),
                                        }
                                    } else {
                                        None
                                    }
                                }
                                InterfaceItem::Function(_) => None,
                            })
                            .unwrap();

                        inheritor.fields_mut().append(&mut fields.clone())
                    }
                }

                let type_def = wit_encoder::TypeDef::record(record_name, fields);
                state.interface.type_def(type_def);
            }
            Definition::Enum(e) => {
                let cases = e.values.body.list.iter().map(|case| {
                    let name = if case.0.is_empty() { "default" } else { case.0 };
                    wit_encoder::EnumCase::new(ident_name(name))
                });
                let type_def = wit_encoder::TypeDef::enum_(ident_name(e.identifier.0), cases);
                state.interface.type_def(type_def);
            }
        }
    }

    for global_name in global_world_singletons {
        let mut func = StandaloneFunc::new(format!("get-{}", global_name.clone()));
        func.set_results(wit_encoder::Type::named(global_name.clone()));
        state.interface.function(func);
        let mut world = World::new(global_name.clone());
        world.named_interface_import(options.interface_name.clone());
        package.world(world);
    }

    if state.import_pollable {
        state.interface.use_type("wasi:io/poll", "pollable", None);
    }

    package.interface(state.interface);

    Ok(package)
}

impl<'a> State<'a> {
    fn function_args(
        &mut self,
        args: &weedle::argument::ArgumentList,
    ) -> anyhow::Result<wit_encoder::Params> {
        Ok(args
            .list
            .iter()
            .map(|arg| match arg {
                weedle::argument::Argument::Variadic(varg) => {
                    let name = ident_name(varg.identifier.0);
                    let type_ = self.wi2w_type(&varg.type_, false).unwrap();
                    let type_ = wit_encoder::Type::List(Box::new(type_));
                    let type_ = self.borrow_resources(type_);
                    (name, type_)
                }
                weedle::argument::Argument::Single(arg) => {
                    let name = ident_name(arg.identifier.0);
                    let optional = arg.optional.is_some();
                    let type_ = self.wi2w_type(&arg.type_.type_, optional).unwrap();
                    let type_ = self.borrow_resources(type_);
                    (name, type_)
                }
            })
            .collect())
    }

    fn interface_members_to_functions<'b>(
        &mut self,
        interface_name: &'b wit_encoder::Ident,
        members: impl IntoIterator<Item = &'b weedle::interface::InterfaceMember<'b>>,
        singleton_interface: bool,
    ) -> anyhow::Result<()> {
        enum Funcs {
            Standalone(LinkedHashMap<Ident, Vec<wit_encoder::StandaloneFunc>>),
            Resource(LinkedHashMap<Ident, Vec<wit_encoder::ResourceFunc>>),
        }
        let mut functions = match singleton_interface {
            true => Funcs::Standalone(LinkedHashMap::new()),
            false => Funcs::Resource(LinkedHashMap::new()),
        };
        for member in members {
            match member {
                weedle::interface::InterfaceMember::Const(const_) => {
                    fn const_type_to_wi_type(
                        const_type: weedle::types::ConstType,
                    ) -> weedle::types::Type {
                        use weedle::types::{ConstType, SingleType, Type};
                        match const_type {
                            ConstType::Integer(t) => Type::Single(SingleType::NonAny(t.into())),
                            ConstType::FloatingPoint(t) => {
                                Type::Single(SingleType::NonAny(t.into()))
                            }
                            ConstType::Boolean(t) => Type::Single(SingleType::NonAny(t.into())),
                            ConstType::Byte(t) => Type::Single(SingleType::NonAny(t.into())),
                            ConstType::Octet(t) => Type::Single(SingleType::NonAny(t.into())),
                            ConstType::Identifier(t) => Type::Single(SingleType::NonAny(t.into())),
                        }
                    }

                    let const_name = ident_name(const_.identifier.0).to_string().to_uppercase();
                    let wit_type = const_type_to_wi_type(const_.const_type.clone());
                    let type_ = self.wi2w_type(&wit_type, false)?;

                    match &mut functions {
                        Funcs::Standalone(functions) => {
                            let mut function = wit_encoder::StandaloneFunc::new(const_name.clone());
                            function.set_results(type_);
                            functions.insert(const_name.into(), vec![function]);
                        }
                        Funcs::Resource(functions) => {
                            let mut function =
                                wit_encoder::ResourceFunc::static_(const_name.clone());
                            function.set_results(type_);
                            functions.insert(const_name.into(), vec![function]);
                        }
                    }
                }
                weedle::interface::InterfaceMember::Iterable(_) => {
                    handle_unsupported(interface_name, "iterable", &self.unsupported_features);
                    continue;
                }
                weedle::interface::InterfaceMember::AsyncIterable(_) => {
                    handle_unsupported(
                        interface_name,
                        "async iterable",
                        &self.unsupported_features,
                    );
                    continue;
                }
                weedle::interface::InterfaceMember::Maplike(_) => {
                    handle_unsupported(interface_name, "maplike", &self.unsupported_features);
                    continue;
                }
                weedle::interface::InterfaceMember::Setlike(setlike) => match &mut functions {
                    Funcs::Standalone(functions) => {
                        let set_methods = self.interface_set_methods(setlike)?;
                        for (name, func) in set_methods {
                            functions.insert(name, vec![func]);
                        }
                    }
                    Funcs::Resource(functions) => {
                        let set_methods = self.resource_set_methods(setlike)?;
                        for (name, func) in set_methods {
                            functions.insert(name, vec![func]);
                        }
                    }
                },
                weedle::interface::InterfaceMember::Stringifier(_) => {
                    handle_unsupported(interface_name, "stringifier", &self.unsupported_features);
                    continue;
                }
                weedle::interface::InterfaceMember::Attribute(attr) => {
                    use weedle::types::{NonAnyType, ReturnType, SingleType, Type};
                    // TODO: remove the `is_undefined_promise` once we have proper Future support
                    let mut is_undefined_promise = false;
                    if let Type::Single(SingleType::NonAny(NonAnyType::Promise(promise))) =
                        &attr.type_.type_
                    {
                        if let ReturnType::Undefined(_) = &*promise.generics.body {
                            is_undefined_promise = true;
                        }
                    }
                    if is_undefined_promise {
                        // wit_encoder::Results::empty()
                        handle_unsupported(
                            attr.identifier.0,
                            "attribute Promise<undefined>",
                            &self.unsupported_features,
                        );
                        continue;
                    }

                    let mut attr_name = ident_name(attr.identifier.0);
                    let mut attr_type = self.wi2w_type(&attr.type_.type_, false)?;
                    let mut setter_name = attr
                        .readonly
                        .is_none()
                        .then(|| ident_name(&format!("set-{}", attr_name.raw_name())));
                    let is_static = matches!(
                        attr.modifier,
                        Some(weedle::interface::StringifierOrInheritOrStatic::Static(_))
                    );

                    if matches!(&attr_type, wit_encoder::Type::Named(name) if name.raw_name() == "event-handler")
                    {
                        if !attr_name.raw_name().starts_with("on") {
                            panic!("EventHandler without `on` prefix");
                        }
                        self.import_pollable = true;
                        attr_type = wit_encoder::Type::named("pollable");
                        attr_name = ident_name(&format!("{attr_name}-subscribe"));
                        setter_name = None;
                    }

                    match &mut functions {
                        Funcs::Standalone(functions) => {
                            if is_static {
                                bail!(
                                    "Static functions unsupported for singleton interface {}",
                                    interface_name
                                );
                            }
                            let mut getter = wit_encoder::StandaloneFunc::new(attr_name.clone());
                            getter.set_results(attr_type.clone());
                            functions.insert(attr_name.clone(), vec![getter]);
                        }
                        Funcs::Resource(functions) => {
                            let mut getter = match attr.modifier {
                                Some(weedle::interface::StringifierOrInheritOrStatic::Static(
                                    _,
                                )) => wit_encoder::ResourceFunc::static_(attr_name.clone()),
                                _ => wit_encoder::ResourceFunc::method(attr_name.clone()),
                            };
                            getter.set_results(attr_type.clone());
                            functions.insert(attr_name.clone(), vec![getter]);
                        }
                    }

                    if let Some(setter_name) = setter_name {
                        match &mut functions {
                            Funcs::Standalone(functions) => {
                                if is_static {
                                    unreachable!()
                                }
                                let mut setter =
                                    wit_encoder::StandaloneFunc::new(setter_name.clone());
                                setter.set_params((attr_name, attr_type));
                                functions.insert(setter_name, vec![setter]);
                            }
                            Funcs::Resource(functions) => {
                                let mut setter = match attr.modifier {
                                    Some(
                                        weedle::interface::StringifierOrInheritOrStatic::Static(_),
                                    ) => wit_encoder::ResourceFunc::static_(setter_name.clone()),
                                    _ => wit_encoder::ResourceFunc::method(setter_name.clone()),
                                };
                                setter.set_params((attr_name, attr_type));
                                functions.insert(setter_name, vec![setter]);
                            }
                        }
                    }
                }
                weedle::interface::InterfaceMember::Constructor(constructor) => {
                    match &mut functions {
                        Funcs::Standalone(_) => {
                            bail!(
                                "Cannot create a singleton interface for {} with a constructor",
                                interface_name
                            );
                        }
                        Funcs::Resource(functions) => {
                            let mut function = wit_encoder::ResourceFunc::constructor();
                            function.set_params(self.function_args(&constructor.args.body)?);
                            functions
                                .entry(Ident::new("constructor"))
                                .or_insert(vec![])
                                .push(function);
                        }
                    }
                }
                weedle::interface::InterfaceMember::Operation(operation) => {
                    if let Some(identifier) = operation.identifier {
                        let function_name = ident_name(identifier.0);

                        let results = match &operation.return_type {
                            weedle::types::ReturnType::Undefined(_) => {
                                wit_encoder::Results::empty()
                            }
                            weedle::types::ReturnType::Type(type_) => {
                                use weedle::types::{NonAnyType, ReturnType, SingleType, Type};
                                // TODO: remove the `is_undefined_promise` once we have proper Future support
                                let mut is_undefined_promise = false;
                                if let Type::Single(SingleType::NonAny(NonAnyType::Promise(
                                    promise,
                                ))) = type_
                                {
                                    if let ReturnType::Undefined(_) = &*promise.generics.body {
                                        is_undefined_promise = true;
                                    }
                                }
                                if is_undefined_promise {
                                    wit_encoder::Results::empty()
                                } else {
                                    self.wi2w_type(&type_, false)?.into()
                                }
                            }
                        };
                        let params = self.function_args(&operation.args.body)?;

                        // TODO: if operation.attributes includes `[Throws]`, make return a result.
                        match &mut functions {
                            Funcs::Standalone(functions) => {
                                let mut function = match operation.modifier {
                                    Some(weedle::interface::StringifierOrStatic::Static(_)) => {
                                        bail!(
                                            "Static functions unsupported for singleton interface {}",
                                            interface_name
                                        )
                                    }
                                    _ => wit_encoder::StandaloneFunc::new(function_name.clone()),
                                };
                                function.set_params(params);
                                function.set_results(results);
                                functions
                                    .entry(function_name)
                                    .or_insert(vec![])
                                    .push(function);
                            }
                            Funcs::Resource(functions) => {
                                let mut function = match operation.modifier {
                                    Some(weedle::interface::StringifierOrStatic::Static(_)) => {
                                        wit_encoder::ResourceFunc::static_(function_name.clone())
                                    }
                                    _ => wit_encoder::ResourceFunc::method(function_name.clone()),
                                };
                                function.set_params(params);
                                function.set_results(results);
                                functions
                                    .entry(function_name)
                                    .or_insert(vec![])
                                    .push(function);
                            }
                        }
                    } else {
                        eprintln!(
                            "WARN: Skipping operation without identifier. Getter/setter/deleter not yet supported",
                        );
                        continue;
                    }
                }
            }
        }

        match functions {
            Funcs::Standalone(mut functions) => {
                for (func_name, functions) in &mut functions {
                    if functions.len() > 1 {
                        let variant_name = Ident::new(format!(
                            "{}-{}-params",
                            interface_name.raw_name(),
                            func_name.raw_name()
                        ));
                        let results = functions[0].results();
                        let same_results = functions.iter().all(|f| f.results() == results);
                        if !same_results {
                            handle_unsupported(
                                func_name,
                                "different results for overloading",
                                &self.unsupported_features,
                            );
                            functions.clear();
                            continue;
                        }
                        let optional = self
                            .interface_functions_params_to_variant(variant_name.clone(), functions);
                        functions.drain(1..);
                        let params = match optional {
                            true => {
                                wit_encoder::Type::option(wit_encoder::Type::named(variant_name))
                            }
                            false => wit_encoder::Type::named(variant_name),
                        };
                        functions[0].set_params(("params", params));
                    }
                }
                let items = functions
                    .into_iter()
                    .map(|(_, functions)| functions.into_iter().map(|f| InterfaceItem::Function(f)))
                    .flatten();
                self.interface.items_mut().extend(items);
            }
            Funcs::Resource(mut functions) => {
                for (func_name, functions) in &mut functions {
                    if functions.len() > 1 {
                        let variant_name = Ident::new(format!(
                            "{}-{}-params",
                            interface_name.raw_name(),
                            func_name.raw_name()
                        ));
                        let results = match functions[0].kind() {
                            wit_encoder::ResourceFuncKind::Method(_, results) => Some(results),
                            wit_encoder::ResourceFuncKind::Static(_, results) => Some(results),
                            wit_encoder::ResourceFuncKind::Constructor => None,
                        };
                        if let Some(results) = results {
                            let same_results = functions.iter().all(|f| match f.kind() {
                                wit_encoder::ResourceFuncKind::Method(_, r) => r == results,
                                wit_encoder::ResourceFuncKind::Static(_, r) => r == results,
                                wit_encoder::ResourceFuncKind::Constructor => true,
                            });
                            if !same_results {
                                handle_unsupported(
                                    func_name,
                                    "different results for overloading",
                                    &self.unsupported_features,
                                );
                                functions.clear();
                                continue;
                            }
                        }
                        let optional = self
                            .resource_functions_params_to_variant(variant_name.clone(), functions);
                        functions.drain(1..);
                        let params = match optional {
                            true => {
                                wit_encoder::Type::option(wit_encoder::Type::named(variant_name))
                            }
                            false => wit_encoder::Type::named(variant_name),
                        };
                        functions[0].set_params(("params", params));
                    }
                }
                let resource = self
                    .interface
                    .items_mut()
                    .iter_mut()
                    .filter_map(|td| match td {
                        wit_encoder::InterfaceItem::TypeDef(td) => Some(td),
                        wit_encoder::InterfaceItem::Function(_) => None,
                    })
                    .find(|td| td.name() == interface_name);

                match resource {
                    None => {
                        self.waiting_resource_methods.insert(
                            interface_name.clone(),
                            functions.into_iter().map(|(_, f)| f).flatten().collect(),
                        );
                    }
                    Some(resource) => {
                        let resource = match resource.kind_mut() {
                            wit_encoder::TypeDefKind::Resource(resource) => resource,
                            _ => panic!("Not a resource"),
                        };
                        resource.funcs_mut().extend(
                            functions
                                .into_iter()
                                .map(|(_, functions)| functions.into_iter().map(|f| f))
                                .flatten(),
                        );
                        if let Some(functions) =
                            self.waiting_resource_methods.remove(&interface_name)
                        {
                            resource.funcs_mut().extend(functions);
                        }
                    }
                };
            }
        }

        Ok(())
    }

    fn params_to_tuple(&self, params: &mut wit_encoder::Params) -> (wit_encoder::Type, String) {
        if params.items().len() == 1 {
            if let Some((name, type_)) = params.items_mut().drain(..).next() {
                return (type_, name.to_string());
            }
        }

        let mut tuple = wit_encoder::Tuple::empty();
        let name = params
            .items()
            .iter()
            .map(|(name, _)| name.raw_name())
            .join("-");
        tuple
            .types_mut()
            .extend(params.items_mut().drain(..).map(|(_, type_)| type_));
        (wit_encoder::Type::Tuple(tuple), name)
    }

    fn resource_functions_params_to_variant(
        &mut self,
        variant_name: Ident,
        functions: &mut Vec<wit_encoder::ResourceFunc>,
    ) -> bool {
        let mut has_empty = false;
        if let Some(pos) = functions.iter().position(|f| f.params().items().is_empty()) {
            functions.remove(pos);
            has_empty = true;
        }
        let cases = functions
            .into_iter()
            .map(|f| self.params_to_tuple(f.params_mut()))
            .map(|(tuple, name)| wit_encoder::VariantCase::value(name, tuple));

        let variant = wit_encoder::TypeDef::variant(variant_name, cases);
        self.interface.type_def(variant);
        has_empty
    }

    fn interface_functions_params_to_variant(
        &mut self,
        variant_name: Ident,
        functions: &mut Vec<StandaloneFunc>,
    ) -> bool {
        let mut has_empty = false;
        if let Some(pos) = functions.iter().position(|f| f.params().items().is_empty()) {
            functions.remove(pos);
            has_empty = true;
        }
        let cases = functions
            .into_iter()
            .map(|f| self.params_to_tuple(f.params_mut()))
            .map(|(tuple, name)| wit_encoder::VariantCase::value(name, tuple));

        let variant = wit_encoder::TypeDef::variant(variant_name, cases);
        self.interface.type_def(variant);
        has_empty
    }

    fn resource_set_methods<'b>(
        &mut self,
        setlike: &weedle::interface::SetlikeInterfaceMember<'b>,
    ) -> anyhow::Result<LinkedHashMap<Ident, wit_encoder::ResourceFunc>> {
        let generic_type = self.wi2w_type(&setlike.generics.body.type_, false)?;
        assert!(
            setlike.readonly.is_some(),
            "TODO: add mutable setlike support"
        );
        let mut functions = LinkedHashMap::new();
        {
            let name = ident_name("has");
            functions.insert(name.clone(), {
                let mut func = wit_encoder::ResourceFunc::method(name);
                func.set_params(("value", generic_type));
                func.set_results(wit_encoder::Type::Bool);
                func
            });
        }
        Ok(functions)
    }
    fn interface_set_methods<'b>(
        &mut self,
        setlike: &weedle::interface::SetlikeInterfaceMember<'b>,
    ) -> anyhow::Result<LinkedHashMap<Ident, wit_encoder::StandaloneFunc>> {
        let generic_type = self.wi2w_type(&setlike.generics.body.type_, false)?;
        assert!(
            setlike.readonly.is_some(),
            "TODO: add mutable setlike support"
        );
        let mut functions = LinkedHashMap::new();
        {
            let name = ident_name("has");
            functions.insert(name.clone(), {
                let mut func = wit_encoder::StandaloneFunc::new(name);
                func.set_params(("value", generic_type));
                func.set_results(wit_encoder::Type::Bool);
                func
            });
        }
        Ok(functions)
    }
}

pub(super) fn ident_name(src: &str) -> wit_encoder::Ident {
    // doing to_pascal_case first to get rid of all dashes. E.g. "A-1" should turn into "a1" and not "a-1".
    let mut name = src.to_pascal_case().to_kebab_case();
    if matches!(name.chars().next(), Some(c) if c.is_digit(10)) {
        name = format!("x{name}");
    }
    wit_encoder::Ident::new(name)
}

fn namespace_to_interface_member(
    mixin_member: weedle::namespace::NamespaceMember,
) -> weedle::interface::InterfaceMember {
    match mixin_member {
        weedle::namespace::NamespaceMember::Const(const_) => {
            weedle::interface::InterfaceMember::Const(weedle::interface::ConstMember {
                attributes: const_.attributes,
                const_: const_.const_,
                const_type: const_.const_type,
                identifier: const_.identifier,
                assign: weedle::term::Assign,
                const_value: const_.const_value,
                semi_colon: weedle::term::SemiColon,
            })
        }
        weedle::namespace::NamespaceMember::Operation(operation) => {
            weedle::interface::InterfaceMember::Operation(
                weedle::interface::OperationInterfaceMember {
                    attributes: operation.attributes,
                    modifier: None,
                    special: None,
                    return_type: operation.return_type,
                    identifier: operation.identifier,
                    args: operation.args,
                    semi_colon: operation.semi_colon,
                },
            )
        }
        weedle::namespace::NamespaceMember::Attribute(attribute) => {
            weedle::interface::InterfaceMember::Attribute(
                weedle::interface::AttributeInterfaceMember {
                    attributes: attribute.attributes,
                    modifier: None,
                    readonly: Some(weedle::term::ReadOnly),
                    attribute: attribute.attribute,
                    type_: attribute.type_,
                    identifier: attribute.identifier,
                    semi_colon: attribute.semi_colon,
                },
            )
        }
    }
}

fn mixin_to_interface_member(
    mixin_member: weedle::mixin::MixinMember,
) -> weedle::interface::InterfaceMember {
    match mixin_member {
        weedle::mixin::MixinMember::Const(const_) => {
            weedle::interface::InterfaceMember::Const(const_)
        }
        weedle::mixin::MixinMember::Operation(operation) => {
            weedle::interface::InterfaceMember::Operation(
                weedle::interface::OperationInterfaceMember {
                    attributes: operation.attributes,
                    modifier: operation.stringifier.map(|stringifier| {
                        weedle::interface::StringifierOrStatic::Stringifier(stringifier)
                    }),
                    special: None,
                    return_type: operation.return_type,
                    identifier: operation.identifier,
                    args: operation.args,
                    semi_colon: operation.semi_colon,
                },
            )
        }
        weedle::mixin::MixinMember::Attribute(attribute) => {
            weedle::interface::InterfaceMember::Attribute(
                weedle::interface::AttributeInterfaceMember {
                    attributes: attribute.attributes,
                    modifier: attribute.stringifier.map(|stringifier| {
                        weedle::interface::StringifierOrInheritOrStatic::Stringifier(stringifier)
                    }),
                    readonly: attribute.readonly,
                    attribute: attribute.attribute,
                    type_: attribute.type_,
                    identifier: attribute.identifier,
                    semi_colon: attribute.semi_colon,
                },
            )
        }
        weedle::mixin::MixinMember::Stringifier(stringifier) => {
            weedle::interface::InterfaceMember::Stringifier(stringifier)
        }
    }
}
