use anyhow::bail;
use heck::{ToKebabCase, ToPascalCase};
use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    fmt,
};
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
        }
    }
}
pub(super) struct State<'a> {
    pub unsupported_features: HandleUnsupported,
    pub interface: wit_encoder::Interface,
    pub mixins: HashMap<String, Vec<weedle::interface::InterfaceMember<'a>>>,
    // Resource names do know what needs to be borrowed.
    pub resource_names: HashSet<Ident>,
    // Add these methods to the resource one you find it.
    // Used when partial interface is found before the main, or include is found before main declaration.
    pub waiting_resource_method: HashMap<Ident, Vec<wit_encoder::ResourceFunc>>,
    // Mixin includes that were found before the mixin declaration.
    pub waiting_includes: HashMap<String, Vec<Ident>>,
    pub any_found: bool,
}

fn handle_unsupported(
    name: impl fmt::Display,
    feature: &str,
    handle_unsupported: &HandleUnsupported,
) {
    match handle_unsupported {
        HandleUnsupported::Bail => todo!("{feature} for {name}"),
        HandleUnsupported::Skip => {}
        HandleUnsupported::Warn => {
            eprintln!("WARN: Skipping {} as {} is unsupported", name, feature);
        }
    }
}

pub fn webidl_to_wit(
    webidl: WebIdlDefinitions,
    options: ConversionOptions,
) -> anyhow::Result<wit_encoder::Package> {
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
        waiting_resource_method: HashMap::new(),
        waiting_includes: HashMap::new(),
        any_found: false,
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
            Definition::Namespace(ns) => {
                handle_unsupported(
                    ns.identifier.0,
                    "callback interface",
                    &state.unsupported_features,
                );
                continue;
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
                if !is_singleton {
                    let resource = wit_encoder::Resource::empty();
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
                let fields = dict.members.body.iter().map(|mem| {
                    let name = ident_name(mem.identifier.0);
                    let ty = state.wi2w_type(&mem.type_, mem.required.is_none()).unwrap();
                    let ty = state.borrow_resources(ty);
                    (name, ty)
                });
                let type_def = wit_encoder::TypeDef::record(ident_name(dict.identifier.0), fields);
                state.interface.type_def(type_def);
            }
            Definition::Enum(e) => {
                let cases = e
                    .values
                    .body
                    .list
                    .iter()
                    .map(|case| wit_encoder::EnumCase::new(ident_name(case.0)));
                let type_def = wit_encoder::TypeDef::enum_(ident_name(e.identifier.0), cases);
                state.interface.type_def(type_def);
            }
        }
    }

    for global_name in global_world_singletons {
        let mut func = StandaloneFunc::new(format!("get-{}", global_name.clone()));
        func.results(wit_encoder::Type::named(global_name.clone()));
        state.interface.function(func);
        let mut world = World::new(global_name.clone());
        world.named_interface_import(options.interface_name.clone());
        package.world(world);
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
}

pub(super) fn ident_name(src: &str) -> wit_encoder::Ident {
    // doing to_pascal_case first to get rid of all dashes. E.g. "A-1" should turn into "a1" and not "a-1".
    let mut name = src.to_pascal_case().to_kebab_case();
    if matches!(name.chars().next(), Some(c) if c.is_digit(10)) {
        name = format!("x{name}");
    }
    wit_encoder::Ident::new(name)
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

impl<'a> State<'a> {
    fn interface_members_to_functions<'b>(
        &mut self,
        interface_name: &'b wit_encoder::Ident,
        members: impl IntoIterator<Item = &'b weedle::interface::InterfaceMember<'b>>,
        singleton_interface: bool,
    ) -> anyhow::Result<()> {
        let mut iface_items = Vec::new();
        let mut resource_functions = Vec::new();
        for member in members {
            match member {
                weedle::interface::InterfaceMember::Const(_) => {
                    handle_unsupported(
                        interface_name,
                        "partial namespace",
                        &self.unsupported_features,
                    );
                    continue;
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
                weedle::interface::InterfaceMember::Setlike(setlike) => {
                    if singleton_interface {
                        let mut set_methods = self.interface_set_methods(setlike)?;
                        let mut set_methods = set_methods
                            .drain(..)
                            .map(|func| InterfaceItem::Function(func))
                            .collect::<Vec<InterfaceItem>>();
                        iface_items.append(&mut set_methods);
                    } else {
                        let mut set_methods = self.resource_set_methods(setlike)?;
                        resource_functions.append(&mut set_methods);
                    }
                }
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

                    let attr_name = ident_name(attr.identifier.0).to_string();
                    let setter_name = format!(
                        "set-{}",
                        if attr_name.starts_with('%') {
                            &attr_name[1..]
                        } else {
                            &attr_name
                        }
                    );
                    let attr_type = self.wi2w_type(&attr.type_.type_, false)?;

                    if singleton_interface {
                        let mut getter = match attr.modifier {
                            Some(weedle::interface::StringifierOrInheritOrStatic::Static(_)) => {
                                bail!(
                                    "Static functions unsupported for singleton interface {}",
                                    interface_name
                                );
                            }
                            _ => wit_encoder::StandaloneFunc::new(attr_name.clone()),
                        };
                        getter.results(attr_type.clone());
                        iface_items.push(InterfaceItem::Function(getter));
                    } else {
                        let mut getter = match attr.modifier {
                            Some(weedle::interface::StringifierOrInheritOrStatic::Static(_)) => {
                                wit_encoder::ResourceFunc::static_(attr_name.clone())
                            }
                            _ => wit_encoder::ResourceFunc::method(attr_name.clone()),
                        };
                        getter.results(attr_type.clone());
                        resource_functions.push(getter);
                    }
                    if attr.readonly.is_none() {
                        if singleton_interface {
                            let mut setter = match attr.modifier {
                                Some(weedle::interface::StringifierOrInheritOrStatic::Static(
                                    _,
                                )) => unreachable!(),
                                _ => wit_encoder::StandaloneFunc::new(setter_name),
                            };
                            setter.params((attr_name, attr_type));
                            iface_items.push(InterfaceItem::Function(setter));
                        } else {
                            let mut setter = match attr.modifier {
                                Some(weedle::interface::StringifierOrInheritOrStatic::Static(
                                    _,
                                )) => wit_encoder::ResourceFunc::static_(setter_name),
                                _ => wit_encoder::ResourceFunc::method(setter_name),
                            };
                            setter.params((attr_name, attr_type));
                            resource_functions.push(setter);
                        }
                    }
                }
                weedle::interface::InterfaceMember::Constructor(constructor) => {
                    if singleton_interface {
                        bail!(
                            "Cannot create a singleton interface for {} with a constructor",
                            interface_name
                        );
                    }
                    let mut function = wit_encoder::ResourceFunc::constructor();
                    function.params(self.function_args(&constructor.args.body)?);
                    resource_functions.push(function);
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

                        if singleton_interface {
                            let mut function = match operation.modifier {
                                Some(weedle::interface::StringifierOrStatic::Static(_)) => {
                                    bail!(
                                        "Static functions unsupported for singleton interface {}",
                                        interface_name
                                    )
                                }
                                _ => wit_encoder::StandaloneFunc::new(function_name),
                            };
                            function.params(self.function_args(&operation.args.body)?);
                            function.results(results);
                            iface_items.push(InterfaceItem::Function(function));
                        } else {
                            // TODO: if operation.attributes includes `[Throws]`, make return a result.
                            let mut function = match operation.modifier {
                                Some(weedle::interface::StringifierOrStatic::Static(_)) => {
                                    wit_encoder::ResourceFunc::static_(function_name)
                                }
                                _ => wit_encoder::ResourceFunc::method(function_name),
                            };
                            function.params(self.function_args(&operation.args.body)?);
                            function.results(results);
                            resource_functions.push(function);
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

        if singleton_interface {
            self.interface.items_mut().extend(iface_items);
        } else {
            let resource = self
                .interface
                .items_mut()
                .iter_mut()
                .filter_map(|td| match td {
                    wit_encoder::InterfaceItem::TypeDef(td) => Some(td),
                    wit_encoder::InterfaceItem::Use(_)
                    | wit_encoder::InterfaceItem::Function(_) => None,
                })
                .find(|td| td.name() == interface_name);

            match resource {
                None => {
                    self.waiting_resource_method
                        .insert(interface_name.clone(), resource_functions);
                }
                Some(resource) => {
                    let resource = match resource.kind_mut() {
                        wit_encoder::TypeDefKind::Resource(resource) => resource,
                        _ => panic!("Not a resource"),
                    };
                    resource.funcs_mut().extend(resource_functions);
                    if let Some(functions) = self.waiting_resource_method.remove(&interface_name) {
                        resource.funcs_mut().extend(functions);
                    }
                }
            };
        }

        Ok(())
    }

    fn resource_set_methods<'b>(
        &mut self,
        setlike: &weedle::interface::SetlikeInterfaceMember<'b>,
    ) -> anyhow::Result<Vec<wit_encoder::ResourceFunc>> {
        let generic_type = self.wi2w_type(&setlike.generics.body.type_, false)?;
        assert!(
            setlike.readonly.is_some(),
            "TODO: add mutable setlike support"
        );
        Ok(vec![{
            let mut func = wit_encoder::ResourceFunc::method("has");
            func.params(("value", generic_type));
            func.results(wit_encoder::Type::Bool);
            func
        }])
    }
    fn interface_set_methods<'b>(
        &mut self,
        setlike: &weedle::interface::SetlikeInterfaceMember<'b>,
    ) -> anyhow::Result<Vec<wit_encoder::StandaloneFunc>> {
        let generic_type = self.wi2w_type(&setlike.generics.body.type_, false)?;
        assert!(
            setlike.readonly.is_some(),
            "TODO: add mutable setlike support"
        );
        Ok(vec![{
            let mut func = wit_encoder::StandaloneFunc::new("has");
            func.params(("value", generic_type));
            func.results(wit_encoder::Type::Bool);
            func
        }])
    }
}
