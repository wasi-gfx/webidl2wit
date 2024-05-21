use std::collections::HashMap;

use heck::{ToKebabCase, ToPascalCase};
use itertools::Itertools;
use weedle::{Definition, Definitions as WebIdlDefinitions};

use crate::translations::types_::wi2w_type;

mod types_;

/// conversion options.
#[derive(Clone, Debug)]
pub struct ConversionOptions {
    /// Name of package for generated wit.
    pub package_name: wit_encoder::PackageName,
    /// interface to hold the generated types and functions.
    pub interface: wit_encoder::Interface,
}
impl Default for ConversionOptions {
    fn default() -> Self {
        Self {
            package_name: wit_encoder::PackageName::new("my-namespace", "my-package", None),
            interface: wit_encoder::Interface::new(Some("my-interface")),
        }
    }
}

pub fn webidl_to_wit(
    webidl: WebIdlDefinitions,
    options: ConversionOptions,
) -> anyhow::Result<wit_encoder::Package> {
    let mut package = wit_encoder::Package::new(options.package_name);
    let mut interface = options.interface;
    let mut mixins: HashMap<String, Vec<weedle::interface::InterfaceMember<'_>>> = HashMap::new();

    for item in webidl {
        match item {
            Definition::Callback(_) => todo!(),
            Definition::CallbackInterface(_) => todo!(),
            Definition::InterfaceMixin(mixin) => {
                let members = mixin
                    .members
                    .body
                    .into_iter()
                    .map(|member| mixin_to_interface_member(member))
                    .collect_vec();
                mixins.insert(mixin.identifier.0.to_string(), members);
            }
            Definition::Namespace(_) => todo!(),
            Definition::PartialInterface(wi_interface) => {
                let resource_name = ident_name(wi_interface.identifier.0);
                interface_members_to_functions(
                    &mut interface,
                    &resource_name,
                    &wi_interface.members.body,
                )?;
            }
            Definition::PartialInterfaceMixin(_) => todo!(),
            Definition::PartialDictionary(_) => todo!(),
            Definition::PartialNamespace(_) => todo!(),
            Definition::IncludesStatement(mixin) => {
                let mixin_name = mixin.rhs_identifier.0.to_string();
                let resource_name = ident_name(mixin.lhs_identifier.0);
                let mixin = mixins.get(&mixin_name).unwrap();
                interface_members_to_functions(&mut interface, &resource_name, mixin)?;
            }
            Definition::Implements(_) => todo!(),
            Definition::Typedef(wi_type) => {
                let type_ = wi2w_type(&mut interface, &wi_type.type_.type_, false)?;
                let type_def = wit_encoder::TypeDef::type_(ident_name(wi_type.identifier.0), type_);
                interface.type_def(type_def);
            }
            Definition::Interface(wi_interface) => {
                let resource = wit_encoder::Resource::empty();
                let resource_name = ident_name(wi_interface.identifier.0);
                let type_def = wit_encoder::TypeDef::new(
                    resource_name.clone(),
                    wit_encoder::TypeDefKind::Resource(resource),
                );
                interface.type_def(type_def);
                interface_members_to_functions(
                    &mut interface,
                    &resource_name,
                    &wi_interface.members.body,
                )?;
            }
            Definition::Dictionary(dict) => {
                let fields = dict.members.body.iter().map(|mem| {
                    let name = ident_name(mem.identifier.0);
                    let ty = wi2w_type(&mut interface, &mem.type_, mem.required.is_none()).unwrap();
                    (name, ty)
                });
                let type_def = wit_encoder::TypeDef::record(ident_name(dict.identifier.0), fields);
                interface.type_def(type_def);
            }
            Definition::Enum(e) => {
                let cases = e
                    .values
                    .body
                    .list
                    .iter()
                    .map(|case| wit_encoder::EnumCase::new(ident_name(case.0)));
                let type_def = wit_encoder::TypeDef::enum_(ident_name(e.identifier.0), cases);
                interface.type_def(type_def);
            }
        }
    }

    package.interface(interface);

    Ok(package)
}

fn function_args(
    interface: &mut wit_encoder::Interface,
    args: &weedle::argument::ArgumentList,
) -> anyhow::Result<wit_encoder::Params> {
    Ok(args
        .list
        .iter()
        .map(|arg| match arg {
            weedle::argument::Argument::Variadic(_) => todo!(),
            weedle::argument::Argument::Single(arg) => {
                let name = ident_name(arg.identifier.0);
                let optional = arg.optional.is_some();
                let type_ = wi2w_type(interface, &arg.type_.type_, optional).unwrap();
                (name, type_)
            }
        })
        .collect())
}

pub(super) fn ident_name(src: &str) -> wit_encoder::Ident {
    // doing to_pascal_case first to get rid of all dashes. E.g. "A-1" should turn into "a1" and not "a-1".
    let name = src.to_pascal_case().to_kebab_case();
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

fn interface_members_to_functions<'a>(
    interface: &mut wit_encoder::Interface,
    resource_name: &wit_encoder::Ident,
    members: impl IntoIterator<Item = &'a weedle::interface::InterfaceMember<'a>>,
) -> anyhow::Result<()> {
    let mut functions = Vec::new();
    for member in members {
        match member {
            weedle::interface::InterfaceMember::Const(_) => todo!(),
            weedle::interface::InterfaceMember::Iterable(_) => todo!(),
            weedle::interface::InterfaceMember::AsyncIterable(_) => todo!(),
            weedle::interface::InterfaceMember::Maplike(_) => todo!(),
            weedle::interface::InterfaceMember::Setlike(_) => todo!(),
            weedle::interface::InterfaceMember::Stringifier(_) => todo!(),
            weedle::interface::InterfaceMember::Attribute(attr) => {
                let attr_name = ident_name(attr.identifier.0);
                let setter_name = format!("set-{attr_name}");
                let attr_type = wi2w_type(interface, &attr.type_.type_, false)?;

                let mut getter = match attr.modifier {
                    Some(weedle::interface::StringifierOrInheritOrStatic::Static(_)) => {
                        wit_encoder::ResourceFunc::static_(attr_name.clone())
                    }
                    _ => wit_encoder::ResourceFunc::method(attr_name.clone()),
                };
                getter.results(attr_type.clone());
                functions.push(getter);
                if attr.readonly.is_none() {
                    let mut setter = match attr.modifier {
                        Some(weedle::interface::StringifierOrInheritOrStatic::Static(_)) => {
                            wit_encoder::ResourceFunc::static_(setter_name)
                        }
                        _ => wit_encoder::ResourceFunc::method(setter_name),
                    };
                    setter.params((attr_name, attr_type));
                    functions.push(setter);
                }
            }
            weedle::interface::InterfaceMember::Constructor(constructor) => {
                let mut function = wit_encoder::ResourceFunc::constructor();
                function.params(function_args(interface, &constructor.args.body)?);
                functions.push(function);
            }
            weedle::interface::InterfaceMember::Operation(operation) => {
                let function_name = ident_name(operation.identifier.unwrap().0);
                let mut function = match operation.modifier {
                    Some(weedle::interface::StringifierOrStatic::Static(_)) => {
                        wit_encoder::ResourceFunc::static_(function_name)
                    }
                    _ => wit_encoder::ResourceFunc::method(function_name),
                };

                function.params(function_args(interface, &operation.args.body)?);

                let results = match &operation.return_type {
                    weedle::types::ReturnType::Undefined(_) => wit_encoder::Results::empty(),
                    weedle::types::ReturnType::Type(type_) => {
                        wi2w_type(interface, &type_, false)?.into()
                    }
                };
                function.results(results);
                functions.push(function);
            }
        }
    }

    let resource = interface
        .type_defs_mut()
        .iter_mut()
        .find(|td| td.name() == resource_name)
        .expect("Resource not found");
    let resource = match resource.kind_mut() {
        wit_encoder::TypeDefKind::Resource(resource) => resource,
        _ => panic!("Not a resource"),
    };

    resource.funcs_mut().extend(functions);

    Ok(())
}
