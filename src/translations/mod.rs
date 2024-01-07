use heck::{ToKebabCase, ToPascalCase};
use itertools::Itertools;
use weedle::{Definition, Definitions as WebIdlDefinitions};
use wit_parser::Resolve;

use crate::translations::types_::wi2w_type;

mod types_;

pub fn webidl_to_wit(webidl: WebIdlDefinitions) -> anyhow::Result<Resolve> {
    let mut resolve: Resolve = Default::default();
    let interface = wit_parser::Interface {
        name: Some("main_interface".to_string()),
        types: Default::default(),
        functions: Default::default(),
        docs: Default::default(),
        package: Default::default(),
    };
    let interface_id = resolve.interfaces.alloc(interface);

    for item in webidl {
        match item {
            Definition::Callback(_) => todo!(),
            Definition::CallbackInterface(_) => todo!(),
            Definition::InterfaceMixin(_) => todo!(),
            Definition::Namespace(_) => todo!(),
            Definition::PartialInterface(_) => todo!(),
            Definition::PartialInterfaceMixin(_) => todo!(),
            Definition::PartialDictionary(_) => todo!(),
            Definition::PartialNamespace(_) => todo!(),
            Definition::IncludesStatement(_) => todo!(),
            Definition::Implements(_) => todo!(),
            Definition::Typedef(wi_type) => {
                let wit_type = wi2w_type(&mut resolve, &wi_type.type_.type_).unwrap();
                let resource = wit_parser::TypeDef {
                    name: Some(ident_name(wi_type.identifier.0)),
                    kind: wit_parser::TypeDefKind::Type(wit_type),
                    owner: wit_parser::TypeOwner::Interface(interface_id),
                    docs: Default::default(),
                };
                add_type(&mut resolve, resource)?;
            }
            Definition::Interface(interface) => {
                let resource = wit_parser::TypeDef {
                    name: Some(ident_name(interface.identifier.0)),
                    kind: wit_parser::TypeDefKind::Resource,
                    owner: wit_parser::TypeOwner::Interface(interface_id),
                    docs: Default::default(),
                };
                let resource_id = add_type(&mut resolve, resource)?;

                for member in interface.members.body {
                    match member {
                        weedle::interface::InterfaceMember::Const(_) => todo!(),
                        weedle::interface::InterfaceMember::Iterable(_) => todo!(),
                        weedle::interface::InterfaceMember::AsyncIterable(_) => todo!(),
                        weedle::interface::InterfaceMember::Maplike(_) => todo!(),
                        weedle::interface::InterfaceMember::Setlike(_) => todo!(),
                        weedle::interface::InterfaceMember::Stringifier(_) => todo!(),
                        weedle::interface::InterfaceMember::Attribute(attr) => {
                            let attr_name = ident_name(&attr.identifier.0);
                            let attr_type = wi2w_type(&mut resolve, &attr.type_.type_)?;
                            let method_kind = match attr.modifier {
                                Some(weedle::interface::StringifierOrInheritOrStatic::Static(
                                    _,
                                )) => wit_parser::FunctionKind::Static(resource_id),
                                _ => wit_parser::FunctionKind::Method(resource_id),
                            };
                            let getter = wit_parser::Function {
                                name: attr_name.clone(),
                                kind: method_kind.clone(),
                                params: Default::default(),
                                results: wit_parser::Results::Anon(attr_type),
                                docs: Default::default(),
                            };
                            let interface = resolve.interfaces.get_mut(interface_id).unwrap();
                            interface.functions.insert(attr_name.clone(), getter);
                            if attr.readonly.is_none() {
                                let setter_name = format!("set-{attr_name}");
                                let setter = wit_parser::Function {
                                    name: setter_name.clone(),
                                    kind: method_kind,
                                    params: vec![(attr_name, attr_type)],
                                    results: wit_parser::Results::Named(Default::default()),
                                    docs: Default::default(),
                                };
                                let interface = resolve.interfaces.get_mut(interface_id).unwrap();
                                interface.functions.insert(setter_name, setter);
                            }
                        }
                        weedle::interface::InterfaceMember::Constructor(constructor) => {
                            let function = wit_parser::Function {
                                name: String::new(),
                                kind: wit_parser::FunctionKind::Constructor(resource_id),
                                params: function_args(&constructor.args.body, &mut resolve)?,
                                docs: Default::default(),
                                results: wit_parser::Results::Named(Default::default()),
                            };
                            let interface = resolve.interfaces.get_mut(interface_id).unwrap();
                            interface
                                .functions
                                .insert("function_name".to_string(), function);
                        }
                        weedle::interface::InterfaceMember::Operation(operation) => {
                            let function_name = ident_name(operation.identifier.unwrap().0);
                            let function = wit_parser::Function {
                                name: function_name.to_string(),
                                kind: wit_parser::FunctionKind::Method(resource_id),
                                results: match &operation.return_type {
                                    weedle::types::ReturnType::Undefined(_) => {
                                        wit_parser::Results::Named(Default::default())
                                    }
                                    weedle::types::ReturnType::Type(type_) => {
                                        let type_ = wi2w_type(&mut resolve, &type_).unwrap();
                                        wit_parser::Results::Anon(type_)
                                    }
                                },
                                params: function_args(&operation.args.body, &mut resolve)?,
                                docs: Default::default(),
                            };
                            let interface = resolve.interfaces.get_mut(interface_id).unwrap();
                            interface.functions.insert(function_name, function);
                        }
                    }
                }
            }
            Definition::Dictionary(dict) => {
                let fields = dict
                    .members
                    .body
                    .iter()
                    .map(|mem| wit_parser::Field {
                        name: ident_name(mem.identifier.0),
                        ty: wi2w_type(&mut resolve, &mem.type_).unwrap(),
                        docs: Default::default(),
                    })
                    .collect_vec();
                let record = wit_parser::Record { fields };
                let out = wit_parser::TypeDef {
                    name: Some(ident_name(dict.identifier.0)),
                    kind: wit_parser::TypeDefKind::Record(record),
                    owner: wit_parser::TypeOwner::None,
                    docs: Default::default(),
                };
                add_type(&mut resolve, out)?;
            }
            Definition::Enum(e) => {
                let cases = e
                    .values
                    .body
                    .list
                    .iter()
                    .map(|case| wit_parser::EnumCase {
                        name: ident_name(case.0),
                        docs: Default::default(),
                    })
                    .collect_vec();
                let out = wit_parser::Enum { cases };
                let out = wit_parser::TypeDef {
                    name: Some(ident_name(e.identifier.0)),
                    kind: wit_parser::TypeDefKind::Enum(out),
                    owner: wit_parser::TypeOwner::None,
                    docs: Default::default(),
                };
                add_type(&mut resolve, out)?;
            }
        }
    }

    Ok(resolve)
}

pub fn add_type(
    resolve: &mut Resolve,
    type_def: wit_parser::TypeDef,
) -> anyhow::Result<wit_parser::TypeId> {
    if let Some((id, td)) = resolve
        .types
        .iter()
        .find(|(_, td)| td.name.is_some() && td.name == type_def.name)
    {
        assert_eq!(td.kind, wit_parser::TypeDefKind::Unknown);
        // drop(td);
        let td = resolve.types.get_mut(id).unwrap();
        td.kind = type_def.kind;
        td.owner = type_def.owner;
        td.docs = type_def.docs;
        Ok(id)
    } else {
        Ok(resolve.types.alloc(type_def))
    }
}

pub fn get_type_id(resolve: &mut Resolve, type_name: String) -> wit_parser::TypeId {
    let type_ = resolve
        .types
        .iter()
        .find(|(_, type_)| type_.name.as_ref() == Some(&type_name));

    match type_ {
        Some((type_id, _)) => type_id,
        None => resolve.types.alloc(wit_parser::TypeDef {
            name: Some(type_name),
            kind: wit_parser::TypeDefKind::Unknown,
            owner: wit_parser::TypeOwner::None,
            docs: Default::default(),
        }),
    }
}

fn function_args(
    args: &weedle::argument::ArgumentList,
    mut resolve: &mut Resolve,
) -> anyhow::Result<wit_parser::Params> {
    Ok(args
        .list
        .iter()
        .map(|arg| match arg {
            weedle::argument::Argument::Variadic(_) => todo!(),
            weedle::argument::Argument::Single(arg) => {
                let name = ident_name(arg.identifier.0);
                let type_ = wi2w_type(&mut resolve, &arg.type_.type_).unwrap();
                (name, type_)
            }
        })
        .collect_vec())
}

pub(super) fn ident_name(src: &str) -> String {
    // doing to_pascal_case first to get rid of all dashes. E.g. "A-1" should turn into "a1" and not "a-1".
    let output = src.to_pascal_case().to_kebab_case();
    match output.as_str() {
        "u8" | "u16" | "u32" | "u64" | "s8" | "s16" | "s32" | "s64" | "float32" | "float64"
        | "char" | "bool" | "string" | "tuple" | "list" | "option" | "result" | "use" | "type"
        | "resource" | "func" | "record" | "enum" | "flags" | "variant" | "static"
        | "interface" | "world" | "import" | "export" | "package" => {
            format!("%{output}")
        }
        _ => output,
    }
}
