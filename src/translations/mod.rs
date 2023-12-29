use convert_case::{Case::Kebab, Casing};
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
            Definition::Typedef(_) => todo!(),
            Definition::IncludesStatement(_) => todo!(),
            Definition::Implements(_) => todo!(),
            Definition::Interface(interface) => {
                let resource = wit_parser::TypeDef {
                    name: Some(interface.identifier.0.to_string().to_case(Kebab)),
                    kind: wit_parser::TypeDefKind::Resource,
                    owner: wit_parser::TypeOwner::Interface(interface_id),
                    docs: Default::default(),
                };
                let resource_id = resolve.types.alloc(resource);

                for member in interface.members.body {
                    match member {
                        weedle::interface::InterfaceMember::Const(_) => todo!(),
                        weedle::interface::InterfaceMember::Attribute(_) => todo!(),
                        weedle::interface::InterfaceMember::Constructor(_) => todo!(),
                        weedle::interface::InterfaceMember::Iterable(_) => todo!(),
                        weedle::interface::InterfaceMember::AsyncIterable(_) => todo!(),
                        weedle::interface::InterfaceMember::Maplike(_) => todo!(),
                        weedle::interface::InterfaceMember::Setlike(_) => todo!(),
                        weedle::interface::InterfaceMember::Stringifier(_) => todo!(),
                        weedle::interface::InterfaceMember::Operation(operation) => {
                            let function_name =
                                operation.identifier.unwrap().0.to_string().to_case(Kebab);
                            let function = wit_parser::Function {
                                name: function_name.to_string(),
                                kind: wit_parser::FunctionKind::Method(resource_id),
                                params: operation
                                    .args
                                    .body
                                    .list
                                    .iter()
                                    .map(|arg| match arg {
                                        weedle::argument::Argument::Variadic(_) => todo!(),
                                        weedle::argument::Argument::Single(arg) => {
                                            let name = arg.identifier.0.to_string().to_case(Kebab);
                                            let type_ =
                                                wi2w_type(&resolve, &arg.type_.type_).unwrap();
                                            (name, type_)
                                        }
                                    })
                                    .collect_vec(),
                                results: wit_parser::Results::Anon({
                                    match &operation.return_type {
                                        weedle::types::ReturnType::Undefined(_) => todo!(),
                                        weedle::types::ReturnType::Type(type_) => {
                                            wi2w_type(&resolve, &type_).unwrap()
                                        }
                                    }
                                }),
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
                        name: mem.identifier.0.to_string().to_case(Kebab),
                        ty: wi2w_type(&resolve, &mem.type_).unwrap(),
                        docs: Default::default(),
                    })
                    .collect_vec();
                let record = wit_parser::Record { fields };
                let out = wit_parser::TypeDef {
                    name: Some(dict.identifier.0.to_string().to_case(Kebab)),
                    kind: wit_parser::TypeDefKind::Record(record),
                    owner: wit_parser::TypeOwner::None,
                    docs: Default::default(),
                };
                resolve.types.alloc(out);
            }
            Definition::Enum(e) => {
                let cases = e
                    .values
                    .body
                    .list
                    .iter()
                    .map(|case| wit_parser::EnumCase {
                        name: case.0.to_string().to_case(Kebab),
                        docs: Default::default(),
                    })
                    .collect_vec();
                let out = wit_parser::Enum { cases };
                let out = wit_parser::TypeDef {
                    name: Some(e.identifier.0.to_string().to_case(Kebab)),
                    kind: wit_parser::TypeDefKind::Enum(out),
                    owner: wit_parser::TypeOwner::None,
                    docs: Default::default(),
                };
                resolve.types.alloc(out);
            }
        }
    }

    Ok(resolve)
}
