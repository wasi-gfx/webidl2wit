use anyhow::Context;
use itertools::Itertools;
use wit_parser::Resolve;

pub trait ToWitSyntax {
    fn to_wit_syntax(&self, resolve: &Resolve) -> anyhow::Result<String>;
}

impl ToWitSyntax for wit_parser::Resolve {
    fn to_wit_syntax(&self, resolve: &Resolve) -> anyhow::Result<String> {
        let mut output = OutputBuilder::new();
        let mut indentation = 0;

        for (_, type_) in &self.types {
            // unnamed types don't need to be in output.
            if let Some(name) = &type_.name {
                match &type_.kind {
                    wit_parser::TypeDefKind::Record(record) => {
                        output.add_line(indentation, &format!("record {} {{", &name));
                        indentation += 1;
                        for case in &record.fields {
                            let ty = &case.ty.to_wit_syntax(&resolve)?;
                            output.add_line(indentation, &format!("{}: {},", case.name, ty));
                        }
                        indentation -= 1;
                        output.add_line(indentation, &format!("}}"));
                    }
                    wit_parser::TypeDefKind::Resource => {
                        output.add_line(indentation, &format!("resource {} {{", &name));
                        indentation += 1;

                        let interface_id = match type_.owner {
                            wit_parser::TypeOwner::World(_) => todo!(),
                            wit_parser::TypeOwner::Interface(interface_id) => interface_id,
                            wit_parser::TypeOwner::None => todo!(),
                        };

                        let interface = resolve.interfaces.get(interface_id).unwrap();

                        for (func_name, function) in &interface.functions {
                            let return_ = match &function.results {
                                wit_parser::Results::Named(returns) => {
                                    if returns.is_empty() {
                                        None
                                    } else {
                                        todo!()
                                    }
                                }
                                wit_parser::Results::Anon(return_type) => {
                                    Some(return_type.to_wit_syntax(&resolve)?)
                                }
                            };
                            let return_ = match return_ {
                                Some(return_) => format!(" -> {return_}"),
                                None => String::new(),
                            };
                            let params = function
                                .params
                                .iter()
                                .map(|(param_name, param_type)| {
                                    format!(
                                        "{}: {}",
                                        param_name,
                                        param_type.to_wit_syntax(&resolve).unwrap()
                                    )
                                })
                                .collect_vec()
                                .join(", ");
                            let static_ = match function.kind {
                                wit_parser::FunctionKind::Freestanding => todo!(),
                                wit_parser::FunctionKind::Method(_) => String::new(),
                                wit_parser::FunctionKind::Static(_) => String::from(" static"),
                                wit_parser::FunctionKind::Constructor(_) => todo!(),
                            };
                            output.add_line(
                                indentation,
                                &format!("{func_name}{static_}: func({params}){return_};"),
                            );
                        }

                        indentation -= 1;
                        output.add_line(indentation, &format!("}}"));
                    }
                    wit_parser::TypeDefKind::Handle(_) => todo!(),
                    wit_parser::TypeDefKind::Flags(_) => todo!(),
                    wit_parser::TypeDefKind::Tuple(_) => todo!(),
                    wit_parser::TypeDefKind::Variant(_) => todo!(),
                    wit_parser::TypeDefKind::Option(_) => todo!(),
                    wit_parser::TypeDefKind::Result(_) => todo!(),
                    wit_parser::TypeDefKind::List(_) => todo!(),
                    wit_parser::TypeDefKind::Future(_) => todo!(),
                    wit_parser::TypeDefKind::Stream(_) => todo!(),
                    wit_parser::TypeDefKind::Type(_) => todo!(),
                    wit_parser::TypeDefKind::Unknown => panic!("Found unresolved type."),
                    wit_parser::TypeDefKind::Enum(e) => {
                        output.add_line(
                            indentation,
                            &format!("enum {} {{", &type_.name.as_ref().unwrap_or(&String::new())),
                        );
                        indentation += 1;
                        for case in &e.cases {
                            output.add_line(indentation, &format!("{},", case.name));
                        }
                        indentation -= 1;
                        output.add_line(indentation, &format!("}}"));
                    }
                }
            }
        }

        Ok(output.to_string())
    }
}

impl ToWitSyntax for wit_parser::Type {
    fn to_wit_syntax(&self, resolve: &Resolve) -> anyhow::Result<String> {
        Ok(String::from(match self {
            wit_parser::Type::Bool => "bool",
            wit_parser::Type::U8 => "u8",
            wit_parser::Type::U16 => "u16",
            wit_parser::Type::U32 => "u32",
            wit_parser::Type::U64 => "u64",
            wit_parser::Type::S8 => "s8",
            wit_parser::Type::S16 => "s16",
            wit_parser::Type::S32 => "s32",
            wit_parser::Type::S64 => "s64",
            wit_parser::Type::Float32 => "float32",
            wit_parser::Type::Float64 => "float64",
            wit_parser::Type::Char => "char",
            wit_parser::Type::String => "string",
            wit_parser::Type::Id(id) => {
                let type_ = resolve.types.get(*id).context("Can't find type.")?;
                return Ok(match &type_.name {
                    Some(name) => name.to_owned(),
                    None => type_.kind.to_wit_syntax(resolve)?,
                });
            }
        }))
    }
}

impl ToWitSyntax for wit_parser::TypeDefKind {
    fn to_wit_syntax(&self, resolve: &Resolve) -> anyhow::Result<String> {
        Ok(String::from(match self {
            wit_parser::TypeDefKind::List(type_) => {
                let type_ = type_.to_wit_syntax(resolve)?;
                format!("list<{type_}>")
            }
            wit_parser::TypeDefKind::Record(_) => todo!(),
            wit_parser::TypeDefKind::Resource => todo!(),
            wit_parser::TypeDefKind::Handle(_) => todo!(),
            wit_parser::TypeDefKind::Flags(_) => todo!(),
            wit_parser::TypeDefKind::Tuple(_) => todo!(),
            wit_parser::TypeDefKind::Variant(_) => todo!(),
            wit_parser::TypeDefKind::Enum(_) => todo!(),
            wit_parser::TypeDefKind::Option(_) => todo!(),
            wit_parser::TypeDefKind::Result(_) => todo!(),
            wit_parser::TypeDefKind::Future(_) => todo!(),
            wit_parser::TypeDefKind::Stream(_) => todo!(),
            wit_parser::TypeDefKind::Type(_) => todo!(),
            wit_parser::TypeDefKind::Unknown => todo!(),
        }))
    }
}

#[derive(Default)]
struct OutputBuilder {
    val: String,
}
impl ToString for OutputBuilder {
    fn to_string(&self) -> String {
        self.val.to_owned()
    }
}
impl OutputBuilder {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn add_line(&mut self, indentation: usize, s: &str) {
        let indentation = std::iter::repeat(" ")
            .take(indentation * 4)
            .collect::<String>();
        self.val += &format!("{indentation}{s}\n");
    }
}
