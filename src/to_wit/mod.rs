use anyhow::Context;
use wit_parser::Resolve;

pub trait ToWitSyntax {
    fn to_wit_syntax(&self, resolve: &Resolve) -> anyhow::Result<String>;
}

impl ToWitSyntax for wit_parser::Resolve {
    fn to_wit_syntax(&self, resolve: &Resolve) -> anyhow::Result<String> {
        let mut output = OutputBuilder::new();
        let mut indentation = 0;

        for (_, type_) in &self.types {
            match &type_.kind {
                wit_parser::TypeDefKind::Record(record) => {
                    output.add_line(
                        indentation,
                        &format!(
                            "record {} {{",
                            &type_.name.as_ref().unwrap_or(&String::new())
                        ),
                    );
                    indentation += 1;
                    for case in &record.fields {
                        let ty = &case.ty.to_wit_syntax(&resolve)?;
                        output.add_line(indentation, &format!("{}: {},", case.name, ty));
                    }
                    indentation -= 1;
                    output.add_line(indentation, &format!("}}"));
                }
                wit_parser::TypeDefKind::Resource => todo!(),
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
                wit_parser::TypeDefKind::Unknown => todo!(),
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
                let name = resolve
                    .types
                    .get(*id)
                    .context("")?
                    .name
                    .as_ref()
                    .context("")?;
                return Ok(name.to_owned());
            }
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
