pub trait ToWitSyntax {
    fn to_wit_syntax(&self) -> anyhow::Result<String>;
}

impl ToWitSyntax for wit_parser::Resolve {
    fn to_wit_syntax(&self) -> anyhow::Result<String> {
        let mut output = OutputBuilder::new();
        let mut indentation = 0;

        for (_, type_) in &self.types {
            match &type_.kind {
                wit_parser::TypeDefKind::Record(_) => todo!(),
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
                        output.add_line(indentation, &format!("{},", &case.name));
                    }
                    indentation -= 1;
                    output.add_line(indentation, &format!("}}"));
                }
            }
        }

        Ok(output.to_string())
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
