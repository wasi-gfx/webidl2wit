#![feature(assert_matches)]

use crate::to_wit::ToWitSyntax;

mod to_wit;
mod translations;

fn main() -> anyhow::Result<()> {
    let webidl_code = include_str!("./web.idl");
    println!("{webidl_code}");
    let webidl = weedle::parse(&webidl_code)?;
    let wit = translations::webidl_to_wit(webidl)?;
    let wit_code = wit.to_wit_syntax(&wit)?;
    println!("{wit_code}");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{fs, path::Path};

    fn compare(path: &str) {
        fn add_interface(wit: &str) -> String {
            format!(
                "
                package foo:bar;
                interface biz {{{wit}}}
            "
            )
        }
        let webidl_input =
            fs::read_to_string(Path::new(&format!("./tests-input/{path}.idl"))).unwrap();
        let wit_input =
            fs::read_to_string(Path::new(&format!("./tests-input/{path}.wit"))).unwrap();
        let wit_input = add_interface(&wit_input);

        let webidl_ast = weedle::parse(&webidl_input).unwrap();
        let wit_ast = translations::webidl_to_wit(webidl_ast).unwrap();
        let wit_output = wit_ast.to_wit_syntax(&wit_ast).unwrap();
        let wit_output = add_interface(&wit_output);

        assert_eq!(wit_input, wit_output)
    }

    #[test]
    fn enum_() {
        compare("enum");
    }

    #[test]
    fn resource() {
        compare("resource");
    }

    #[test]
    fn record() {
        compare("record");
    }

    #[test]
    fn type_() {
        compare("type");
    }

    #[test]
    fn webgpu() {
        compare("webgpu");
    }
}
