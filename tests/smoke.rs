use pretty_assertions::assert_eq;
use std::{fs, path::Path};

fn compare(path: &str) {
    let webidl_input =
        fs::read_to_string(Path::new(&format!("./tests/inputs/{path}.idl"))).unwrap();
    let wit_input = fs::read_to_string(Path::new(&format!("./tests/inputs/{path}.wit"))).unwrap();

    let webidl_ast = weedle::parse(&webidl_input).unwrap();
    let wit_ast =
        webidl_wit::webidl_to_wit(webidl_ast, webidl_wit::ConversionOptions::default()).unwrap();
    let wit_output = wit_ast.to_string();

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
fn borrow() {
    compare("borrow");
}

#[test]
fn webgpu() {
    compare("webgpu");
}
