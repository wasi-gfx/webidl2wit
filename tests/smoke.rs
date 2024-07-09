use pretty_assertions::assert_eq;
use std::{fs, path::Path};
use webidl2wit::{ConversionOptions, HandleUnsupported};

fn compare(path: &str, opts: ConversionOptions) {
    let webidl_input =
        fs::read_to_string(Path::new(&format!("./tests/inputs/{path}.idl"))).unwrap();
    let wit_input = fs::read_to_string(Path::new(&format!("./tests/inputs/{path}.wit"))).unwrap();

    let webidl_ast = weedle::parse(&webidl_input).unwrap();
    let wit_ast = webidl2wit::webidl_to_wit(webidl_ast, opts).unwrap();
    let wit_output = wit_ast.to_string();

    assert_eq!(wit_input, wit_output)
}

#[test]
fn enum_() {
    compare("enum", Default::default());
}

#[test]
fn resource() {
    compare("resource", Default::default());
}

#[test]
fn record() {
    compare("record", Default::default());
}

#[test]
fn type_() {
    compare("type", Default::default());
}

#[test]
fn borrow() {
    compare("borrow", Default::default());
}

#[test]
fn webgpu() {
    compare("webgpu", Default::default());
}

#[test]
fn console() {
    compare("console", Default::default());
}

#[test]
fn window() {
    compare("window", Default::default());
}

#[test]
fn unsupported() {
    compare(
        "unsupported",
        ConversionOptions {
            unsupported_features: HandleUnsupported::Bail,
            ..Default::default()
        },
    );
}
