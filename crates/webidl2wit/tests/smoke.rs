use similar_asserts::assert_eq;
use std::{fs, path::Path};
use webidl2wit::{ConversionOptions, HandleUnsupported, ResourceInheritance};

fn assert_wit_parses(path: &str) {
    let mut resolve = wit_parser::Resolve::new();
    resolve.push_file("./tests/inputs/pollable.wit").unwrap();
    resolve
        .push_file(format!("./tests/inputs/{path}.wit"))
        .unwrap();
}

fn compare(path: &str, opts: ConversionOptions) {
    assert_wit_parses(path);
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
fn inheritance() {
    compare(
        "inheritance",
        ConversionOptions {
            resource_inheritance: ResourceInheritance::Both,
            ..Default::default()
        },
    );
}

#[test]
fn option() {
    compare("option", Default::default());
}

#[test]
fn order() {
    compare(
        "order",
        ConversionOptions {
            resource_inheritance: ResourceInheritance::DuplicateMethods,
            ..Default::default()
        },
    );
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
fn borrow_params() {
    compare("borrow-params", Default::default());
}

#[test]
fn webgpu() {
    compare(
        "webgpu",
        ConversionOptions {
            phantom_interface: vec![
                "Navigator".into(),
                "WorkerNavigator".into(),
                "HTMLVideoElement".into(),
                "HTMLImageElement".into(),
                "VideoFrame".into(),
                "ImageBitmap".into(),
                "ImageData".into(),
                "EventTarget".into(),
                "DOMException".into(),
                "Event".into(),
            ],
            phantom_dictionaries: vec![
                "EventInit".into(),
                "PredefinedColorSpace".into(),
                "HTMLCanvasElement".into(),
                "OffscreenCanvas".into(),
            ],
            resource_inheritance: ResourceInheritance::DuplicateMethods,
            ..Default::default()
        },
    );
}

#[test]
fn console() {
    compare(
        "console",
        ConversionOptions {
            singleton_interface: Some("console".into()),
            ..Default::default()
        },
    );
}

#[test]
fn overloading() {
    compare(
        "overloading",
        ConversionOptions {
            singleton_interface: Some("overloading".into()),
            unsupported_features: HandleUnsupported::Warn,
            ..Default::default()
        },
    );
}

#[test]
fn window() {
    compare(
        "window",
        ConversionOptions {
            phantom_interface: vec!["EventTarget".into()],
            phantom_dictionaries: vec!["StructuredSerializeOptions".into()],
            ..Default::default()
        },
    );
}

#[test]
fn custom_resources() {
    compare("custom-resources", Default::default());
}

#[test]
fn unsupported() {
    compare(
        "unsupported",
        ConversionOptions {
            unsupported_features: HandleUnsupported::Warn,
            singleton_interface: Some("Singleton".into()),
            ..Default::default()
        },
    );
}

#[test]
fn html_element() {
    compare(
        "html-element",
        ConversionOptions {
            unsupported_features: HandleUnsupported::Warn,
            ..Default::default()
        },
    );
}
