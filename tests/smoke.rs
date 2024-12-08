use pretty_assertions::assert_eq;
use std::{fs, path::Path};
use webidl2wit::{ConversionOptions, HandleUnsupported, ResourceInheritance};

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
fn borrow() {
    compare("borrow", Default::default());
}

#[test]
fn webgpu() {
    compare(
        "webgpu",
        ConversionOptions {
            phantom_interface: vec![
                "HTMLVideoElement".into(),
                "HTMLImageElement".into(),
                "HTMLCanvasElement".into(),
                "OffscreenCanvas".into(),
                "VideoFrame".into(),
                "ImageBitmap".into(),
                "ImageData".into(),
                "EventTarget".into(),
                "DOMException".into(),
                "Event".into(),
            ],
            phantom_dictionaries: vec!["EventInit".into(), "PredefinedColorSpace".into()],
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
