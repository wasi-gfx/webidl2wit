use std::path::PathBuf;
use std::process::{Command, Stdio};

use anyhow::{Context as _, Result};

const WEBIDL_FIXTURE_PATHS: [&str; 1] = ["fixtures/book-store.webidl"];

/// Ensure that basic generation works for WebIDL features
#[test]
fn test_webidl_fixtures() -> Result<()> {
    let fixture_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
    for fixture_path in WEBIDL_FIXTURE_PATHS {
        let output = Command::new(env!("CARGO_BIN_EXE_webidl2wit"))
            .args([
                "--webidl",
                &format!("{}", fixture_dir.join(fixture_path).display()),
                "--wit-ns",
                "test-ns",
                "--wit-package",
                "test-pkg",
            ])
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .with_context(|| format!("failed to run webidl2wit for fixture [{fixture_path}]"))?;
        assert!(output.status.success());
        let output = String::from_utf8(output.stdout)?;
        assert!(output.contains("test-ns") && output.contains("test-pkg"));
    }

    Ok(())
}
