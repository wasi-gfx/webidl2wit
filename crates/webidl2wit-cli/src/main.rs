//! This binary enables use of the [`webidl2wit`] package, providing it's functionality in
//! an easy to use CLI.

use std::path::PathBuf;

use anyhow::{bail, Context as _, Result};
use clap::Parser as _;

use semver::Version;
use webidl2wit::{webidl_to_wit, ConversionOptions, HandleUnsupported, PackageName};
use weedle::Parse;

mod style;
use style::CLI_STYLES;

#[derive(Debug, Clone, PartialEq, Eq, clap::Parser)]
#[command(name = "webidl2wit")]
#[command(bin_name = "webidl2wit")]
#[command(version)]
#[command(about, long_about)]
#[command(styles = CLI_STYLES)]
struct Cmd {
    /// Input file/directory path (WebIDL)
    #[clap(short = 'i', long = "webidl")]
    webidl_path: PathBuf,

    /// Interface name to use
    #[clap(long)]
    webidl_interface: Option<String>,

    /// Interface to use as a singleton
    #[clap(long)]
    webidl_singleton_interface: Option<String>,

    /// Output file/directory path (WIT)
    ///
    /// If this is not provided, then output will be written to stdout
    #[clap(short = 'o', long = "wit")]
    wit_path: Option<PathBuf>,

    /// WIT namespace to use for the resulting output
    #[clap(long)]
    wit_ns: Option<String>,

    /// WIT package to use for the resulting output
    #[clap(long, alias = "wit-pkg")]
    wit_package: Option<String>,

    /// WIT version to use for the package
    #[clap(long)]
    wit_version: Option<String>,
}

fn main() -> Result<()> {
    let Cmd {
        webidl_path,
        wit_path,
        wit_ns,
        wit_package,
        wit_version,
        webidl_interface,
        webidl_singleton_interface,
        ..
    } = Cmd::try_parse().context("missing/invalid command line arguments")?;

    // Resolve some options
    let wit_ns = wit_ns.unwrap_or_else(|| "webidl".into());
    let wit_package = wit_package.unwrap_or_else(|| "pkg".into());
    let wit_version = if let Some(raw) = wit_version {
        Some(
            Version::parse(&raw)
                .map_err(|e| anyhow::anyhow!(e).context("failed to parse version"))?,
        )
    } else {
        None
    };

    // Ensure the input file path exists
    if !std::fs::exists(&webidl_path)? {
        bail!("missing path [{}]", webidl_path.display());
    }

    // Ensure the output path does not already exist
    if let Some(ref p) = wit_path {
        if std::fs::exists(p)? {
            bail!("output path [{}] already exists", p.display());
        }
    }

    // Read in the WebIDL input
    let webidl_input = std::fs::read_to_string(&webidl_path).with_context(|| {
        format!(
            "failed to read input WebIDL from [{}]",
            webidl_path.display()
        )
    })?;

    // Parse the WebIDL input
    let webidl = parse_webidl(&webidl_input)?;

    // Build the conversion options
    let opts = ConversionOptions {
        package_name: PackageName::new(wit_ns, wit_package, wit_version),
        interface_name: webidl_interface.unwrap_or_else(|| "default".into()),
        unsupported_features: HandleUnsupported::Bail,
        singleton_interface: webidl_singleton_interface,
        ..Default::default()
    };

    // Build the WIT package from the given WebIDL
    let pkg =
        webidl_to_wit(webidl, opts).context("failed to perform conversion from WebIDL to WIT")?;

    // Write out the WIT package to disk
    let package_str = pkg.to_string();
    if let Some(ref output_path) = wit_path {
        std::fs::write(output_path, package_str).with_context(|| {
            format!("failed to write output to file [{}]", output_path.display())
        })?;
    } else {
        println!("{}", package_str);
    }

    Ok(())
}

/// Helper function for parsing WebIDL using `weedle::parse`
///
/// This function exists due to slightly confusing `'static` bounds
/// requirements that will be inferred on `weedle::parse` by default.
fn parse_webidl(input: &str) -> Result<Vec<weedle::Definition>> {
    match weedle::Definitions::parse(input) {
        Ok(("", parsed)) => Ok(parsed),

        Ok((remaining, _))
        | Err(weedle::Err::Error((remaining, _)))
        | Err(weedle::Err::Failure((remaining, _))) => {
            bail!(
                "failed to parse entirety of input (stopped at {})",
                input.len() - remaining.len()
            )
        }

        Err(e) => {
            bail!("failed to parse: {e}");
        }
    }
}
