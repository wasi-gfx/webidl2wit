//! This binary enables use of the [`webidl2wit`] package, providing it's functionality in
//! an easy to use CLI.

use std::path::PathBuf;

use anyhow::{Context as _, Result};
use clap::Parser as _;

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
    #[clap(short = 'i', long)]
    input: PathBuf,

    /// Output file/directory path (WIT)
    ///
    /// If this is not provided, then output will be written to stdout
    #[clap(short = 'o', long)]
    output: Option<PathBuf>,
}

fn main() -> Result<()> {
    let _cmd = Cmd::try_parse().context("missing/invalid command line arguments")?;
    Ok(())
}
