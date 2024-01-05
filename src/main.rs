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
