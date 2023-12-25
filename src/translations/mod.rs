use itertools::Itertools;
use weedle::{Definition, Definitions as WebIdlDefinitions};
use wit_parser::Resolve;

use crate::translations::types_::wi2w_type;

mod types_;

pub fn webidl_to_wit(webidl: WebIdlDefinitions) -> anyhow::Result<Resolve> {
    let mut resolve: Resolve = Default::default();

    for item in webidl {
        match item {
            Definition::Callback(_) => todo!(),
            Definition::CallbackInterface(_) => todo!(),
            Definition::Interface(_) => todo!(),
            Definition::InterfaceMixin(_) => todo!(),
            Definition::Namespace(_) => todo!(),
            Definition::PartialInterface(_) => todo!(),
            Definition::PartialInterfaceMixin(_) => todo!(),
            Definition::PartialDictionary(_) => todo!(),
            Definition::PartialNamespace(_) => todo!(),
            Definition::Typedef(_) => todo!(),
            Definition::IncludesStatement(_) => todo!(),
            Definition::Implements(_) => todo!(),
            Definition::Dictionary(dict) => {
                let fields = dict
                    .members
                    .body
                    .iter()
                    .map(|mem| wit_parser::Field {
                        name: mem.identifier.0.to_string(),
                        ty: wi2w_type(&mem.type_).unwrap(),
                        docs: Default::default(),
                    })
                    .collect_vec();
                let record = wit_parser::Record { fields };
                let out = wit_parser::TypeDef {
                    name: Some(dict.identifier.0.to_string()),
                    kind: wit_parser::TypeDefKind::Record(record),
                    owner: wit_parser::TypeOwner::None,
                    docs: Default::default(),
                };
                resolve.types.alloc(out);
            }
            Definition::Enum(e) => {
                let cases = e
                    .values
                    .body
                    .list
                    .iter()
                    .map(|case| wit_parser::EnumCase {
                        name: case.0.to_string(),
                        docs: Default::default(),
                    })
                    .collect_vec();
                let out = wit_parser::Enum { cases };
                let out = wit_parser::TypeDef {
                    name: Some(e.identifier.0.to_string()),
                    kind: wit_parser::TypeDefKind::Enum(out),
                    owner: wit_parser::TypeOwner::None,
                    docs: Default::default(),
                };
                resolve.types.alloc(out);
            }
        }
    }

    Ok(resolve)
}
