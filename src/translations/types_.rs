use std::collections::HashSet;

use convert_case::{Case, Casing};
use itertools::Itertools;
use wit_parser::Resolve;

use crate::to_wit::ToWitSyntax;

use super::{add_type, get_type_id};

pub fn wi2w_type(
    resolve: &mut Resolve,
    wi: &weedle::types::Type,
) -> anyhow::Result<wit_parser::Type> {
    match wi {
        weedle::types::Type::Single(weedle::types::SingleType::NonAny(type_)) => {
            wi_non_any2w(resolve, type_)
        }
        weedle::types::Type::Single(weedle::types::SingleType::Any(_any)) => todo!(),
        weedle::types::Type::Union(union) => {
            // using a HashSet to get rid of types that are different in WebIDL but are the same in wit.
            // e.g. `(long or DOMString or ByteString)` should not have two sting options.

            let cases = union
                .type_
                .body
                .list
                .iter()
                .map(|type_| match type_ {
                    weedle::types::UnionMemberType::Single(type_) => {
                        wi_non_any2w(resolve, &type_.type_).unwrap()
                    }
                    weedle::types::UnionMemberType::Union(_) => todo!(),
                })
                .collect::<HashSet<_>>();

            // Only create `Variant` if there's more than one type.
            if cases.len() == 1 {
                let type_ = cases.into_iter().next().unwrap();
                return Ok(type_);
            }

            let variant_name = cases
                .iter()
                .map(|type_| type_.to_wit_syntax(resolve).unwrap())
                .collect_vec()
                .join("-or-");
            let type_id = add_type(
                resolve,
                wit_parser::TypeDef {
                    name: Some(variant_name),
                    kind: wit_parser::TypeDefKind::Variant(wit_parser::Variant {
                        cases: cases
                            .into_iter()
                            .map(|case| wit_parser::Case {
                                name: case.to_wit_syntax(&resolve).unwrap(),
                                ty: Some(case),
                                docs: Default::default(),
                            })
                            .collect_vec(),
                    }),
                    owner: wit_parser::TypeOwner::None,
                    docs: Default::default(),
                },
            )?;
            Ok(wit_parser::Type::Id(type_id))
        }
    }
}

fn wi_non_any2w(
    resolve: &mut Resolve,
    wi: &weedle::types::NonAnyType,
) -> anyhow::Result<wit_parser::Type> {
    Ok(match wi {
        weedle::types::NonAnyType::Boolean(_) => wit_parser::Type::Bool,
        weedle::types::NonAnyType::ByteString(_) => wit_parser::Type::String,
        weedle::types::NonAnyType::DOMString(_) => wit_parser::Type::String,
        weedle::types::NonAnyType::USVString(_) => wit_parser::Type::String,
        weedle::types::NonAnyType::Integer(int) => match int.type_ {
            weedle::types::IntegerType::LongLong(int) => match int.unsigned {
                Some(_) => wit_parser::Type::U64,
                None => wit_parser::Type::S64,
            },
            weedle::types::IntegerType::Long(int) => match int.unsigned {
                Some(_) => wit_parser::Type::U32,
                None => wit_parser::Type::S32,
            },
            weedle::types::IntegerType::Short(int) => match int.unsigned {
                Some(_) => wit_parser::Type::U16,
                None => wit_parser::Type::S16,
            },
        },
        weedle::types::NonAnyType::FloatingPoint(float) => match float.type_ {
            weedle::types::FloatingPointType::Float(_) => wit_parser::Type::Float32,
            weedle::types::FloatingPointType::Double(_) => wit_parser::Type::Float64,
        },
        weedle::types::NonAnyType::Identifier(ident) => {
            let type_id = get_type_id(resolve, ident.type_.0.to_string().to_case(Case::Kebab));
            wit_parser::Type::Id(type_id)
        }
        weedle::types::NonAnyType::Promise(promise) => {
            // use wit_parser::TypeDefKind::Future instead?
            match &*promise.generics.body {
                weedle::types::ReturnType::Undefined(_) => todo!(),
                weedle::types::ReturnType::Type(type_) => wi2w_type(resolve, type_)?,
            }
        }
        weedle::types::NonAnyType::Sequence(seq) => {
            let type_ = wi2w_type(resolve, &*seq.type_.generics.body)?;
            let type_id = add_type(
                resolve,
                wit_parser::TypeDef {
                    name: None,
                    kind: wit_parser::TypeDefKind::List(type_),
                    owner: wit_parser::TypeOwner::None,
                    docs: Default::default(),
                },
            )?;

            wit_parser::Type::Id(type_id)
        }
        weedle::types::NonAnyType::Error(_) => todo!(),
        weedle::types::NonAnyType::Byte(_) => todo!(),
        weedle::types::NonAnyType::Octet(_) => todo!(),
        weedle::types::NonAnyType::Object(_) => todo!(),
        weedle::types::NonAnyType::Symbol(_) => todo!(),
        weedle::types::NonAnyType::ArrayBuffer(_) => todo!(),
        weedle::types::NonAnyType::DataView(_) => todo!(),
        weedle::types::NonAnyType::Int8Array(_) => todo!(),
        weedle::types::NonAnyType::Int16Array(_) => todo!(),
        weedle::types::NonAnyType::Int32Array(_) => todo!(),
        weedle::types::NonAnyType::Uint8Array(_) => todo!(),
        weedle::types::NonAnyType::Uint16Array(_) => todo!(),
        weedle::types::NonAnyType::Uint32Array(_) => todo!(),
        weedle::types::NonAnyType::Uint8ClampedArray(_) => todo!(),
        weedle::types::NonAnyType::Float32Array(_) => todo!(),
        weedle::types::NonAnyType::Float64Array(_) => todo!(),
        weedle::types::NonAnyType::ArrayBufferView(_) => todo!(),
        weedle::types::NonAnyType::BufferSource(_) => todo!(),
        weedle::types::NonAnyType::FrozenArrayType(_) => todo!(),
        weedle::types::NonAnyType::RecordType(_) => todo!(),
    })
}
