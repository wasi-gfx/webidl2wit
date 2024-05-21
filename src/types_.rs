use std::collections::BTreeMap;

use itertools::Itertools;

use crate::translations::ident_name;

// WebIdl 2 Wit
pub fn wi2w_type(
    interface: &mut wit_encoder::Interface,
    wi: &weedle::types::Type,
    optional: bool,
) -> anyhow::Result<wit_encoder::Type> {
    match wi {
        weedle::types::Type::Single(weedle::types::SingleType::NonAny(type_)) => {
            wi_non_any2w(interface, type_, optional)
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
                        let type_ = wi_non_any2w(interface, &type_.type_, false).unwrap();
                        let type_name = type_.to_string();
                        let type_name = clean_generic(type_name);
                        (type_name, type_)
                    }
                    weedle::types::UnionMemberType::Union(_) => todo!(),
                })
                .collect::<BTreeMap<_, _>>();

            // Only create `Variant` if there's more than one type.
            if cases.len() == 1 {
                let (_, type_) = cases.into_iter().next().unwrap();
                return Ok(type_);
            }

            let variant_name = cases
                .iter()
                .map(|(name, _)| name.to_owned())
                .collect_vec()
                .join("-or-");
            let variant_name = ident_name(&variant_name);

            if interface
                .type_defs()
                .iter()
                .all(|dt| dt.name() != &variant_name)
            {
                interface.type_def({
                    let cases = cases
                        .into_iter()
                        .map(|(name, case)| wit_encoder::VariantCase::value(name, case))
                        .collect_vec();
                    let variant = wit_encoder::TypeDef::variant(variant_name.clone(), cases);
                    variant
                });
            }

            Ok(wit_encoder::Type::named(variant_name))
        }
    }
}

// WebIdl non any 2 Wit
fn wi_non_any2w(
    interface: &mut wit_encoder::Interface,
    wi: &weedle::types::NonAnyType,
    optional: bool,
) -> anyhow::Result<wit_encoder::Type> {
    let type_ = match wi {
        weedle::types::NonAnyType::Boolean(_) => wit_encoder::Type::Bool,
        weedle::types::NonAnyType::ByteString(_) => wit_encoder::Type::String,
        weedle::types::NonAnyType::DOMString(_) => wit_encoder::Type::String,
        weedle::types::NonAnyType::USVString(_) => wit_encoder::Type::String,
        weedle::types::NonAnyType::Integer(int) => match int.type_ {
            weedle::types::IntegerType::LongLong(int) => match int.unsigned {
                Some(_) => wit_encoder::Type::U64,
                None => wit_encoder::Type::S64,
            },
            weedle::types::IntegerType::Long(int) => match int.unsigned {
                Some(_) => wit_encoder::Type::U32,
                None => wit_encoder::Type::S32,
            },
            weedle::types::IntegerType::Short(int) => match int.unsigned {
                Some(_) => wit_encoder::Type::U16,
                None => wit_encoder::Type::S16,
            },
        },
        weedle::types::NonAnyType::FloatingPoint(float) => match float.type_ {
            weedle::types::FloatingPointType::Float(_) => wit_encoder::Type::F32,
            weedle::types::FloatingPointType::Double(_) => wit_encoder::Type::F64,
        },
        weedle::types::NonAnyType::Identifier(ident) => {
            wit_encoder::Type::named(ident_name(ident.type_.0))
        }
        weedle::types::NonAnyType::Promise(promise) => {
            // use wit_encoder::TypeDefKind::Future instead?
            match &*promise.generics.body {
                weedle::types::ReturnType::Undefined(_) => todo!(),
                weedle::types::ReturnType::Type(type_) => wi2w_type(interface, type_, false)?,
            }
        }
        weedle::types::NonAnyType::Sequence(seq) => {
            let type_ = wi2w_type(interface, &*seq.type_.generics.body, seq.q_mark.is_some())?;
            wit_encoder::Type::list(type_)
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
    };

    Ok(match optional {
        false => type_,
        true => wit_encoder::Type::option(type_),
    })
}

fn clean_generic(mut s: String) -> String {
    if s.contains("<") {
        s = s.replace("<", "-");
        s = s.replace(">", "")
    }
    s
}
