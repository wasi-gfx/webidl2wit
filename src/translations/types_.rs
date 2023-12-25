pub fn wi2w_type(wi: &weedle::types::Type) -> anyhow::Result<wit_parser::Type> {
    match wi {
        weedle::types::Type::Single(weedle::types::SingleType::NonAny(type_)) => {
            // match type_
            wi_non_any2w(type_)
        }
        weedle::types::Type::Single(weedle::types::SingleType::Any(_any)) => todo!(),
        weedle::types::Type::Union(_) => todo!(),
    }
}

fn wi_non_any2w(wi: &weedle::types::NonAnyType) -> anyhow::Result<wit_parser::Type> {
    Ok(match wi {
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
        weedle::types::NonAnyType::Boolean(_) => wit_parser::Type::Bool,
        weedle::types::NonAnyType::ByteString(_) => wit_parser::Type::String,
        weedle::types::NonAnyType::DOMString(_) => wit_parser::Type::String,
        weedle::types::NonAnyType::USVString(_) => wit_parser::Type::String,
        weedle::types::NonAnyType::Error(_) => todo!(),
        weedle::types::NonAnyType::Byte(_) => todo!(),
        weedle::types::NonAnyType::Octet(_) => todo!(),
        weedle::types::NonAnyType::Promise(_) => todo!(),
        weedle::types::NonAnyType::Sequence(_) => todo!(),
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
        weedle::types::NonAnyType::Identifier(_) => todo!(),
    })
}
