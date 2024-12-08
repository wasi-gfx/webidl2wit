use std::collections::BTreeMap;

use itertools::Itertools;
use wit_encoder::Ident;

use crate::{
    custom_resources::TypedArrayKind,
    translations::{ident_name, State},
};

impl<'a> State<'a> {
    // WebIdl 2 Wit
    pub fn wi2w_type(
        &mut self,
        wi: &weedle::types::Type,
        optional: bool,
    ) -> anyhow::Result<wit_encoder::Type> {
        match wi {
            weedle::types::Type::Single(weedle::types::SingleType::NonAny(type_)) => {
                self.wi_non_any2w(type_, optional)
            }
            weedle::types::Type::Single(weedle::types::SingleType::Any(_any)) => {
                Ok(wit_encoder::Type::named(self.found_any()))
            }
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
                            let type_ = self.wi_non_any2w(&type_.type_, false).unwrap();
                            let type_name = type_.to_string();
                            let type_name = clean_generic(type_name);
                            let type_ = self.borrow_resources(type_);
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

                if self.interface.items().iter().all(|dt| match dt {
                    wit_encoder::InterfaceItem::TypeDef(dt) => dt.name() != &variant_name,
                    wit_encoder::InterfaceItem::Function(_) => true,
                }) {
                    self.interface.type_def({
                        let cases = cases
                            .into_iter()
                            .map(|(name, case)| wit_encoder::VariantCase::value(name, case))
                            .collect_vec();
                        let variant = wit_encoder::TypeDef::variant(variant_name.clone(), cases);
                        variant
                    });
                }

                Ok(match optional {
                    true => wit_encoder::Type::option(wit_encoder::Type::named(variant_name)),
                    false => wit_encoder::Type::named(variant_name),
                })
            }
        }
    }

    // WebIdl non any 2 Wit
    fn wi_non_any2w(
        &mut self,
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
                // TODO: can remove this check one weedle has native support for AllowSharedBufferSource
                if ident.type_.0 == "AllowSharedBufferSource" {
                    self.add_allow_shared_buffer_source()?;
                }
                let mut type_ = wit_encoder::Type::named(ident_name(ident.type_.0));
                if ident.q_mark.is_some() {
                    type_ = wit_encoder::Type::option(type_)
                }
                type_
            }
            weedle::types::NonAnyType::Promise(promise) => {
                // use wit_encoder::TypeDefKind::Future instead?
                match &*promise.generics.body {
                    weedle::types::ReturnType::Undefined(_) => todo!(),
                    weedle::types::ReturnType::Type(type_) => self.wi2w_type(type_, false)?,
                }
            }
            weedle::types::NonAnyType::Sequence(seq) => {
                let type_ = self.wi2w_type(&*seq.type_.generics.body, seq.q_mark.is_some())?;
                wit_encoder::Type::list(type_)
            }
            weedle::types::NonAnyType::Error(_) => todo!(),
            weedle::types::NonAnyType::Byte(_) => wit_encoder::Type::S8,
            weedle::types::NonAnyType::Octet(_) => wit_encoder::Type::U8,
            weedle::types::NonAnyType::Object(_) => {
                let object = self.add_object();
                wit_encoder::Type::named(object)
            }
            weedle::types::NonAnyType::Symbol(_) => todo!(),
            weedle::types::NonAnyType::ArrayBuffer(_) => {
                let buffer = self.add_array_buffer()?;
                wit_encoder::Type::named(buffer)
            }
            weedle::types::NonAnyType::DataView(_) => {
                let view = self.add_data_view()?;
                wit_encoder::Type::named(view)
            }
            weedle::types::NonAnyType::Int8Array(_) => {
                let array = self.add_typed_array(TypedArrayKind::Int8)?;
                wit_encoder::Type::named(array)
            }
            weedle::types::NonAnyType::Int16Array(_) => {
                let array = self.add_typed_array(TypedArrayKind::Int16)?;
                wit_encoder::Type::named(array)
            }
            weedle::types::NonAnyType::Int32Array(_) => {
                let array = self.add_typed_array(TypedArrayKind::Int32)?;
                wit_encoder::Type::named(array)
            }
            weedle::types::NonAnyType::Uint8Array(_) => {
                let array = self.add_typed_array(TypedArrayKind::UInt8)?;
                wit_encoder::Type::named(array)
            }
            weedle::types::NonAnyType::Uint16Array(_) => {
                let array = self.add_typed_array(TypedArrayKind::UInt16)?;
                wit_encoder::Type::named(array)
            }
            weedle::types::NonAnyType::Uint32Array(_) => {
                let array = self.add_typed_array(TypedArrayKind::UInt32)?;
                wit_encoder::Type::named(array)
            }
            weedle::types::NonAnyType::Uint8ClampedArray(_) => {
                let array = self.add_typed_array(TypedArrayKind::UInt8Clamped)?;
                wit_encoder::Type::named(array)
            }
            weedle::types::NonAnyType::Float32Array(_) => {
                let array = self.add_typed_array(TypedArrayKind::Float32)?;
                wit_encoder::Type::named(array)
            }
            weedle::types::NonAnyType::Float64Array(_) => {
                let array = self.add_typed_array(TypedArrayKind::Float64)?;
                wit_encoder::Type::named(array)
            }
            weedle::types::NonAnyType::ArrayBufferView(_) => {
                let buffer_view = self.add_array_buffer_view()?;
                wit_encoder::Type::named(buffer_view)
            }
            weedle::types::NonAnyType::BufferSource(_) => {
                let buffer_source = self.add_buffer_source()?;
                wit_encoder::Type::named(buffer_source)
            }
            weedle::types::NonAnyType::FrozenArrayType(array) => {
                let type_ = self.wi2w_type(&*array.type_.generics.body, array.q_mark.is_some())?;
                wit_encoder::Type::list(type_)
            }
            weedle::types::NonAnyType::RecordType(record) => {
                let record = self.add_record(&record.type_)?;
                wit_encoder::Type::named(record)
            }
        };

        Ok(match optional {
            false => type_,
            true => wit_encoder::Type::option(type_),
        })
    }

    pub fn borrow_resources(&self, type_: wit_encoder::Type) -> wit_encoder::Type {
        match type_ {
            wit_encoder::Type::Option(type_) => {
                wit_encoder::Type::option(self.borrow_resources(*type_))
            }
            wit_encoder::Type::List(type_) => {
                wit_encoder::Type::list(self.borrow_resources(*type_))
            }
            wit_encoder::Type::Result(_) => todo!(),
            wit_encoder::Type::Tuple(_) => todo!(),
            wit_encoder::Type::Named(ref name) if self.resource_names.contains(&name) => {
                wit_encoder::Type::borrow(name.clone())
            }
            _ => type_,
        }
    }

    pub(super) fn found_any(&mut self) -> wit_encoder::Ident {
        let any_name = Ident::new("any");

        if !self.any_found {
            // TODO: this should run after all types (except variants) are added, and should have a case for each type.
            let any = wit_encoder::TypeDef::variant(
                any_name.clone(),
                [
                    wit_encoder::VariantCase::value("bool", wit_encoder::Type::Bool),
                    wit_encoder::VariantCase::value("s8", wit_encoder::Type::S8),
                    wit_encoder::VariantCase::value("s16", wit_encoder::Type::S16),
                    wit_encoder::VariantCase::value("s32", wit_encoder::Type::S32),
                    wit_encoder::VariantCase::value("s64", wit_encoder::Type::S64),
                    wit_encoder::VariantCase::value("u8", wit_encoder::Type::U8),
                    wit_encoder::VariantCase::value("u16", wit_encoder::Type::U16),
                    wit_encoder::VariantCase::value("u32", wit_encoder::Type::U32),
                    wit_encoder::VariantCase::value("u64", wit_encoder::Type::U64),
                    wit_encoder::VariantCase::value("f32", wit_encoder::Type::F32),
                    wit_encoder::VariantCase::value("f64", wit_encoder::Type::F64),
                    wit_encoder::VariantCase::value("string", wit_encoder::Type::String),
                ],
            );
            self.interface
                .items_mut()
                .push(wit_encoder::InterfaceItem::TypeDef(any));
            self.any_found = true;
        }

        any_name
    }
}

fn clean_generic(mut s: String) -> String {
    if s.contains("<") {
        s = s.replace("<", "-");
        s = s.replace(">", "")
    }
    s
}
