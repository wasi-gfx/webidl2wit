use crate::translations::State;

pub(super) enum TypedArrayKind {
    UInt8,
    UInt8Clamped,
    UInt16,
    UInt32,
    Int8,
    Int16,
    Int32,
    Float32,
    Float64,
}

impl TypedArrayKind {
    fn to_wit_encoder_type(&self) -> wit_encoder::Type {
        match self {
            TypedArrayKind::UInt8 => wit_encoder::Type::U8,
            TypedArrayKind::UInt8Clamped => wit_encoder::Type::U8,
            TypedArrayKind::UInt16 => wit_encoder::Type::U16,
            TypedArrayKind::UInt32 => wit_encoder::Type::U32,
            TypedArrayKind::Int8 => wit_encoder::Type::S8,
            TypedArrayKind::Int16 => wit_encoder::Type::S16,
            TypedArrayKind::Int32 => wit_encoder::Type::S32,
            TypedArrayKind::Float32 => wit_encoder::Type::F32,
            TypedArrayKind::Float64 => wit_encoder::Type::F64,
        }
    }
    fn prefix_name(&self) -> &'static str {
        match self {
            TypedArrayKind::UInt8 => "uint8",
            TypedArrayKind::UInt8Clamped => "uint8-clamped",
            TypedArrayKind::UInt16 => "uint16",
            TypedArrayKind::UInt32 => "uint32",
            TypedArrayKind::Int8 => "int8",
            TypedArrayKind::Int16 => "int16",
            TypedArrayKind::Int32 => "int32",
            TypedArrayKind::Float32 => "float32",
            TypedArrayKind::Float64 => "float64",
        }
    }
}

impl<'a> State<'a> {
    pub(super) fn add_array_buffer<'b>(&mut self) -> anyhow::Result<wit_encoder::Ident> {
        let buffer_name = wit_encoder::Ident::new("array-buffer");
        if !self.type_def_exists(&buffer_name) {
            let constructor_options_name =
                wit_encoder::Ident::new(format!("array-buffer-constructor-options"));
            let constructor_options = wit_encoder::TypeDef::record(
                constructor_options_name.clone(),
                [("max-byte-length", wit_encoder::Type::U32)],
            );
            self.interface
                .items_mut()
                .push(wit_encoder::InterfaceItem::TypeDef(constructor_options));

            let array = wit_encoder::TypeDef::resource(
                buffer_name.clone(),
                [
                    {
                        let mut func = wit_encoder::ResourceFunc::constructor();
                        func.set_params(wit_encoder::Params::from_iter([
                            ("length", wit_encoder::Type::U32),
                            (
                                "options",
                                wit_encoder::Type::option(wit_encoder::Type::named(
                                    constructor_options_name.clone(),
                                )),
                            ),
                        ]));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("byte-length");
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::U32));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("slice");
                        func.set_params(wit_encoder::Params::from_iter([
                            ("begin", wit_encoder::Type::U32),
                            ("end", wit_encoder::Type::option(wit_encoder::Type::U32)),
                        ]));
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::named(
                            "array-buffer",
                        )));
                        func
                    },
                    // still missing:
                    // - isView
                    // - detached
                    // - maxByteLength
                    // - resizable
                    // - resize
                    // - transfer
                    // - transferToFixedLength
                ],
            );
            self.interface
                .items_mut()
                .push(wit_encoder::InterfaceItem::TypeDef(array));
        }
        Ok(buffer_name)
    }

    pub(super) fn add_typed_array<'b>(
        &mut self,
        kind: TypedArrayKind,
    ) -> anyhow::Result<wit_encoder::Ident> {
        self.add_array_buffer()?;
        let type_ = kind.to_wit_encoder_type();
        let array_name = wit_encoder::Ident::new(format!("{}-array", kind.prefix_name()));
        if !self.type_def_exists(&array_name) {
            let constructor_options_name =
                wit_encoder::Ident::new(format!("{array_name}-constructor-options"));
            let constructor_options = wit_encoder::TypeDef::variant(
                constructor_options_name.clone(),
                [
                    wit_encoder::VariantCase::value(
                        array_name.clone(),
                        wit_encoder::Type::named(array_name.clone()),
                    ),
                    wit_encoder::VariantCase::value("length", wit_encoder::Type::U32),
                    wit_encoder::VariantCase::value(
                        "array-buffer",
                        wit_encoder::Type::tuple([
                            wit_encoder::Type::named("array-buffer"),
                            wit_encoder::Type::option(wit_encoder::Type::U32),
                            wit_encoder::Type::option(wit_encoder::Type::U32),
                        ]),
                    ),
                    // wit_encoder::VariantCase::value(
                    //     "shared-array-buffer",
                    //     wit_encoder::Type::tuple([
                    //         wit_encoder::Type::named("shared-array-buffer"),
                    //         wit_encoder::Type::option(wit_encoder::Type::U32),
                    //         wit_encoder::Type::option(wit_encoder::Type::U32),
                    //     ]),
                    // ),
                ],
            );
            self.interface
                .items_mut()
                .push(wit_encoder::InterfaceItem::TypeDef(constructor_options));

            let set_src_name = wit_encoder::Ident::new(format!("{array_name}-set-src"));
            let set_src = wit_encoder::TypeDef::variant(
                set_src_name.clone(),
                [
                    wit_encoder::VariantCase::value("list", wit_encoder::Type::list(type_.clone())),
                    wit_encoder::VariantCase::value(
                        array_name.clone(),
                        wit_encoder::Type::named(array_name.clone()),
                    ),
                ],
            );
            self.interface
                .items_mut()
                .push(wit_encoder::InterfaceItem::TypeDef(set_src));

            let array = wit_encoder::TypeDef::resource(
                array_name.clone(),
                [
                    {
                        let mut func = wit_encoder::ResourceFunc::constructor();
                        func.set_params((
                            "options",
                            wit_encoder::Type::option(wit_encoder::Type::named(
                                constructor_options_name.clone(),
                            )),
                        ));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("fill");
                        func.set_params(wit_encoder::Params::from_iter([
                            ("value", type_.clone()),
                            ("start", wit_encoder::Type::option(wit_encoder::Type::U32)),
                            ("end", wit_encoder::Type::option(wit_encoder::Type::U32)),
                        ]));
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::named(
                            array_name.clone(),
                        )));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("buffer");
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::named(
                            "array-buffer",
                        )));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("length");
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::U32));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("byte-offset");
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::U32));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("byte-length");
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::U32));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("at");
                        func.set_params(("index", wit_encoder::Type::S32));
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::option(
                            type_.clone(),
                        )));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("copy-within");
                        func.set_params(wit_encoder::Params::from_iter([
                            ("target", wit_encoder::Type::U32),
                            ("start", wit_encoder::Type::U32),
                            ("end", wit_encoder::Type::option(wit_encoder::Type::U32)),
                        ]));
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::named(
                            array_name.clone(),
                        )));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("set");
                        func.set_params(wit_encoder::Params::from_iter([
                            ("src", wit_encoder::Type::named(set_src_name.clone())),
                            ("offset", wit_encoder::Type::U32),
                        ]));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("slice");
                        func.set_params(wit_encoder::Params::from_iter([
                            ("begin", wit_encoder::Type::U32),
                            ("end", wit_encoder::Type::U32),
                        ]));
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::named(
                            array_name.clone(),
                        )));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("subarray");
                        func.set_params(wit_encoder::Params::from_iter([
                            ("begin", wit_encoder::Type::U32),
                            ("end", wit_encoder::Type::U32),
                        ]));
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::named(
                            array_name.clone(),
                        )));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("values");
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::list(
                            type_.clone(),
                        )));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("get-index");
                        func.set_params(("index", wit_encoder::Type::U32));
                        func.set_results(wit_encoder::Results::anon(type_.clone()));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("set-index");
                        func.set_params(wit_encoder::Params::from_iter([
                            ("index", wit_encoder::Type::U32),
                            ("value", type_.clone()),
                        ]));
                        func
                    },
                    // still missing:
                    // - entries
                    // - every
                    // - filter
                    // - find
                    // - findIndex
                    // - findLast
                    // - findLastIndex
                    // - includes
                    // - indexOf
                    // - join
                    // - keys
                    // - lastIndexOf
                    // - map
                    // - reduce
                    // - reduceRight
                    // - some
                    // - sort
                    // - toReversed
                    // - toSorted
                    // - with
                    // - forEach
                ],
            );
            self.interface
                .items_mut()
                .push(wit_encoder::InterfaceItem::TypeDef(array));
        }
        Ok(array_name)
    }

    pub(super) fn add_data_view<'b>(&mut self) -> anyhow::Result<wit_encoder::Ident> {
        fn setter_getter(type_: wit_encoder::Type) -> Vec<wit_encoder::ResourceFunc> {
            let type_name = match type_ {
                wit_encoder::Type::U8 => "uint8",
                wit_encoder::Type::U16 => "uint16",
                wit_encoder::Type::U32 => "uint32",
                wit_encoder::Type::U64 => "uint64",
                wit_encoder::Type::S8 => "int8",
                wit_encoder::Type::S16 => "int16",
                wit_encoder::Type::S32 => "int32",
                wit_encoder::Type::S64 => "int64",
                wit_encoder::Type::F32 => "float32",
                wit_encoder::Type::F64 => "float64",
                _ => unimplemented!(),
            };

            let is8 = match type_ {
                wit_encoder::Type::U8 | wit_encoder::Type::S8 => true,
                _ => false,
            };

            vec![
                {
                    let mut func = wit_encoder::ResourceFunc::method(format!("get-{}", type_name));
                    if is8 {
                        func.set_params(("byte-offset", wit_encoder::Type::U32));
                    } else {
                        func.set_params(wit_encoder::Params::from_iter([
                            ("byte-offset", wit_encoder::Type::U32),
                            (
                                "little-endian",
                                wit_encoder::Type::option(wit_encoder::Type::Bool),
                            ),
                        ]));
                    }
                    func.set_results(wit_encoder::Results::anon(type_.clone()));
                    func
                },
                {
                    let mut func = wit_encoder::ResourceFunc::method(format!("set-{}", type_name));
                    if is8 {
                        func.set_params(wit_encoder::Params::from_iter([
                            ("byte-offset", wit_encoder::Type::U32),
                            ("value", type_.clone()),
                        ]));
                    } else {
                        func.set_params(wit_encoder::Params::from_iter([
                            ("byte-offset", wit_encoder::Type::U32),
                            ("value", type_.clone()),
                            (
                                "little-endian",
                                wit_encoder::Type::option(wit_encoder::Type::Bool),
                            ),
                        ]));
                    }
                    func
                },
            ]
        }
        let view_name = wit_encoder::Ident::new("data-view");
        if !self.type_def_exists(&view_name) {
            let view = wit_encoder::TypeDef::resource(view_name.clone(), {
                let mut funcs = vec![
                    {
                        let mut func = wit_encoder::ResourceFunc::constructor();
                        func.set_params(wit_encoder::Params::from_iter([
                            ("buffer", wit_encoder::Type::borrow("array-buffer")),
                            (
                                "byte-offset",
                                wit_encoder::Type::option(wit_encoder::Type::U32),
                            ),
                            (
                                "byte-length",
                                wit_encoder::Type::option(wit_encoder::Type::U32),
                            ),
                        ]));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("buffer");
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::named(
                            "array-buffer",
                        )));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("byte-length");
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::U32));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("byte-offset");
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::U32));
                        func
                    },
                ];
                funcs.append(&mut setter_getter(wit_encoder::Type::S8));
                funcs.append(&mut setter_getter(wit_encoder::Type::S16));
                funcs.append(&mut setter_getter(wit_encoder::Type::S32));
                funcs.append(&mut setter_getter(wit_encoder::Type::U8));
                funcs.append(&mut setter_getter(wit_encoder::Type::U16));
                funcs.append(&mut setter_getter(wit_encoder::Type::U32));
                funcs.append(&mut setter_getter(wit_encoder::Type::F32));
                funcs.append(&mut setter_getter(wit_encoder::Type::F64));
                funcs
            });
            self.interface
                .items_mut()
                .push(wit_encoder::InterfaceItem::TypeDef(view));
        }
        Ok(view_name)
    }

    pub(crate) fn add_array_buffer_view(&mut self) -> anyhow::Result<wit_encoder::Ident> {
        // TODO: maybe like any, only include that ones that were used.
        let buffer_view_name = wit_encoder::Ident::new("array-buffer-view");
        if !self.type_def_exists(&buffer_view_name) {
            let buffer_view = wit_encoder::TypeDef::variant(
                buffer_view_name.clone(),
                [
                    {
                        let view = self.add_data_view()?;
                        wit_encoder::VariantCase::value(
                            view.clone(),
                            wit_encoder::Type::named(view),
                        )
                    },
                    {
                        let array = self.add_typed_array(TypedArrayKind::UInt8)?;
                        wit_encoder::VariantCase::value(
                            array.clone(),
                            wit_encoder::Type::named(array),
                        )
                    },
                    {
                        let array = self.add_typed_array(TypedArrayKind::UInt8Clamped)?;
                        wit_encoder::VariantCase::value(
                            array.clone(),
                            wit_encoder::Type::named(array),
                        )
                    },
                    {
                        let array = self.add_typed_array(TypedArrayKind::UInt16)?;
                        wit_encoder::VariantCase::value(
                            array.clone(),
                            wit_encoder::Type::named(array),
                        )
                    },
                    {
                        let array = self.add_typed_array(TypedArrayKind::UInt32)?;
                        wit_encoder::VariantCase::value(
                            array.clone(),
                            wit_encoder::Type::named(array),
                        )
                    },
                    {
                        let array = self.add_typed_array(TypedArrayKind::Int8)?;
                        wit_encoder::VariantCase::value(
                            array.clone(),
                            wit_encoder::Type::named(array),
                        )
                    },
                    {
                        let array = self.add_typed_array(TypedArrayKind::Int16)?;
                        wit_encoder::VariantCase::value(
                            array.clone(),
                            wit_encoder::Type::named(array),
                        )
                    },
                    {
                        let array = self.add_typed_array(TypedArrayKind::Int32)?;
                        wit_encoder::VariantCase::value(
                            array.clone(),
                            wit_encoder::Type::named(array),
                        )
                    },
                    {
                        let array = self.add_typed_array(TypedArrayKind::Float32)?;
                        wit_encoder::VariantCase::value(
                            array.clone(),
                            wit_encoder::Type::named(array),
                        )
                    },
                    {
                        let array = self.add_typed_array(TypedArrayKind::Float64)?;
                        wit_encoder::VariantCase::value(
                            array.clone(),
                            wit_encoder::Type::named(array),
                        )
                    },
                ],
            );
            self.interface
                .items_mut()
                .push(wit_encoder::InterfaceItem::TypeDef(buffer_view));
        }
        Ok(buffer_view_name)
    }

    pub(crate) fn add_buffer_source(&mut self) -> anyhow::Result<wit_encoder::Ident> {
        // TODO: maybe like any, only include that ones that were used.
        // TODO: maybe have the actual variants of `array-buffer-view` here.
        let buffer_view_name = wit_encoder::Ident::new("buffer-source");
        if !self.type_def_exists(&buffer_view_name) {
            let buffer_view = wit_encoder::TypeDef::variant(
                buffer_view_name.clone(),
                [
                    {
                        let buffer = self.add_array_buffer()?;
                        wit_encoder::VariantCase::value(
                            buffer.clone(),
                            wit_encoder::Type::named(buffer),
                        )
                    },
                    {
                        let buffer_view = self.add_array_buffer_view()?;
                        wit_encoder::VariantCase::value(
                            buffer_view.clone(),
                            wit_encoder::Type::named(buffer_view),
                        )
                    },
                ],
            );
            self.interface
                .items_mut()
                .push(wit_encoder::InterfaceItem::TypeDef(buffer_view));
        }
        Ok(buffer_view_name)
    }

    pub(super) fn add_record<'b>(
        &mut self,
        record: &weedle::types::RecordType<'b>,
    ) -> anyhow::Result<wit_encoder::Ident> {
        let value = self.wi2w_type(&record.generics.body.2, false)?;

        let record_name = wit_encoder::Ident::new(format!("record-{value}"));
        if !self.type_def_exists(&record_name) {
            let set = wit_encoder::TypeDef::resource(
                record_name.clone(),
                [
                    { wit_encoder::ResourceFunc::constructor() },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("add");
                        func.set_params(wit_encoder::Params::from_iter([
                            ("key", wit_encoder::Type::String),
                            ("value", value.clone()),
                        ]));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("get");
                        func.set_params(("key", wit_encoder::Type::String));
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::option(
                            value.clone(),
                        )));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("has");
                        func.set_params(("key", wit_encoder::Type::String));
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::Bool));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("remove");
                        func.set_params(("key", wit_encoder::Type::String));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("keys");
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::list(
                            wit_encoder::Type::String,
                        )));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("values");
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::list(
                            value.clone(),
                        )));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("entries");
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::list(
                            wit_encoder::Type::tuple([wit_encoder::Type::String, value.clone()]),
                        )));
                        func
                    },
                ],
            );
            self.interface
                .items_mut()
                .push(wit_encoder::InterfaceItem::TypeDef(set));
        }
        Ok(record_name)
    }

    pub(super) fn add_object(&mut self) -> wit_encoder::Ident {
        let any = self.found_any();

        let object_name = wit_encoder::Ident::new(format!("object"));
        if !self.type_def_exists(&object_name) {
            let object = wit_encoder::TypeDef::resource(
                object_name.clone(),
                [
                    { wit_encoder::ResourceFunc::constructor() },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("add");
                        func.set_params(wit_encoder::Params::from_iter([
                            ("key", wit_encoder::Type::String),
                            ("value", wit_encoder::Type::named(any.clone())),
                        ]));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("get");
                        func.set_params(("key", wit_encoder::Type::String));
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::option(
                            wit_encoder::Type::named(any.clone()),
                        )));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("has");
                        func.set_params(("key", wit_encoder::Type::String));
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::Bool));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("remove");
                        func.set_params(("key", wit_encoder::Type::String));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("keys");
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::list(
                            wit_encoder::Type::String,
                        )));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("values");
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::list(
                            wit_encoder::Type::named(any.clone()),
                        )));
                        func
                    },
                    {
                        let mut func = wit_encoder::ResourceFunc::method("entries");
                        func.set_results(wit_encoder::Results::anon(wit_encoder::Type::list(
                            wit_encoder::Type::tuple([
                                wit_encoder::Type::String,
                                wit_encoder::Type::named(any.clone()),
                            ]),
                        )));
                        func
                    },
                ],
            );
            self.interface
                .items_mut()
                .push(wit_encoder::InterfaceItem::TypeDef(object));
        }
        object_name
    }

    pub(crate) fn add_allow_shared_buffer_source(&mut self) -> anyhow::Result<wit_encoder::Ident> {
        // TODO: maybe like any, only include that ones that were used.
        let allow_name = wit_encoder::Ident::new("allow-shared-buffer-source");
        if !self.type_def_exists(&allow_name) {
            let allow = wit_encoder::TypeDef::variant(
                allow_name.clone(),
                [
                    {
                        let name = self.add_array_buffer_view()?;
                        wit_encoder::VariantCase::value(
                            name.clone(),
                            wit_encoder::Type::named(name),
                        )
                    },
                    {
                        let name = self.add_array_buffer()?;
                        wit_encoder::VariantCase::value(
                            name.clone(),
                            wit_encoder::Type::named(name),
                        )
                    },
                    // {
                    //     let name = self.add_shared_array_buffer()?;
                    //     wit_encoder::VariantCase::value(
                    //         name.clone(),
                    //         wit_encoder::Type::named(name),
                    //     )
                    // },
                ],
            );
            self.interface
                .items_mut()
                .push(wit_encoder::InterfaceItem::TypeDef(allow));
        }

        Ok(allow_name)
    }

    fn type_def_exists(&self, name: &wit_encoder::Ident) -> bool {
        self.interface.items().iter().any(|item| match item {
            wit_encoder::InterfaceItem::TypeDef(td) => td.name() == name,
            wit_encoder::InterfaceItem::Function(_) => false,
        })
    }
}
