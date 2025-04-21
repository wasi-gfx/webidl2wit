use std::collections::HashSet;

use wit_encoder::{Ident, Interface, InterfaceItem, Params, Type, TypeDef, TypeDefKind};

pub fn params_resources_borrow(mut interface: &mut Interface, resource_names: &HashSet<Ident>) {
    let param_types =
        get_all_func_param_types_with_resource(interface, resource_names, &mut Default::default());
    let result_types =
        get_all_func_return_types_with_resource(interface, resource_names, &mut Default::default());

    let mut overlaps = Vec::new();
    let mut only_param = HashSet::new();
    for name in param_types {
        match result_types.contains(&name) {
            true => {
                overlaps.push(name);
            }
            false => {
                only_param.insert(name);
            }
        }
    }

    let overlaps_set = overlaps.iter().collect();
    for name in &overlaps {
        copy_type_def_to_owned(interface, &name, &overlaps_set);
        make_type_def_borrow(interface, &name, resource_names);
    }
    replace_all_named_returns_with_owned(&mut interface, &overlaps.iter().collect());

    for name in only_param {
        make_type_def_borrow(interface, &name, resource_names);
    }

    change_func_resource_params_to_borrow(interface, resource_names);
}

fn name_to_owned(name: &Ident) -> Ident {
    Ident::new(format!("{}-owned", name.raw_name()))
}

fn get_all_func_param_types_with_resource<'a>(
    interface: &'a Interface,
    resource_names: &HashSet<Ident>,
    checked: &mut HashSet<&'a Ident>,
) -> Vec<Ident> {
    interface
        .items()
        .iter()
        .flat_map(|item| match item {
            InterfaceItem::Function(func) => func
                .params()
                .items()
                .iter()
                .flat_map(|(_, type_)| {
                    get_all_named_with_resources_from_type(
                        type_,
                        interface,
                        resource_names,
                        checked,
                    )
                })
                .collect::<Vec<_>>(),
            InterfaceItem::TypeDef(type_def) => {
                if let TypeDefKind::Resource(resource) = type_def.kind() {
                    resource
                        .funcs()
                        .iter()
                        .map(|func| func.params())
                        .flat_map(|params| {
                            params
                                .items()
                                .iter()
                                .flat_map(|(_, type_)| {
                                    get_all_named_with_resources_from_type(
                                        type_,
                                        interface,
                                        resource_names,
                                        checked,
                                    )
                                })
                                .collect::<Vec<_>>()
                        })
                        .collect()
                } else {
                    vec![]
                }
            }
        })
        .collect()
}

fn get_all_func_return_types_with_resource<'a>(
    interface: &'a Interface,
    resource_names: &HashSet<Ident>,
    checked: &mut HashSet<&'a Ident>,
) -> HashSet<Ident> {
    interface
        .items()
        .iter()
        .flat_map(|item| match item {
            InterfaceItem::Function(func) => func
                .result()
                .as_ref()
                .map(|type_| {
                    get_all_named_with_resources_from_type(
                        type_,
                        interface,
                        resource_names,
                        checked,
                    )
                })
                .unwrap_or_default(),
            InterfaceItem::TypeDef(type_def) => {
                if let TypeDefKind::Resource(resource) = type_def.kind() {
                    resource
                        .funcs()
                        .iter()
                        .map(|func| func.result())
                        .flat_map(|result| match result {
                            Some(result) => result
                                .as_ref()
                                .map(|type_| {
                                    get_all_named_with_resources_from_type(
                                        type_,
                                        interface,
                                        resource_names,
                                        checked,
                                    )
                                })
                                .unwrap_or_default(),
                            None => vec![],
                        })
                        .collect()
                } else {
                    vec![]
                }
            }
        })
        .collect()
}

fn get_all_named_with_resources_from_type<'a>(
    type_: &'a Type,
    interface: &'a Interface,
    resource_names: &HashSet<Ident>,
    checked: &mut HashSet<&'a Ident>,
) -> Vec<Ident> {
    fn get_named_type<'a>(type_: &'a Type) -> Vec<&'a Ident> {
        match type_ {
            Type::Bool
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::S8
            | Type::S16
            | Type::S32
            | Type::S64
            | Type::F32
            | Type::F64
            | Type::Char
            | Type::String => vec![],
            Type::Borrow(_) => {
                // no one is adding borrows before this point
                unreachable!()
            }
            Type::Option(type_) => get_named_type(type_),
            Type::Result(_result) => todo!(),
            Type::List(type_) => get_named_type(type_),
            Type::Tuple(tuple) => tuple
                .types()
                .iter()
                .map(|type_| get_named_type(type_))
                .flatten()
                .collect(),
            Type::Named(ident) => {
                vec![ident]
            }
            Type::Future(_) => todo!(),
            Type::Stream(_) => todo!(),
            Type::ErrorContext => todo!(),
        }
    }
    fn get_all_named_with_resources_from_type_def<'a>(
        name: &'a Ident,
        interface: &'a Interface,
        resource_names: &HashSet<Ident>,
        checked: &mut HashSet<&'a Ident>,
    ) -> Vec<Ident> {
        // TODO: remove once we nave native async support
        if name.raw_name() == "pollable" {
            return vec![];
        }
        if checked.contains(name) {
            return vec![];
        }
        checked.insert(name);
        let type_def = interface
            .items()
            .into_iter()
            .find_map(|item| match item {
                InterfaceItem::Function(_) => None,
                InterfaceItem::TypeDef(type_def) => {
                    if type_def.name() == name {
                        Some(type_def)
                    } else {
                        None
                    }
                }
            })
            .expect(&format!("Can't find type {name}"));

        let mut contains_resource_directly = false;
        let mut output = vec![];
        match type_def.kind() {
            TypeDefKind::Resource(_) | TypeDefKind::Flags(_) | TypeDefKind::Enum(_) => {}
            TypeDefKind::Record(record) => {
                for field in record.fields() {
                    for ident in get_named_type(field.type_()) {
                        if resource_names.contains(ident) {
                            contains_resource_directly = true;
                        }
                    }
                    output.append(&mut get_all_named_with_resources_from_type(
                        field.type_(),
                        interface,
                        resource_names,
                        checked,
                    ));
                }
            }
            TypeDefKind::Variant(variant) => {
                for case in variant.cases() {
                    if let Some(type_) = case.type_() {
                        for ident in get_named_type(type_) {
                            if resource_names.contains(ident) {
                                contains_resource_directly = true;
                            }
                        }
                        output.append(&mut get_all_named_with_resources_from_type(
                            type_,
                            interface,
                            resource_names,
                            checked,
                        ));
                    }
                }
            }
            TypeDefKind::Type(type_) => {
                for ident in get_named_type(type_) {
                    if resource_names.contains(ident) {
                        contains_resource_directly = true;
                    }
                }
                output.append(&mut get_all_named_with_resources_from_type(
                    type_,
                    interface,
                    resource_names,
                    checked,
                ));
            }
        }
        if output.len() > 0 || contains_resource_directly {
            output.push(name.clone());
        }
        output
    }

    match type_ {
        Type::Bool
        | Type::U8
        | Type::U16
        | Type::U32
        | Type::U64
        | Type::S8
        | Type::S16
        | Type::S32
        | Type::S64
        | Type::F32
        | Type::F64
        | Type::Char
        | Type::String => vec![],
        Type::Borrow(_) => {
            // Only one adding borrows before this point is custom_resource.rs. It takes care of borrowing on itself
            vec![]
        }
        Type::Option(type_) => {
            get_all_named_with_resources_from_type(type_, interface, resource_names, checked)
        }
        Type::Result(_result) => todo!(),
        Type::List(type_) => {
            get_all_named_with_resources_from_type(type_, interface, resource_names, checked)
        }
        Type::Tuple(tuple) => tuple
            .types()
            .iter()
            .map(|type_| {
                get_all_named_with_resources_from_type(type_, interface, resource_names, checked)
            })
            .flatten()
            .collect(),
        Type::Named(ident) => {
            get_all_named_with_resources_from_type_def(ident, interface, resource_names, checked)
        }
        Type::Future(_) => todo!(),
        Type::Stream(_) => todo!(),
        Type::ErrorContext => todo!(),
    }
}

fn make_type_def_borrow(interface: &mut Interface, name: &Ident, resource_names: &HashSet<Ident>) {
    fn make_type_borrow(type_: &mut Type, resource_names: &HashSet<Ident>) {
        match type_ {
            Type::Bool
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::S8
            | Type::S16
            | Type::S32
            | Type::S64
            | Type::F32
            | Type::F64
            | Type::Char
            | Type::String => {}
            Type::Borrow(_) => {
                // no one is adding borrows before this point
                unreachable!()
            }
            Type::Option(type_) => make_type_borrow(type_, resource_names),
            Type::Result(_result) => todo!(),
            Type::List(type_) => make_type_borrow(type_, resource_names),
            Type::Tuple(tuple) => {
                for type_ in tuple.types_mut() {
                    make_type_borrow(type_, resource_names);
                }
            }
            Type::Named(ident) => {
                if resource_names.contains(ident) {
                    *type_ = Type::borrow(ident.clone());
                }
            }
            Type::Future(_) => todo!(),
            Type::Stream(_) => todo!(),
            Type::ErrorContext => todo!(),
        }
    }

    let type_def = interface
        .items_mut()
        .into_iter()
        .find_map(|item| match item {
            InterfaceItem::Function(_) => None,
            InterfaceItem::TypeDef(type_def) => {
                if type_def.name() == name {
                    Some(type_def)
                } else {
                    None
                }
            }
        })
        .expect(&format!("Can't find type {name}"));
    match type_def.kind_mut() {
        TypeDefKind::Resource(_) | TypeDefKind::Flags(_) | TypeDefKind::Enum(_) => {}
        TypeDefKind::Record(record) => {
            for field in record.fields_mut() {
                make_type_borrow(field.type_mut(), resource_names);
            }
        }
        TypeDefKind::Variant(variant) => {
            for case in variant.cases_mut() {
                if let Some(type_) = case.type_mut() {
                    make_type_borrow(type_, resource_names);
                }
            }
        }
        TypeDefKind::Type(type_) => {
            make_type_borrow(type_, resource_names);
        }
    }
}

fn copy_type_def_to_owned(
    interface: &mut Interface,
    name: &Ident,
    types_being_copied: &HashSet<&Ident>,
) {
    fn copy_type_to_owned(type_: &Type, types_being_copied: &HashSet<&Ident>) -> Type {
        match type_ {
            Type::Bool
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::S8
            | Type::S16
            | Type::S32
            | Type::S64
            | Type::F32
            | Type::F64
            | Type::Char
            | Type::String => type_.clone(),
            Type::Borrow(_) => {
                // no one is adding borrows before this point
                unreachable!();
            }
            Type::Option(type_) => Type::option(copy_type_to_owned(type_, types_being_copied)),
            Type::Result(_result) => todo!(),
            Type::List(type_) => Type::list(copy_type_to_owned(type_, types_being_copied)),
            Type::Tuple(_tuple) => {
                todo!()
            }
            Type::Named(name) => {
                // if type is being copied to -owned, use the -owned version
                if types_being_copied.contains(name) {
                    Type::named(name_to_owned(name))
                } else {
                    type_.clone()
                }
            }
            Type::Future(_) => todo!(),
            Type::Stream(_) => todo!(),
            Type::ErrorContext => todo!(),
        }
    }

    let type_def = interface
        .items()
        .into_iter()
        .find_map(|item| match item {
            InterfaceItem::Function(_) => None,
            InterfaceItem::TypeDef(type_def) => {
                if type_def.name() == name {
                    Some(type_def)
                } else {
                    None
                }
            }
        })
        .expect(&format!("Can't find type {name}"));
    let new_type_kind = match type_def.kind() {
        TypeDefKind::Resource(_) | TypeDefKind::Flags(_) | TypeDefKind::Enum(_) => unreachable!(),
        TypeDefKind::Record(record) => {
            let fields = record.fields().iter().map(|f| {
                let mut f = f.clone();
                *f.type_mut() = copy_type_to_owned(f.type_(), types_being_copied);
                f
            });

            TypeDefKind::record(fields)
        }
        TypeDefKind::Variant(variant) => {
            let cases = variant.cases().iter().map(|case| {
                let mut case = case.clone();
                if let Some(type_) = case.type_mut() {
                    *type_ = copy_type_to_owned(type_, types_being_copied);
                }
                case
            });
            TypeDefKind::variant(cases)
        }
        TypeDefKind::Type(type_) => {
            TypeDefKind::type_(copy_type_to_owned(type_, types_being_copied))
        }
    };
    let name = name_to_owned(name);
    let type_def = TypeDef::new(name, new_type_kind);
    interface.type_def(type_def);
}

fn replace_all_named_returns_with_owned(interface: &mut Interface, to_rename: &HashSet<&Ident>) {
    fn postfix_named_type(type_: &mut Type, to_rename: &HashSet<&Ident>) {
        match type_ {
            Type::Bool
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::S8
            | Type::S16
            | Type::S32
            | Type::S64
            | Type::F32
            | Type::F64
            | Type::Char
            | Type::String => {}
            Type::Borrow(_) => unreachable!(),
            Type::Option(type_) => {
                postfix_named_type(type_, to_rename);
            }
            Type::Result(_result) => todo!(),
            Type::List(type_) => {
                postfix_named_type(type_, to_rename);
            }
            Type::Tuple(tuple) => {
                for type_ in tuple.types_mut() {
                    postfix_named_type(type_, to_rename);
                }
            }
            Type::Named(ident) => {
                if to_rename.contains(ident) {
                    *ident = name_to_owned(ident);
                }
            }
            Type::Future(_) => todo!(),
            Type::Stream(_) => todo!(),
            Type::ErrorContext => todo!(),
        }
    }
    fn replace_func_returns_with_owned(type_: &mut Option<Type>, to_rename: &HashSet<&Ident>) {
        if let Some(type_) = type_ {
            postfix_named_type(type_, to_rename);
        }
    }

    for item in interface.items_mut() {
        match item {
            InterfaceItem::Function(func) => {
                replace_func_returns_with_owned(func.result_mut(), to_rename);
            }
            InterfaceItem::TypeDef(type_def) => {
                if let TypeDefKind::Resource(resource) = type_def.kind_mut() {
                    for func in resource.funcs_mut() {
                        if let Some(results) = func.result_mut() {
                            replace_func_returns_with_owned(results, to_rename);
                        }
                    }
                }
            }
        }
    }
}

fn change_func_resource_params_to_borrow(
    interface: &mut Interface,
    resource_names: &HashSet<Ident>,
) {
    fn named_to_borrow(type_: &mut Type, resource_names: &HashSet<Ident>) {
        match type_ {
            Type::Named(ident) => {
                if resource_names.contains(&ident) {
                    *type_ = Type::borrow(ident.clone())
                }
            }
            Type::Bool
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::S8
            | Type::S16
            | Type::S32
            | Type::S64
            | Type::F32
            | Type::F64
            | Type::Char
            | Type::String
            | Type::Borrow(_) => {}
            Type::Option(type_) => named_to_borrow(type_, resource_names),
            Type::Result(_result) => todo!(),
            Type::List(type_) => named_to_borrow(type_, resource_names),
            Type::Tuple(_tuple) => todo!(),
            Type::Future(_) => todo!(),
            Type::Stream(_) => todo!(),
            Type::ErrorContext => todo!(),
        }
    }
    fn named_to_borrow_params(params: &mut Params, resource_names: &HashSet<Ident>) {
        for (_, type_) in params.items_mut() {
            named_to_borrow(type_, resource_names);
        }
    }

    for item in interface.items_mut() {
        match item {
            InterfaceItem::Function(func) => {
                named_to_borrow_params(func.params_mut(), resource_names);
            }
            InterfaceItem::TypeDef(type_def) => {
                if let TypeDefKind::Resource(resource) = type_def.kind_mut() {
                    for func in resource.funcs_mut() {
                        named_to_borrow_params(func.params_mut(), resource_names);
                    }
                }
            }
        }
    }
}
