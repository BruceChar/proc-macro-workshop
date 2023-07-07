use proc_macro2::Ident;
use syn::{punctuated::Punctuated, Data, DeriveInput, Expr, ExprLit, Meta, MetaNameValue, Token};

use crate::types::{AttrType, FieldInfo, MetaType, TypeName, WrapAttr, WrapType};

pub fn parse_field_type_info<'a>(f: &'a syn::Field) -> WrapType {
    if let syn::Type::Path(syn::TypePath { ref path, .. }, ..) = f.ty {
        let parse_inner = |seg: &'a syn::PathSegment, tyn: TypeName| {
            if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                ref args,
                ..
            }) = seg.arguments
            {
                if let Some(syn::GenericArgument::Type(inner)) = args.first() {
                    return WrapType::new(&f.ty, Some(&inner), tyn);
                }
            }
            // TODO: parse error
            WrapType::new(&f.ty, None, TypeName::Whocares)
        };
        // the last segment of the type path,
        // e.g. std::option::Option -> Option
        if let Some(seg) = path.segments.last() {
            if seg.ident == "Option" {
                return parse_inner(seg, TypeName::Option);
            }
        }
        // Vec<T>
        if let Some(seg) = path.segments.first() {
            if seg.ident == "Vec" {
                return parse_inner(seg, TypeName::Vector);
            }
        }
    }
    WrapType::new(&f.ty, None, TypeName::Whocares)
}

pub fn parse_filed_attrs<'a>(f: &'a syn::Field) -> syn::Result<WrapAttr> {
    for attr in &f.attrs {
        if !attr.path().is_ident("builder") {
            return Err(syn::Error::new_spanned(attr.path(), "expected `builder(...)`"));
        }

        let nested = attr
            .parse_args_with(Punctuated::<Meta, Token!(,)>::parse_terminated)
            .expect("parse args failed");
        if let Some(Meta::NameValue(MetaNameValue {
            path,
            value:
                Expr::Lit(ExprLit {
                    lit: syn::Lit::Str(l),
                    ..
                }),
            ..
        })) = nested.first()
        {
            if path.is_ident("each") {
                return Ok(WrapAttr::new(
                    attr.path().get_ident(),
                    MetaType::NameValue("each", Ident::new(&l.value(), l.span())),
                    AttrType::Each,
                ));
            } else {
                return Err(syn::Error::new_spanned(&attr.meta, "expected `builder(each = \"...\")`"));
            }
        }
    }
    Ok(WrapAttr::default())
}

/// what if the field is Option
pub fn collect_named_struct_field_info(st: &DeriveInput) -> syn::Result<FieldInfo> {
    if let Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = st.data
    {
        let idents: Vec<_> = named.iter().map(|f| &f.ident).collect();
        let types: Vec<_> = named.iter().map(parse_field_type_info).collect();
        let mut attrs = vec![];
        for f in named.iter() {
            let wat = parse_filed_attrs(f)?;
            attrs.push(wat);
        }   

        return Ok(FieldInfo {
            idents,
            types,
            attrs,
        });
    }
    Err(syn::Error::new_spanned(st, "must define on a named struct").into())
}
