use syn::{DeriveInput, Data};

use crate::types::{WrapType, TypeName, FieldInfo};

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
            eprintln!("no inner type: {:?}", seg);
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
    eprintln!("out no inner type: {:?}", f.ty);

    WrapType::new(&f.ty, None, TypeName::Whocares)
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
        let attrs: Vec<_> = named.iter().map(|f| &f.attrs).collect();
        return Ok(FieldInfo {
            idents,
            types,
            attrs,
        });
    }
    Err(syn::Error::new_spanned(st, "must define on a named struct").into())
}