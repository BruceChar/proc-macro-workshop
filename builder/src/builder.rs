use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::{spanned::Spanned, DeriveInput};

use crate::types::{TypeName, WrapType};

/// if the field itself is a Option<T> type,
/// we don't need wrap Option again.
pub fn generate_builder_struct(
    st: &DeriveInput,
    idents: &Vec<&Option<Ident>>,
    types: &Vec<WrapType>,
) -> syn::Result<TokenStream> {
    let build_ident = Ident::new(&format!("{}Builder", st.ident.to_string()), st.span());
    let vis = &st.vis;

    let mut expand: Vec<TokenStream> = vec![];
    for (ident, WrapType { ty, tyn, .. }) in idents.iter().zip(types) {
        let tmp = if tyn.eq(&TypeName::Option) {
            quote!(#ident: #ty)
        } else {
            quote!(#ident: std::option::Option<#ty>)
        };
        expand.push(tmp);
    }
    return Ok(quote! {
        #vis struct #build_ident {
            #(#expand),*
        }
    });
}

pub fn generate_builder_function(
    st: &DeriveInput,
    idents: &Vec<&Option<Ident>>,
) -> syn::Result<TokenStream> {
    let ident = st.ident.to_string();
    let ident = Ident::new(&ident, st.span());
    let build_ident = Ident::new(&format!("{}Builder", st.ident.to_string()), st.span());
    let ts = quote! {
        impl #ident {
            fn builder() -> #build_ident {
                #build_ident {
                    #(#idents: std::option::Option::None),*
                }
            }
        }
    };
    return Ok(ts);
}
