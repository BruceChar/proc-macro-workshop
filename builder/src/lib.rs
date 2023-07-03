use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use proc_macro2::{Ident};
use syn::{parse_macro_input, DeriveInput, Data, spanned::Spanned, };

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let din = parse_macro_input!(input as DeriveInput);
    let ident = din.ident.to_string();
    let build_ident = Ident::new(&format!("{}Builder", ident), din.span());
    let ident = Ident::new(&ident, ident.span());
    let (idents, fields) = get_named_struct_fields(&din).expect("get named struct fields failed");
    let vis = din.vis.to_token_stream();

    let expand = quote!{
        #vis struct #build_ident {
            #fields
        }
        impl #ident {
            fn builder() -> #build_ident {
                #build_ident {
                    #(#idents: std::option::Option::None),*
                }
            }
        }
    };


    TokenStream::from(expand)
}

// type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token!(,)>;
fn get_named_struct_fields(input: &DeriveInput) -> syn::Result<(Vec<&std::option::Option<syn::Ident>>, proc_macro2::TokenStream)> {
    if let Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed {ref named, ..}),
        ..
    }) = input.data {
        let idents: Vec<_> = named.iter().map(|f| &f.ident).collect();
        let types: Vec<_> = named.iter().map(|f| &f.ty).collect();
        let ts = quote!{
            #(#idents: std::option::Option<#types>),*
        };
        return Ok((idents, ts));
    }
    Err(syn::Error::new_spanned(input, "Must define on a named Struct, not Enum"))
}
