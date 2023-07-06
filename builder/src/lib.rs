use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, DeriveInput};

mod parse;
mod setter;
mod types;
mod builder;

use parse::*;
use setter::*;
use types::*;
use builder::*;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = parse_macro_input!(input as DeriveInput);

    let ident = st.ident.to_string();
    let build_ident = Ident::new(&format!("{}Builder", ident), st.span());

    let fi = collect_named_struct_field_info(&st).expect("collect named struct field info failed");
    let builder_struct =
        generate_builder_struct(&st, &fi.idents, &fi.types).expect("generate build struct failed");
    let builder_func =
        generate_builder_function(&st, &fi.idents).expect("generate builder function failed");

    let setters = generate_setters(&fi).expect("generate setter failed");
    let build_func = generate_build_function(&st, &fi.idents, &fi.types)
        .expect("generate build function failed");
    let ts = quote! {

        #builder_struct

        #builder_func

        impl #build_ident {
            #setters
            #build_func
        }
    };

    TokenStream::from(ts)
}

// fn get_named_struct_field_attributes(st: &DeriveInput) {
//     if let syn::Data::Struct(syn::DataStruct {
//         fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
//         ..
//     }) = st.data
//     {
//         let attrs: Vec<_> = named
//             .iter()
//             .map(|f| {
//                 eprintln!("inner attrs:\n {:?}", f.attrs);
//                 if let Some(syn::Attribute { style, meta, .. }) = f.attrs.last() {}
//             })
//             .collect();
//     }
// }
