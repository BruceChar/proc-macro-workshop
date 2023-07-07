use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, DeriveInput};

mod builder;
mod parse;
mod setter;
mod types;

use builder::*;
use parse::*;
use setter::*;
use types::*;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = parse_macro_input!(input as DeriveInput);
    match do_expand(&st) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into()
    }
}

fn do_expand(st: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
   
    let ident = st.ident.to_string();
    let build_ident = Ident::new(&format!("{}Builder", ident), st.span());

    let fi = collect_named_struct_field_info(&st)?;
    let builder_struct =
        generate_builder_struct(&st, &fi.idents, &fi.types)?;
    let builder_func =
        generate_builder_function(&st, &fi.idents)?;

    let setters = generate_setters(&fi)?;
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
    Ok(ts)
}
