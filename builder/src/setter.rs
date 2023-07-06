use std::iter::zip;

use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::DeriveInput;

use crate::{FieldInfo, TypeName, WrapType};

/// Option type keep the origin and wrap the non-option
/// type T with std::option::Option<T>.
///
/// Vec type add the for-one setter, if the
/// setter attribute name(e.g. `arg`) is differ from
/// the field name.
/// ```rust
/// use derive_builder::Builder;
/// #[derive(Builder)]
/// struct Command {
///     // the attribute name is different
///     #[builder(each = "arg")]
///     args: Vec<String>,
///
///     // the name is same, for-all setter disable
///     #[builder(each = "env")]
///     env: Vec<String>
/// }
/// ```
pub fn generate_setters(fields: &FieldInfo) -> syn::Result<proc_macro2::TokenStream> {
    let FieldInfo {
        idents,
        types,
        attrs,
    } = fields;
    let mut ts: Vec<TokenStream> = vec![];
    for (ident, (WrapType { ty, inner, tyn }, _attr)) in idents.iter().zip(zip(types, attrs)) {
        let mut type_ident = ty;

        // if origin type is Option, keep it.
        if tyn.eq(&TypeName::Option) {
            type_ident = inner.as_ref().unwrap();
        };

        // Vec type
        if tyn.eq(&TypeName::Vector) {}

        ts.push(quote! {
            fn #ident(&mut self, #ident: #type_ident) -> &mut Self {
                self.#ident = std::option::Option::Some(#ident);
                self
            }
        });
    }
    Ok(quote! {
        #(#ts)*
    })
}


pub fn generate_build_function(
    st: &DeriveInput,
    idents: &Vec<&Option<Ident>>,
    types: &Vec<WrapType>,
) -> syn::Result<TokenStream> {
    let struct_ident = &st.ident;
    let mut expand = vec![];
    let mut checks = vec![];
    for (ident, WrapType { tyn, .. }) in idents.iter().zip(types) {
        let tmp = if tyn.eq(&TypeName::Option) {
            quote!(#ident: self.#ident.take())
        } else {
            checks.push(quote!{
                if self.#ident.is_none() {
                    return std::result::Result::Err(format!("field[{}] not be initialized", stringify!(#ident)).into())
                }
            });
            quote! {
                #ident: self.#ident.take().unwrap()
            }
        };
        expand.push(tmp);
    }
    let res = quote! {
        fn build(&mut self) -> std::result::Result<#struct_ident, std::boxed::Box<dyn std::error::Error>> {
            #(#checks)*

            std::result::Result::Ok(
                #struct_ident {
                    #(#expand),*
            })
        }
    };
    Ok(res)
}