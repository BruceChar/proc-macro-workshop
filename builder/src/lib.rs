use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{quote};
use syn::{parse_macro_input, spanned::Spanned, Data, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = parse_macro_input!(input as DeriveInput);
    let ident = st.ident.to_string();
    let build_ident = Ident::new(&format!("{}Builder", ident), st.span());

    let build_struct = generate_build_struct(&st).expect("failed");
    let setters = generate_setters(&st).expect("setter failed");
    let build = generate_build(&st).expect("build generate failed");
    let builder = generate_builder(&st).expect("builder generate failed");
    let expand = quote! {
        
        #build_struct

        #builder

        impl #build_ident {
            #setters

            #build
        }
    };

    TokenStream::from(expand)
}

type Result<T> = syn::Result<T>;

#[inline]
fn get_named_struct_fields(
    st: &DeriveInput,
) -> syn::Result<(Vec<&std::option::Option<syn::Ident>>, Vec<&syn::Type>)> {
    if let Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = st.data
    {
        let idents: Vec<_> = named.iter().map(|f| &f.ident).collect();
        let types: Vec<_> = named.iter().map(|f| &f.ty).collect();
        return Ok((idents, types));
    }
    Err(syn::Error::new_spanned(
        st,
        "Must define on a named Struct, not Enum",
    ).into())
}

fn generate_setters(input: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let (idents, types) = get_named_struct_fields(input)?;
    return Ok(quote! {
        #(fn #idents(&mut self, #idents: #types ) -> &mut Self {
            self.#idents = std::option::Option::Some(#idents);
            self
        })*
    });
}

fn generate_build_struct(st: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let (idents, types) = get_named_struct_fields(st)?;
    let build_ident = Ident::new(&format!("{}Builder", st.ident.to_string()), st.span());
    let vis = &st.vis;
    return Ok(quote! {
        #vis struct #build_ident {
            #(#idents: std::option::Option<#types>),*
        }
    });
}

fn generate_builder(st: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let (idents, _types) = get_named_struct_fields(st)?;
    let ident = st.ident.to_string();
    let ident = Ident::new(&ident, st.span());
    let build_ident = Ident::new(&format!("{}Builder", st.ident.to_string()), st.span());
    let expand = quote!{
        impl #ident {
            fn builder() -> #build_ident {
                #build_ident {
                    #(#idents: std::option::Option::None),*
                }
            }
        }
    };
    return Ok(expand)
}

fn generate_build(st: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let (idents, _types) = get_named_struct_fields(st)?;
    let ident = &st.ident;
    let expand = quote!{
        fn build(self) -> std::result::Result<#ident, std::boxed::Box<dyn std::error::Error>> {
            std::result::Result::Ok(
                #ident {
                    #(#idents: self.#idents.expect("should be initialized")),*
            })
        }
    };
    return Ok(expand)
}