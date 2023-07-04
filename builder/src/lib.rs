use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, spanned::Spanned, Data, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = parse_macro_input!(input as DeriveInput);
    let ident = st.ident.to_string();
    let build_ident = Ident::new(&format!("{}Builder", ident), st.span());
    let (idents, types) = get_named_struct_fields(&st).expect("get named fields failed");
    let build_struct = generate_build_struct(&st, &idents, &types).expect("failed");
    let setters = generate_setters(&st).expect("setter failed");
    let build = generate_build(&st, &idents, &types).expect("build generate failed");
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

/// what if the field is Option
#[inline]
fn get_named_struct_fields(
    st: &DeriveInput,
) -> syn::Result<(Vec<&std::option::Option<syn::Ident>>, Vec<WrapType>)> {
    if let Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = st.data
    {
        let idents: Vec<_> = named.iter().map(|f| &f.ident).collect();
        let types: Vec<_> = named
            .iter()
            .map(|f| {
                if let syn::Type::Path(syn::TypePath { path, .. }, ..) = &f.ty {
                    // the last segment of the type path,
                    // e.g. std::option::Option -> Option
                    if let Some(seg) = path.segments.last() {
                        if seg.ident == "Option" {
                            // get the inner generic type
                            if let syn::PathArguments::AngleBracketed(
                                syn::AngleBracketedGenericArguments { ref args, .. },
                            ) = seg.arguments
                            {
                                if let Some(syn::GenericArgument::Type(inner)) = args.first() {
                                    return WrapType(&inner, true);
                                }
                            }
                        }
                    }
                }
                return WrapType(&f.ty, false);
            })
            .collect();
        return Ok((idents, types));
    }
    Err(syn::Error::new_spanned(st, "Must define on a named Struct, not Enum").into())
}

fn generate_setters(input: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let (idents, types) = get_named_struct_fields(input)?;
    let mut expand: Vec<proc_macro2::TokenStream> = vec![];
    for (ident, WrapType(ty, _is_option)) in idents.iter().zip(types) {
        let tmp = quote! {
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = std::option::Option::Some(#ident);
                self
            }
        };
        expand.push(tmp);
    }
    Ok(quote! {
        #(#expand)*
    })
}

struct WrapType<'a>(&'a syn::Type, bool);

impl<'a> ToTokens for WrapType<'a> {
    fn into_token_stream(self) -> proc_macro2::TokenStream
    where
        Self: Sized,
    {
        self.0.to_token_stream()
    }

    fn to_token_stream(&self) -> proc_macro2::TokenStream {
        self.0.to_token_stream()
    }

    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.extend(self.0.to_token_stream())
    }
}

/// if the field itself is a Option<T> type,
/// we don't need wrap Option again.
fn generate_build_struct(
    st: &DeriveInput,
    idents: &Vec<&std::option::Option<Ident>>,
    types: &Vec<WrapType>,
) -> Result<proc_macro2::TokenStream> {
    let build_ident = Ident::new(&format!("{}Builder", st.ident.to_string()), st.span());
    let vis = &st.vis;

    let mut expand: Vec<proc_macro2::TokenStream> = vec![];
    for (ident, WrapType(ty, _is_option)) in idents.iter().zip(types) {
        let tmp = quote!(#ident: std::option::Option<#ty>);
        expand.push(tmp);
    }
    return Ok(quote! {
        #vis struct #build_ident {
            #(#expand),*
        }
    });
}

fn generate_builder(st: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let (idents, _types) = get_named_struct_fields(st)?;
    let ident = st.ident.to_string();
    let ident = Ident::new(&ident, st.span());
    let build_ident = Ident::new(&format!("{}Builder", st.ident.to_string()), st.span());
    let expand = quote! {
        impl #ident {
            fn builder() -> #build_ident {
                #build_ident {
                    #(#idents: std::option::Option::None),*
                }
            }
        }
    };
    return Ok(expand);
}

fn generate_build(
    st: &DeriveInput,
    idents: &Vec<&std::option::Option<Ident>>,
    types: &Vec<WrapType>,
) -> Result<proc_macro2::TokenStream> {
    let struct_ident = &st.ident;
    let mut expand = vec![];
    for (ident, WrapType(_ty, is_option)) in idents.iter().zip(types) {
        let tmp = if *is_option {
            quote!(#ident: self.#ident.take())
        } else {
            quote!{
                #ident: self.#ident.take().expect(&format!("field[{}] not be initialized", stringify!(#ident)))
            }
        };
        expand.push(tmp);
    }
    let res = quote! {
        fn build(&mut self) -> std::result::Result<#struct_ident, std::boxed::Box<dyn std::error::Error>> {
            std::result::Result::Ok(
                #struct_ident {
                    #(#expand),*
            })
        }
    };
    Ok(res)
}
