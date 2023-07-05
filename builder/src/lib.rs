use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, Data, DeriveInput};


struct FieldInfo<'a> {
    idents: Vec<&'a Option<syn::Ident>>,
    types: Vec<WrapType<'a>>,
    attrs: Vec<&'a Vec<syn::Attribute>>,
}
struct WrapType<'a>(&'a syn::Type, InnerType);

#[derive(PartialEq, Eq)]
enum InnerType {
    Option,
    Vec,
    Whocares,
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = parse_macro_input!(input as DeriveInput);
    eprintln!("{:?}", st.attrs);

    let ident = st.ident.to_string();
    let build_ident = Ident::new(&format!("{}Builder", ident), st.span());
    let FieldInfo {
        idents,
        types,
        ..
    } = get_named_struct_fields(&st).expect("get named fields failed");
    let build_struct =
        generate_build_struct(&st, &idents, &types).expect("generate build struct failed");
    let setters = generate_setters(&idents, &types).expect("generate setter failed");
    let build = generate_build(&st, &idents, &types).expect("generate build function failed");
    let builder = generate_builder(&st, &idents).expect("generate builder function failed");
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
fn get_named_struct_fields(
    st: &DeriveInput,
) -> syn::Result<FieldInfo> {
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
                                    return WrapType(&inner, InnerType::Option);
                                }
                            }
                        }
                    }
                }
                return WrapType(&f.ty, InnerType::Whocares);
            })
            .collect();
        let attrs: Vec<_> = named.iter().map(|f| &f.attrs).collect();
        return Ok(FieldInfo {
            idents,
            types,
            attrs
        });
    }
    Err(syn::Error::new_spanned(st, "Must define on a named Struct, not Enum").into())
}

fn generate_setters(
    idents: &Vec<&std::option::Option<Ident>>,
    types: &Vec<WrapType>,
) -> Result<proc_macro2::TokenStream> {
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

fn generate_builder(
    st: &DeriveInput,
    idents: &Vec<&std::option::Option<Ident>>,
) -> Result<proc_macro2::TokenStream> {
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
    let mut checks = vec![];
    for (ident, WrapType(_ty, inner)) in idents.iter().zip(types) {
        let tmp = if inner.eq(&InnerType::Option) {
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

fn get_named_struct_field_attributes(st: &DeriveInput) {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = st.data
    {
        let attrs: Vec<_> = named
            .iter()
            .map(
                |f| {
                    eprintln!("inner attrs:\n {:?}", f.attrs);
                    if let Some(syn::Attribute { style, meta, .. }) = f.attrs.last() {
                        
                    }
                },
            )
            .collect();
    }
}
