use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, Data, DeriveInput};
struct FieldInfo<'a> {
    idents: Vec<&'a Option<syn::Ident>>,
    types: Vec<WrapType<'a>>,
    attrs: Vec<&'a Vec<syn::Attribute>>,
}
struct WrapType<'a> {
    /// origin syn Type
    ty: &'a syn::Type,

    /// inner Type
    /// may be something we don't care
    inner: Option<&'a syn::Type>,

    /// origin syn Type name
    /// for now we do care Option & Vec
    tyn: TypeName,
}

impl<'a> WrapType<'a> {
    fn new(ty: &'a syn::Type, inner: Option<&'a syn::Type>, tyn: TypeName) -> Self {
        WrapType { ty, inner, tyn }
    }
}

#[derive(PartialEq, Eq)]
enum TypeName {
    Option,
    Vector,
    Whocares,
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = parse_macro_input!(input as DeriveInput);
    eprintln!("{:?}", st.attrs);

    let ident = st.ident.to_string();
    let build_ident = Ident::new(&format!("{}Builder", ident), st.span());
    let FieldInfo { idents, types, .. } =
        collect_named_struct_field_info(&st).expect("collect named struct field info failed");
    let builder_struct =
        generate_builder_struct(&st, &idents, &types).expect("generate build struct failed");
    let setters = generate_setters(&idents, &types).expect("generate setter failed");
    let build = generate_build(&st, &idents, &types).expect("generate build function failed");
    let builder_func =
        generate_builder_function(&st, &idents).expect("generate builder function failed");
    let expand = quote! {

        #builder_struct

        #builder_func

        impl #build_ident {
            #setters
            #build
        }
    };

    TokenStream::from(expand)
}

type Result<T> = syn::Result<T>;

fn parse_field_type_info<'a>(f: &'a syn::Field) -> WrapType {
    if let syn::Type::Path(syn::TypePath { ref path, .. }, ..) = f.ty {
        eprintln!("path: {:?}", path);
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
            WrapType::new(&f.ty, None, TypeName::Whocares)
        };
        // the last segment of the type path,
        // e.g. std::option::Option -> Option
        if let Some(seg) = path.segments.last() {
            if seg.ident == "Option" {
                return parse_inner(seg, TypeName::Option);
                // // get the inner generic type
                // if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                //     ref args,
                //     ..
                // }) = seg.arguments
                // {
                //     if let Some(syn::GenericArgument::Type(inner)) = args.first() {
                //         return WrapType(&inner, TypeName::Option);
                //     }
                // }
            }
        }
        // Vec<T>
        if let Some(seg) = path.segments.first() {
            if seg.ident == "Vec" {
                eprintln!("Vec type: {:?}", seg);
                return parse_inner(seg, TypeName::Vector);
            }
        }
    }
    WrapType::new(&f.ty, None, TypeName::Whocares)
}

/// what if the field is Option
fn collect_named_struct_field_info(st: &DeriveInput) -> syn::Result<FieldInfo> {
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
    Err(syn::Error::new_spanned(st, "Must define on a named Struct, not Enum").into())
}

fn generate_setters(
    idents: &Vec<&std::option::Option<Ident>>,
    types: &Vec<WrapType>,
) -> Result<proc_macro2::TokenStream> {
    let mut expand: Vec<proc_macro2::TokenStream> = vec![];
    for (ident, WrapType { ty, inner, tyn }) in idents.iter().zip(types) {
        let tmp = if tyn.eq(&TypeName::Option) {
            quote! {
                fn #ident(&mut self, #ident: #inner) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            }
        } else {
            quote! {
                fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
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
fn generate_builder_struct(
    st: &DeriveInput,
    idents: &Vec<&std::option::Option<Ident>>,
    types: &Vec<WrapType>,
) -> Result<proc_macro2::TokenStream> {
    let build_ident = Ident::new(&format!("{}Builder", st.ident.to_string()), st.span());
    let vis = &st.vis;

    let mut expand: Vec<proc_macro2::TokenStream> = vec![];
    for (ident, WrapType { ty, inner, tyn }) in idents.iter().zip(types) {
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

fn generate_builder_function(
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

fn get_named_struct_field_attributes(st: &DeriveInput) {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = st.data
    {
        let attrs: Vec<_> = named
            .iter()
            .map(|f| {
                eprintln!("inner attrs:\n {:?}", f.attrs);
                if let Some(syn::Attribute { style, meta, .. }) = f.attrs.last() {}
            })
            .collect();
    }
}
