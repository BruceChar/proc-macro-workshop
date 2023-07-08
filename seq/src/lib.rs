use proc_macro::TokenStream;
use proc_macro2::{Literal, TokenTree};
use quote::{ToTokens, quote};
use syn::{parse::Parse, parse_macro_input};

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let st = parse_macro_input!(input as SeqParser);
    
    // eprintln!("body: {}", body);
    let mut ts = proc_macro2::TokenStream::new();
    for i in st.start..st.end {
        ts.extend(st.do_expand(&st.body, i));
    }
    ts.into()
}

struct SeqParser {
    n: syn::Ident,
    start: usize,
    end: usize,
    body: proc_macro2::TokenStream,
}

impl Parse for SeqParser {
    // seq!(N in 0..100 {...})
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let n = input.parse()?;

        // consume the token `in`
        input.parse::<syn::Token!(in)>()?;
        let start = input.parse::<syn::LitInt>()?;

        // consume the token `..`
        input.parse::<syn::Token!(..)>()?;

        let end: syn::LitInt = input.parse()?;

        // parse the body in brace `{}`
        let body_buf;
        syn::braced!(body_buf in input);

        let body = body_buf.parse()?;

        Ok(SeqParser {
            n,
            start: start.base10_parse()?,
            end: end.base10_parse()?,
            body,
        })
    }
}

impl SeqParser {
    fn do_expand(&self, ts: &proc_macro2::TokenStream, n: usize) -> proc_macro2::TokenStream {
        let mut buf = proc_macro2::TokenStream::new();
        let body = ts.clone().into_iter().collect::<Vec<_>>();
        for i in 0..body.len() {
            match &body[i] {
                // match the N and replace
                TokenTree::Ident(i) => {
                    if self.n.eq(i) {
                        let ln = proc_macro2::TokenTree::from(Literal::usize_unsuffixed(n));
                        buf.extend(ln.to_token_stream());
                    } else {
                        buf.extend(i.to_token_stream());
                    }
                }
                TokenTree::Group(g) => {
                    let s = self.do_expand(&g.stream(), n);
                    let new_grp = proc_macro2::Group::new(g.delimiter(), s);
                    buf.extend(new_grp.to_token_stream());
                },
                t =>{ 
                    // buf.extend(quote!(#t))
                    buf.extend(t.to_token_stream());
                },
            }

        }
        buf
    }
}