use proc_macro::TokenStream;
use proc_macro2::{Ident, Literal, TokenTree};
use quote::{quote, ToTokens};
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
        let mut id = 0;
        let blen = body.len();
        while id < blen {
            match &body[id] {
                // match the N and replace
                TokenTree::Ident(i) => {
                    // match the `xxx#N`
                    // the `xxx #N`, `xxx # N`, `xxx# N` is parsed the same
                    // as `xxx#N`.So we need to check the span
                    if id + 2 < blen {
                        if let TokenTree::Punct(p) = &body[id + 1] {
                            if i.span().end() == p.span().start() && p.as_char().eq(&'~') {
                                if let TokenTree::Ident(i2) = &body[id + 2] {
                                    if i2.span().start() == p.span().end() && i2.eq(&self.n) {
                                        let ni = Ident::new(&format!("{}{}", i, n), i.span());
                                        buf.extend(quote!(#ni));
                                        id += 3;
                                        continue;
                                    }
                                }
                            }
                        }
                    }
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

                }
                t => {
                    // buf.extend(quote!(#t))
                    buf.extend(t.to_token_stream());
                }
            }
            id += 1;
        }
        buf
    }
}
