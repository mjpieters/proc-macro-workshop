use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::parse::Parse;
use syn::{braced, parse_macro_input, Ident, LitInt, Token};

#[derive(Debug)]
enum SeqPart {
    TokenStream(Vec<proc_macro2::TokenTree>),
    Group(proc_macro2::Delimiter, proc_macro2::Span, Vec<SeqPart>),
    Placeholder(proc_macro2::Span, Option<Ident>),
    Repeated(Vec<SeqPart>),
}

impl SeqPart {
    fn from_stream<TT: Iterator<Item = proc_macro2::TokenTree>>(
        placeholder: &Ident,
        mut it: TT,
    ) -> Vec<Self> {
        let mut seq = Vec::new();
        let mut stream = Vec::new();
        while let Some(tree) = it.next() {
            use proc_macro2::TokenTree::*;
            match tree {
                Ident(ref ident) if ident == placeholder => {
                    let mut prefix = None;
                    let mut span = ident.span();
                    if stream.len() > 1
                        && matches!(&stream[stream.len() - 2..], [Ident(..), Punct(ref punct)] if punct.as_char() == '~')
                    {
                        stream.pop().unwrap();
                        if let Some(Ident(ident)) = stream.pop() {
                            span = ident.span();
                            prefix = Some(ident);
                        }
                    }
                    if !stream.is_empty() {
                        seq.push(Self::TokenStream(stream.drain(..).collect()));
                    }
                    seq.push(Self::Placeholder(span, prefix))
                }
                Group(group) => {
                    if !stream.is_empty() {
                        seq.push(Self::TokenStream(stream.drain(..).collect()));
                    }
                    seq.push(Self::Group(
                        group.delimiter(),
                        group.span(),
                        Self::from_stream(placeholder, group.stream().into_iter()),
                    ))
                }
                Punct(ref punct)
                    if punct.as_char() == '*'
                        && seq.len() > 1
                        && matches!(seq.last(), Some(Self::Group(..)))
                        && matches!(&seq[seq.len() - 2], Self::TokenStream(tokens) if matches!(tokens.last(), Some(Punct(prec)) if prec.as_char() == '#')) =>
                {
                    // repeating group
                    if let Some(Self::Group(_, _, group)) = seq.pop() {
                        if let Some(Self::TokenStream(prec)) = seq.last_mut() {
                            assert!(
                                matches!(prec.pop(), Some(Punct(prec)) if prec.as_char() == '#')
                            );
                        }
                        seq.push(Self::Repeated(group));
                    }
                }
                tt => stream.extend(std::iter::once(tt)),
            }
        }
        if !stream.is_empty() {
            seq.push(Self::TokenStream(stream.drain(..).collect()));
        }
        seq
    }

    fn repeated<R: Iterator<Item = usize> + Clone>(&self, range: &R) -> proc_macro2::TokenStream {
        match self {
            Self::Placeholder(..) => {
                quote! { compile_error!("Placeholder outside of repeated section") }
            }
            Self::Group(delimiter, span, group) => {
                let mut grouped = proc_macro2::Group::new(
                    delimiter.clone(),
                    group.iter().map(|token| token.repeated(range)).collect(),
                );
                grouped.set_span(span.clone());
                proc_macro2::TokenTree::from(grouped).into()
            }
            Self::Repeated(group) => {
                let mut stream = proc_macro2::TokenStream::new();
                for n in range.clone() {
                    stream.extend(group.iter().map(|token| token.substitute(n)))
                }
                stream
            }
            Self::TokenStream(stream) => stream.iter().cloned().collect(),
        }
    }

    fn has_repetition(&self) -> bool {
        match self {
            Self::Repeated(..) => true,
            Self::Group(_, _, group) => group.iter().any(|part| part.has_repetition()),
            _ => false,
        }
    }

    fn substitute(&self, n: usize) -> proc_macro2::TokenStream {
        match self {
            Self::Placeholder(span, prefix) => match prefix {
                None => LitInt::new(&n.to_string(), span.clone()).into_token_stream(),
                Some(prefix) => {
                    format_ident!("{}{}", prefix, n, span = span.clone()).to_token_stream()
                }
            },
            Self::Group(delimiter, span, group) => {
                let mut grouped = proc_macro2::Group::new(
                    delimiter.clone(),
                    group.iter().map(|token| token.substitute(n)).collect(),
                );
                grouped.set_span(span.clone());
                proc_macro2::TokenTree::from(grouped).into()
            }
            Self::Repeated(..) => {
                quote! { compile_error!("Repeat inside repeat is not supported") }
            }
            Self::TokenStream(stream) => stream.iter().cloned().collect(),
        }
    }
}

struct Seq {
    range: std::ops::RangeInclusive<usize>,
    tokens: Vec<SeqPart>,
}

impl Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        let ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let start = input.parse::<LitInt>()?.base10_parse()?;
        input.parse::<Token![..]>()?;
        let range = {
            if input.peek(Token![=]) {
                input.parse::<Token![=]>()?;
                let end = input.parse::<LitInt>()?.base10_parse()?;
                start..=end
            } else {
                let end: usize = input.parse::<LitInt>()?.base10_parse()?;
                start..=end - 1
            }
        };
        braced!(content in input);
        let mut tokens = SeqPart::from_stream(
            &ident,
            content.parse::<proc_macro2::TokenStream>()?.into_iter(),
        );
        if !tokens.iter().any(|part| part.has_repetition()) {
            // wrap the token tree in a repeater
            tokens = vec![SeqPart::Repeated(tokens)];
        }
        Ok(Seq { range, tokens })
    }
}

impl ToTokens for Seq {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if !self.tokens.is_empty() {
            tokens.extend(self.tokens.iter().map(|tok| tok.repeated(&self.range)));
        }
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    parse_macro_input!(input as Seq).to_token_stream().into()
}
