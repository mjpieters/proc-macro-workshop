use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::visit_mut::VisitMut;
use syn::{
    parse_macro_input, parse_quote, Attribute, Error, Expr, ExprMatch, Ident, Item, ItemEnum,
    ItemFn, Pat, Result, Variant,
};

enum SortCheckItem<'a> {
    Enum(&'a ItemEnum),
    Match(&'a ExprMatch),
}

impl<'a> TryFrom<&'a Item> for SortCheckItem<'a> {
    type Error = Error;

    fn try_from(value: &'a Item) -> Result<Self> {
        match value {
            Item::Enum(item) => Ok(Self::Enum(item)),
            _ => Err(Error::new(
                Span::call_site(),
                "expected enum or match expression",
            )),
        }
    }
}

impl SortCheckItem<'_> {
    fn check_sorted(&self) -> Result<()> {
        is_sorted(match self {
            Self::Enum(item) => item.variants.iter().map(|variant| variant.into()).collect(),
            Self::Match(item) => item
                .arms
                .iter()
                .map(|arm| (&arm.pat).try_into())
                .collect::<Result<_>>()?,
        })
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord)]
struct Sortable(Vec<Ident>);

impl<'a> From<&'a Variant> for Sortable {
    fn from(value: &'a Variant) -> Self {
        Self(vec![value.ident.clone()])
    }
}

impl TryFrom<&Pat> for Sortable {
    type Error = Error;

    fn try_from(value: &Pat) -> std::result::Result<Self, Self::Error> {
        use Pat::*;
        let path_idents =
            |path: &syn::Path| path.segments.iter().map(|seg| seg.ident.clone()).collect();
        Ok(Self(match value {
            Ident(pat) => vec![pat.ident.clone()],
            Path(pat) => path_idents(&pat.path),
            Struct(pat) => path_idents(&pat.path),
            TupleStruct(pat) => path_idents(&pat.path),
            Wild(pat) => vec![syn::Ident::from(pat.underscore_token)],
            _ => return Err(Error::new_spanned(value, "unsupported by #[sorted]")),
        }))
    }
}

impl ToTokens for Sortable {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        for ident in &self.0 {
            ident.to_tokens(tokens);
        }
    }
}

impl std::fmt::Display for Sortable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut sep = "";
        for ident in &self.0 {
            f.write_str(sep)?;
            ident.fmt(f)?;
            sep = "::";
        }
        Ok(())
    }
}

fn is_sorted(items: Vec<Sortable>) -> Result<()> {
    for i in 1..items.len() {
        let curr = &items[i];
        let prec = &items[i - 1];
        if let std::cmp::Ordering::Less = &curr.cmp(prec) {
            let before = items[..i]
                .iter()
                .find(|&name| matches!(&curr.cmp(name), std::cmp::Ordering::Less))
                .unwrap();
            return Err(Error::new_spanned(
                curr,
                format!("{} should sort before {}", &curr, &before),
            ));
        }
    }
    Ok(())
}

fn marked_as_sorted(attrs: &mut Vec<Attribute>) -> bool {
    for i in 0..attrs.len() {
        if attrs[i].meta.path().is_ident("sorted") {
            attrs.remove(i);
            return true;
        }
    }
    false
}

struct CheckVisitor;

impl VisitMut for CheckVisitor {
    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        let match_expr = match expr {
            Expr::Match(match_expr) => match_expr,
            _ => return,
        };
        if !marked_as_sorted(&mut match_expr.attrs) {
            return;
        }
        match SortCheckItem::Match(match_expr).check_sorted() {
            Ok(_) => (),
            Err(err) => {
                let err = err.to_compile_error();
                *expr = parse_quote!({
                    #err
                    #match_expr
                })
            }
        };
    }
}

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = parse_macro_input!(args as syn::parse::Nothing);
    let item = parse_macro_input!(input as Item);
    match SortCheckItem::try_from(&item).and_then(|item| item.check_sorted()) {
        Err(err) => {
            let err = err.into_compile_error();
            quote! {
                #err
                #item
            }
        }
        Ok(..) => item.to_token_stream(),
    }
    .into()
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = parse_macro_input!(args as syn::parse::Nothing);
    let mut item_fn = parse_macro_input!(input as ItemFn);
    CheckVisitor.visit_item_fn_mut(&mut item_fn);
    item_fn.to_token_stream().into()
}
