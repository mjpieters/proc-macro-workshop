use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, parse_quote, visit::Visit, Data, DeriveInput, Error, Fields};

#[derive(PartialEq, Debug)]
enum DebugFieldGenerics<'a> {
    TypeParam(&'a syn::Ident),
    AssocType(&'a syn::Type),
}

struct GenericsVisitor<'a> {
    path_ident: Option<&'a syn::Ident>,
    results: Vec<DebugFieldGenerics<'a>>,
}

impl GenericsVisitor<'_> {
    fn new() -> Self {
        Self {
            path_ident: None,
            results: Vec::new(),
        }
    }
}

impl<'a> syn::visit::Visit<'a> for GenericsVisitor<'a> {
    fn visit_path_segment(&mut self, segment: &'a syn::PathSegment) {
        match &segment.arguments {
            syn::PathArguments::None => {
                self.results
                    .push(DebugFieldGenerics::TypeParam(&segment.ident));
            }
            arguments => {
                self.path_ident = Some(&segment.ident);
                syn::visit::visit_path_arguments(self, arguments);
                self.path_ident = None;
            }
        }
    }
    fn visit_angle_bracketed_generic_arguments(
        &mut self,
        gargs: &'a syn::AngleBracketedGenericArguments,
    ) {
        if gargs.args.len() == 1 {
            if let Some(ident) = self.path_ident {
                if ident == "PhantomData" {
                    return;
                }
            }
            self.visit_generic_argument(gargs.args.first().unwrap());
        }
    }
    fn visit_generic_argument(&mut self, arg: &'a syn::GenericArgument) {
        if let syn::GenericArgument::Type(ref ty) = arg {
            if let syn::Type::Path(syn::TypePath {
                qself: None,
                path:
                    syn::Path {
                        leading_colon: None,
                        ref segments,
                    },
            }) = ty
            {
                match segments.len() {
                    1 => {
                        let segment = segments.first().unwrap();
                        if let syn::PathSegment {
                            ref ident,
                            arguments: syn::PathArguments::None,
                        } = segment
                        {
                            self.results.push(DebugFieldGenerics::TypeParam(ident));
                        } else {
                            self.visit_path_segment(segment);
                        }
                    }
                    2 => {
                        if segments.iter().all(|seg| {
                            matches!(
                                seg,
                                syn::PathSegment {
                                    arguments: syn::PathArguments::None,
                                    ..
                                }
                            )
                        }) {
                            self.results.push(DebugFieldGenerics::AssocType(ty))
                        }
                    }
                    _ => {}
                }
            }
        }
    }
}

struct DebugField<'a> {
    name: &'a syn::Ident,
    format: Option<&'a syn::LitStr>,
    generics: Vec<DebugFieldGenerics<'a>>,
}

impl<'a> ToTokens for DebugField<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let ident = self.name;
        // output the format components as literal strings
        tokens.extend(match self.format {
            None => quote! { ::core::write!(f, ::core::concat!(::core::stringify!(#ident), ": {:?}"), &self.#ident) },
            Some(format) => {
                quote! { ::core::write!(f, ::core::concat!(::core::stringify!(#ident), ": ", #format), &self.#ident) }
            }
        })
    }
}

impl<'a> TryFrom<&'a syn::Field> for DebugField<'a> {
    type Error = syn::Error;

    fn try_from(value: &'a syn::Field) -> syn::Result<Self> {
        let name = value.ident.as_ref().unwrap();
        let mut format = None;
        for attr in &value.attrs {
            if !attr.path().is_ident("debug") {
                continue;
            }
            if let syn::Meta::NameValue(syn::MetaNameValue {
                value:
                    syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Str(ref format_str),
                        ..
                    }),
                ..
            }) = attr.meta
            {
                format = Some(format_str);
            } else {
                return Err(syn::Error::new_spanned(
                    &attr.meta,
                    r#"expected debug = "...""#,
                ));
            }
        }
        let mut visitor = GenericsVisitor::new();
        visitor.visit_type(&value.ty);
        Ok(DebugField {
            name,
            format,
            generics: visitor.results,
        })
    }
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);

    let fields: Vec<_> = if let Data::Struct(syn::DataStruct {
        fields: Fields::Named(ref fields),
        ..
    }) = input.data
    {
        let fields: Result<_, _> = fields.named.iter().map(DebugField::try_from).collect();
        match fields {
            Ok(fields) => fields,
            Err(err) => return err.into_compile_error().into(),
        }
    } else {
        unimplemented!()
    };

    let pred = {
        match input
            .attrs
            .iter()
            .fold(Ok(None), |bound, attr| match bound {
                Err(_) => bound,
                Ok(Some(_)) => Err(Error::new_spanned(&attr.meta, "no other options accepted")),
                Ok(None) => {
                    if !attr.path().is_ident("debug") {
                        Ok(None)
                    } else {
                        let mut expl_pred = None;
                        attr.parse_nested_meta(|meta| {
                            if !meta.path.is_ident("bound") {
                                Err(Error::new_spanned(
                                    &attr.meta,
                                    r#"expected debug(bound = "...")"#,
                                ))
                            } else {
                                let value = meta.value()?;
                                let pred_str: syn::LitStr = value.parse()?;
                                let pred: syn::WherePredicate = pred_str.parse()?;
                                expl_pred = Some(pred);
                                Ok(())
                            }
                        })?;
                        Ok(expl_pred)
                    }
                }
            }) {
            Ok(res) => res,
            Err(err) => return err.into_compile_error().into(),
        }
    };

    let name = input.ident;
    match pred {
        None => {
            for type_param in input.generics.type_params_mut() {
                if fields.iter().any(|f| {
                    f.generics
                        .contains(&DebugFieldGenerics::TypeParam(&type_param.ident))
                }) {
                    type_param.bounds.push(parse_quote!(::core::fmt::Debug));
                }
            }
            for f in fields.iter() {
                for generic in f.generics.iter() {
                    if let DebugFieldGenerics::AssocType(ref assoc) = generic {
                        let where_clause = input.generics.make_where_clause();
                        where_clause
                            .predicates
                            .push(parse_quote! { #assoc: ::core::fmt::Debug });
                    }
                }
            }
        }
        Some(pred) => {
            let where_clause = input.generics.make_where_clause();
            where_clause.predicates.push(pred);
        }
    }

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let tokens = quote! {
        impl #impl_generics ::core::fmt::Debug for #name #ty_generics #where_clause {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::result::Result<(), ::core::fmt::Error> {
                let mut has_fields = false;
                f.write_str(::core::stringify!(#name))?;
                #(
                    f.write_str(if has_fields { ", " } else { " { "})?;
                    #fields?;
                    has_fields = true;
                )*
                if has_fields { f.write_str(" }")?; };
                Ok(())
            }
        }
    };

    tokens.into()
}
