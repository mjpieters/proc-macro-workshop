use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::{parse::Nothing, *};

struct BitFieldField<'a> {
    ident: &'a Ident,
    ty: &'a Ident,
    size_check: Option<TokenStream2>,
}

impl BitFieldField<'_> {
    fn size(&self) -> TokenStream2 {
        let ty = self.ty;
        quote!(<#ty as bitfield::Specifier>::BITS)
    }

    fn accessors(&self, vis: &Visibility, offset: &TokenStream2) -> TokenStream2 {
        let get = format_ident!("get_{}", self.ident);
        let set = format_ident!("set_{}", self.ident);
        let ty = self.ty;
        quote! {
            #vis fn #get(&self) -> <#ty as Specifier>::InterfaceType {
                <#ty as bitfield::Specifier>::get(&self.data, #offset)
            }
            #vis fn #set(&mut self, v: <#ty as Specifier>::InterfaceType) {
                <#ty as bitfield::Specifier>::set(&mut self.data, #offset, v);
            }
        }
    }
}

impl<'a> TryFrom<&'a Field> for BitFieldField<'a> {
    type Error = Error;

    fn try_from(value: &'a Field) -> Result<Self> {
        use Type::*;
        let ty = match &value.ty {
            Path(type_path) => type_path.path.require_ident()?,
            _ => return Err(Error::new_spanned(&value.ty, "unsupported type")),
        };
        let mut size_check = None;
        for attr in &value.attrs {
            if !attr.path().is_ident("bits") {
                continue;
            }
            if size_check.is_some() {
                return Err(Error::new_spanned(attr, "bits already set"));
            }
            if let Meta::NameValue(MetaNameValue {
                value:
                    Expr::Lit(ExprLit {
                        lit: Lit::Int(ref expected_size),
                        ..
                    }),
                ..
            }) = attr.meta
            {
                size_check = Some(
                    quote_spanned!(expected_size.span() => const _: [(); #expected_size] = [(); <#ty as Specifier>::BITS];),
                );
            } else {
                return Err(syn::Error::new_spanned(&attr.meta, "expected bits = ..."));
            }
        }
        Ok(Self {
            ident: value.ident.as_ref().unwrap(),
            ty,
            size_check,
        })
    }
}

struct BitField<'a> {
    ident: &'a Ident,
    vis: &'a Visibility,
    fields: Vec<BitFieldField<'a>>,
}

impl BitField<'_> {
    fn bit_sizes(&self) -> TokenStream2 {
        let field_sizes: Vec<_> = self.fields.iter().map(|field| field.size()).collect();
        quote!(0 #(+ #field_sizes)*)
    }

    fn accessors(&self) -> TokenStream2 {
        let mut tokens = TokenStream2::new();
        let mut offset = quote!(0);
        for field in &self.fields {
            tokens.extend(field.accessors(self.vis, &offset));
            let field_size = field.size();
            offset.extend(quote!(+ #field_size));
        }
        tokens
    }
}

impl<'a> TryFrom<&'a ItemStruct> for BitField<'a> {
    type Error = Error;

    fn try_from(value: &'a ItemStruct) -> Result<Self> {
        use Fields::*;
        Ok(Self {
            ident: &value.ident,
            vis: &value.vis,
            fields: match &value.fields {
                Named(fields) => fields
                    .named
                    .iter()
                    .map(|field| field.try_into())
                    .collect::<Result<_>>()?,
                _ => return Err(Error::new_spanned(&value.fields, "unsupported by bitfield")),
            },
        })
    }
}

impl ToTokens for BitField<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let vis = self.vis;
        let ident = self.ident;
        let bit_sizes = self.bit_sizes();
        let accessors = self.accessors();
        let field_checks: Vec<_> = self
            .fields
            .iter()
            .filter_map(|f| f.size_check.as_ref())
            .collect();

        quote! {
            #[repr(C)]
            #vis struct #ident {
                data: [u8; #ident::BYTES],
            }

            impl #ident {
                const BITS: usize = (#bit_sizes);
                const BYTES: usize = Self::BITS / 8;

                #vis fn new() -> Self {
                    Self { data: [0; Self::BYTES] }
                }

                #accessors
            }

            const _: () = assert!(#ident::BITS % 8 == 0, "bit count must be a multiple of 8");
            #(#field_checks)*
        }
        .to_tokens(tokens);
    }
}

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = parse_macro_input!(args as Nothing);
    let item = parse_macro_input!(input as ItemStruct);

    match BitField::try_from(&item) {
        Ok(bitfield) => quote!(#bitfield),
        Err(err) => {
            let err = err.to_compile_error();
            quote! {
                #err
                #item
            }
        }
    }
    .into()
}

struct EnumBitfieldSpecifier<'a> {
    ident: &'a Ident,
    variants: Vec<&'a Ident>,
}

impl<'a> TryFrom<&'a DeriveInput> for EnumBitfieldSpecifier<'a> {
    type Error = Error;

    fn try_from(value: &'a DeriveInput) -> Result<Self> {
        use Data::*;
        match &value.data {
            Enum(enum_data) => {
                let variants: Vec<_> = enum_data
                    .variants
                    .iter()
                    .map(|variant| {
                        if !matches!(variant.fields, Fields::Unit) {
                            return Err(Error::new_spanned(variant, "expected unit variant"));
                        };
                        Ok(&variant.ident)
                    })
                    .collect::<Result<_>>()?;
                if !variants.len().is_power_of_two() {
                    Err(Error::new(
                        Span::call_site(),
                        "BitfieldSpecifier expected a number of variants which is a power of 2",
                    ))
                } else {
                    Ok(Self {
                        ident: &value.ident,
                        variants,
                    })
                }
            }
            _ => unimplemented!(),
        }
    }
}

impl ToTokens for EnumBitfieldSpecifier<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let ident = self.ident;
        let bits = self.variants.len().trailing_zeros() as usize;
        let int_type = format_ident!(
            "u{}",
            if self.variants.len() < 5 {
                8
            } else {
                self.variants.len().next_power_of_two()
            }
        );
        let to_interface: Vec<_> = self
            .variants
            .iter()
            .map(|variant| {
                quote! { _ if v == #ident::#variant as #int_type => #ident::#variant }
            })
            .collect();
        let variant_count = &self.variants.len();
        let variant_checks: Vec<_> = self.variants.iter().map(|variant| {
            let ident = quote! { #ident::#variant };
            quote_spanned! {
                variant.span() =>
                    assert!(
                        (#ident as usize) < #variant_count,
                        concat!(stringify!(#ident), " discriminant exceeds the range 0..", #variant_count)
                    );
                }
            }
        ).collect();

        quote! {
            const _: () = {
                #(#variant_checks)*
            };
            impl Specifier for #ident {
                const BITS: usize = #bits;
                type InterfaceType = #ident;
                type IntType = #int_type;

                fn to_interface_type(v: Self::IntType) -> Self::InterfaceType {
                    match v {
                        #(#to_interface,)*
                        _ => unreachable!(),
                    }
                }
            }
            impl From<#ident> for #int_type {
                fn from(v: #ident) -> #int_type {
                    v as #int_type
                }
            }
        }
        .to_tokens(tokens);
    }
}

#[proc_macro_derive(BitfieldSpecifier)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match EnumBitfieldSpecifier::try_from(&input) {
        Err(err) => err.into_compile_error(),
        Ok(result) => result.into_token_stream(),
    }
    .into()
}
