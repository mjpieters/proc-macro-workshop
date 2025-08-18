use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Error, Field, Fields, Ident, Type};

/// If this is an optionized type, extract and return the type, None otherwise
fn optionized_type(ty: &Type) -> Option<&Type> {
    if let Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path {
            leading_colon: _,
            ref segments,
        },
    }) = ty
    {
        if segments.len() == 1 {
            if let Some(syn::PathSegment {
                ref ident,
                arguments:
                    syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                        ref args,
                        ..
                    }),
            }) = segments.first()
            {
                if ident == "Option" && args.len() == 1 {
                    if let Some(syn::GenericArgument::Type(ref ty)) = args.first() {
                        return Some(ty);
                    }
                }
            }
        }
    }
    None
}

/// If this is a vector, extract and return the item type, None otherwise
fn vec_item_type(ty: &Type) -> Option<&Type> {
    if let Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path {
            leading_colon: _,
            ref segments,
        },
    }) = ty
    {
        if segments.len() == 1 {
            if let Some(syn::PathSegment {
                ref ident,
                arguments:
                    syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                        ref args,
                        ..
                    }),
            }) = segments.first()
            {
                if ident == "Vec" && args.len() == 1 {
                    if let Some(syn::GenericArgument::Type(ref ty)) = args.first() {
                        return Some(ty);
                    }
                }
            }
        }
    }
    None
}

struct BuilderFieldConfig {
    each: Option<Ident>,
}

impl TryFrom<&Vec<syn::Attribute>> for BuilderFieldConfig {
    type Error = syn::Error;

    fn try_from(attrs: &Vec<syn::Attribute>) -> syn::Result<Self> {
        let mut each = None;
        for attr in attrs {
            if !attr.path().is_ident("builder") {
                continue;
            }

            attr.parse_nested_meta(|meta| {
                if !meta.path.is_ident("each") {
                    return Err(Error::new_spanned(
                        &attr.meta,
                        r#"expected `builder(each = "...")`"#,
                    ));
                };
                let value = meta.value()?;
                let ident_str: syn::LitStr = value.parse()?;
                let ident: Ident = ident_str.parse()?;
                if each.is_some() {
                    return Err(meta.error("each specified already"));
                };
                each = Some(ident);
                Ok(())
            })?;
        }
        Ok(Self { each })
    }
}

struct BuilderField<'a> {
    ident: &'a Ident,
    ty: &'a Type,
    optional: bool,
    config: BuilderFieldConfig,
}

impl<'a> BuilderField<'a> {
    /// the definition of this field in the builder struct
    fn field_definition(&self) -> proc_macro2::TokenStream {
        let ident = self.ident;
        let ty = self.ty;
        if self.config.each.is_none() {
            quote! { #ident: std::option::Option<#ty> }
        } else {
            quote! {#ident: #ty }
        }
    }

    /// initial value for the field when creating a builder struct
    fn field_init(&self) -> proc_macro2::TokenStream {
        let ident = self.ident;
        if self.config.each.is_none() {
            quote! { #ident: None }
        } else {
            quote! { #ident: Vec::new() }
        }
    }

    /// the method on the builder to handle this field
    fn field_method(&self) -> proc_macro2::TokenStream {
        let ident = self.ident;
        let ty = self.ty;
        if let Some(ref each) = self.config.each {
            let item_type = vec_item_type(ty);
            let set_vec = if each != ident {
                quote! {
                    pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = #ident;
                        self
                    }
                }
            } else {
                quote! {}
            };
            quote! {
                pub fn #each(&mut self, item: #item_type) -> &mut Self {
                    self.#ident.push(item);
                    self
                }
                #set_vec
            }
        } else {
            quote! {
                pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            }
        }
    }

    /// method to construct the final value on the built type
    fn construct_field(&self) -> proc_macro2::TokenStream {
        let ident = self.ident;
        if self.config.each.is_some() {
            quote! { #ident: self.#ident.drain(..).collect() }
        } else if self.optional {
            quote! { #ident: self.#ident.take() }
        } else {
            quote! { #ident: self.#ident.take().ok_or(concat!(stringify!(#ident), " is not set"))? }
        }
    }
}

impl<'a> TryFrom<&'a Field> for BuilderField<'a> {
    type Error = syn::Error;

    fn try_from(value: &'a Field) -> syn::Result<Self> {
        let attrs = &value.attrs;
        let config: BuilderFieldConfig = attrs.try_into()?;
        let optionized = optionized_type(&value.ty);
        Ok(Self {
            ident: value.ident.as_ref().unwrap(),
            ty: optionized.unwrap_or(&value.ty),
            optional: optionized.is_some(),
            config,
        })
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let fields: Vec<_> = if let Data::Struct(syn::DataStruct {
        fields: Fields::Named(ref fields),
        ..
    }) = input.data
    {
        let fields: Result<_, _> = fields.named.iter().map(BuilderField::try_from).collect();
        match fields {
            Ok(fields) => fields,
            Err(err) => return err.into_compile_error().into(),
        }
    } else {
        unimplemented!()
    };

    let name = input.ident;
    let builder_name = format_ident!("{name}Builder");
    let builder_fields: Vec<_> = fields.iter().map(BuilderField::field_definition).collect();
    let builder_init: Vec<_> = fields.iter().map(BuilderField::field_init).collect();
    let builder_methods: Vec<_> = fields.iter().map(BuilderField::field_method).collect();
    let build_struct_steps: Vec<_> = fields.iter().map(BuilderField::construct_field).collect();

    let tokens = quote! {
        pub struct #builder_name {
            #(#builder_fields),*
        }

        impl #builder_name {
            #(#builder_methods)*

            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#build_struct_steps),*
                })
            }
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_init),*
                }
            }
        }
    };

    tokens.into()
}
