extern crate proc_macro;

use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Field, Fields, FieldsNamed, Type};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    generate_builder(input)
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}

fn generate_builder(input: DeriveInput) -> Result<TokenStream, syn::Error> {
    let struct_name = &input.ident;
    let builder_name = Ident::new(&format!("{}Builder", struct_name), Span::call_site());
    let fields = FieldsInfo::new(&input)?;

    let init_exprs = fields.for_each(|name, _| {
        quote! { #name: None }
    });

    let builder_fields = fields.for_each(|name, ty| {
        quote! { #name: Option<#ty> }
    });

    let builder_methods = fields.for_each(|name, ty| {
        quote! {
            fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });

    let builder_results = fields.for_each(|name, _| {
        quote! {
            #name: self.#name.clone().ok_or_else(|| Box::<std::error::Error>::from(
                concat!("Required field '", stringify!(#name), "' has not been set.")
            ))?
        }
    });

    Ok(quote! {
        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#init_exprs,)*
                }
            }
        }

        pub struct #builder_name {
            #(#builder_fields,)*
        }

        impl #builder_name {
            #(#builder_methods)*

            pub fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                Ok(#struct_name {
                    #(#builder_results,)*
                })
            }
        }
    })
}

struct FieldsInfo<'a> {
    names_and_types: Vec<(Ident, &'a Type)>,
}

impl FieldsInfo<'_> {
    fn new(input: &DeriveInput) -> Result<FieldsInfo, syn::Error> {
        let named_fields = named_fields_iter(&input).ok_or(syn::Error::new(
            Span::call_site(),
            "Cannot generate builder for this type",
        ))?;

        let names_and_types: Option<Vec<_>> = named_fields
            .map(|f| Some((f.ident.clone()?, &f.ty)))
            .collect();
        let names_and_types = names_and_types.ok_or(syn::Error::new(
            Span::call_site(),
            "Cannot generate builder for unnamed field",
        ))?;

        Ok(FieldsInfo { names_and_types })
    }

    fn for_each<F: std::ops::Fn(&Ident, &Type) -> TokenStream>(&self, func: F) -> Vec<TokenStream> {
        self.names_and_types
            .iter()
            .map(|(name, ty)| func(name, ty))
            .collect()
    }
}

fn named_fields_iter(input: &DeriveInput) -> Option<impl Iterator<Item = &Field>> {
    match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => Some(named.iter()),
        _ => None,
    }
}
