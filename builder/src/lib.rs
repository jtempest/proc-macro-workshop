extern crate proc_macro;

use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Field, Fields, FieldsNamed, Type};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_name = &input.ident;
    let builder_name = Ident::new(&format!("{}Builder", struct_name), Span::call_site());

    let fields = match FieldsInfo::new(&input) {
        Ok(info) => info,
        Err(e) => return e.to_compile_error().into(),
    };

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

    let result = quote! {
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
        }
    };
    result.into()
}

struct FieldsInfo<'a> {
    names_and_types: Vec<(Ident, &'a Type)>,
}

impl FieldsInfo<'_> {
    fn new(input: &DeriveInput) -> Result<FieldsInfo, syn::Error> {
        let named_fields = get_named_fields(&input).ok_or(syn::Error::new(
            Span::call_site(),
            "Cannot generate builder for this type",
        ))?;

        let names_and_types: Option<Vec<_>> = named_fields
            .iter()
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

fn get_named_fields(input: &DeriveInput) -> Option<Vec<&Field>> {
    match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => Some(named.iter().collect()),
        _ => None,
    }
}
