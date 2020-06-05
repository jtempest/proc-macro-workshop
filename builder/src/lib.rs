extern crate proc_macro;

use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{
    parse_macro_input, Data, DataStruct, DeriveInput, Field, Fields, FieldsNamed, GenericArgument,
    PathArguments, Type,
};

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

    let init_exprs = fields.for_each(|field| {
        let name = &field.name;
        quote! { #name: None }
    });

    let builder_fields = fields.for_each(|field| {
        let name = &field.name;
        let ty = &field.ty;
        quote! { #name: Option<#ty> }
    });

    let builder_methods = fields.for_each(|field| {
        let name = &field.name;
        let ty = &field.ty;
        quote! {
            fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });

    let builder_results = fields.for_each(|field| {
        let name = &field.name;
        if field.is_optional {
            quote! { #name: self.#name.clone() }
        } else {
            quote! {
                #name: self.#name.clone().ok_or_else(|| Box::<std::error::Error>::from(
                    concat!("Required field '", stringify!(#name), "' has not been set.")
                ))?
            }
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

struct FieldInfo<'a> {
    name: Ident,
    ty: &'a Type,
    is_optional: bool,
}

struct FieldsInfo<'a>(Vec<FieldInfo<'a>>);

impl FieldsInfo<'_> {
    fn new(input: &DeriveInput) -> Result<FieldsInfo, syn::Error> {
        let named_fields = named_fields_iter(&input).ok_or(syn::Error::new(
            Span::call_site(),
            "Cannot generate builder for this type",
        ))?;

        let fields: Option<Vec<_>> = named_fields
            .map(|f| {
                let name = f.ident.clone()?;
                let (ty, is_optional) = read_field_type(&f.ty);
                Some(FieldInfo {
                    name,
                    ty,
                    is_optional,
                })
            })
            .collect();
        let fields = fields.ok_or(syn::Error::new(
            Span::call_site(),
            "Cannot generate builder for unnamed field",
        ))?;

        Ok(FieldsInfo(fields))
    }

    fn for_each<F: std::ops::Fn(&FieldInfo) -> TokenStream>(&self, func: F) -> Vec<TokenStream> {
        self.0.iter().map(|f| func(f)).collect()
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

// (ty, is_optional): if optional, returns the type contained by the Option.
fn read_field_type(ty: &Type) -> (&Type, bool) {
    if let Type::Path(path) = ty {
        if path.qself.is_none() {
            let segments = &path.path.segments;
            if segments.len() > 0 {
                let seg0 = &segments[0];
                if seg0.ident.to_string() == "Option" {
                    if let PathArguments::AngleBracketed(args) = &seg0.arguments {
                        if let Some(GenericArgument::Type(optional_type)) = args.args.first() {
                            return (optional_type, true);
                        }
                    }
                }
            }
        }
    }
    (ty, false)
}
