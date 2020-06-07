extern crate proc_macro;

use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, Data, DeriveInput, Fields, FieldsNamed, GenericArgument,
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
    let fields = read_fields(&input)?;

    let init_exprs = fields.expand(|field| {
        let name = &field.name;
        quote! { #name: None }
    });

    let builder_fields = fields.expand(|field| {
        let name = &field.name;
        let ty = &field.ty;
        quote! { #name: Option<#ty> }
    });

    let builder_methods = fields.expand(|field| {
        let name = &field.name;
        let ty = &field.ty;
        quote! {
            fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });

    let builder_results = fields.expand(|field| {
        let name = &field.name;
        if field.is_optional {
            quote! { #name: self.#name.clone() }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(
                    concat!("Required field '", stringify!(#name), "' has not been set.").to_owned()
                )?
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

            pub fn build(&mut self) -> Result<#struct_name, String> {
                Ok(#struct_name {
                    #(#builder_results,)*
                })
            }
        }
    })
}

struct FieldInfo<'a> {
    name: &'a Ident,
    ty: &'a Type,
    is_optional: bool,
}

struct FieldsInfo<'a>(Vec<FieldInfo<'a>>);

impl FieldsInfo<'_> {
    fn expand<F: std::ops::Fn(&FieldInfo) -> TokenStream>(&self, func: F) -> Vec<TokenStream> {
        self.0.iter().map(|f| func(f)).collect()
    }
}

fn read_fields(input: &DeriveInput) -> Result<FieldsInfo, syn::Error> {
    match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(FieldsNamed { named, .. }) => {
                Ok(FieldsInfo(named.iter().map(read_field).collect()))
            }
            _ => Err(syn::Error::new(
                data.fields.span(),
                "Builders may only be derived for structs with named fields.",
            )),
        },
        _ => Err(syn::Error::new(
            input.ident.span(),
            "Builders may only be derived for structs.",
        )),
    }
}

fn read_field(field: &syn::Field) -> FieldInfo {
    let name = field.ident.as_ref().expect("Expected named field");
    let (ty, is_optional) = read_field_type(&field.ty);
    FieldInfo {
        name,
        ty,
        is_optional,
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
