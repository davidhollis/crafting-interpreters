use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parenthesized,
    parse::{Parse, Parser},
    punctuated::Punctuated,
    token::{self, Comma},
    Attribute, Data, DataEnum, DeriveInput, Fields, Ident, Meta, MetaList, Path, Variant,
};

struct HelperOption {
    name: Ident,
    _paren: token::Paren,
    path: Path,
}

impl Parse for HelperOption {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        Ok(HelperOption {
            name: input.parse()?,
            _paren: parenthesized!(content in input),
            path: content.parse()?,
        })
    }
}

#[proc_macro_derive(FromU8, attributes(from_u8))]
pub fn from_u8_derive(enum_item: TokenStream) -> TokenStream {
    let DeriveInput {
        attrs,
        ident: enum_ident,
        data,
        ..
    } = syn::parse(enum_item).unwrap();
    let mut variant_names: Vec<proc_macro2::TokenStream> = vec![];
    let mut err_type = None;
    let mut err_constructor = None;

    match data {
        Data::Enum(DataEnum { variants, .. }) => {
            for Variant {
                ident: variant_ident,
                fields,
                ..
            } in variants
            {
                match fields {
                    Fields::Unit => variant_names.push(quote! { #enum_ident::#variant_ident }),
                    _ => panic!("Enums which derive FromU8 may have only unit variants"),
                }
            }
        }
        _ => panic!("FromU8 can only be derived for enums"),
    }

    let helper_attr = attrs.into_iter().find(|attr| match attr.meta {
        Meta::List(MetaList { ref path, .. }) if path.is_ident("from_u8") => true,
        _ => false,
    });
    match helper_attr {
        Some(Attribute {
            meta: Meta::List(MetaList { tokens, .. }),
            ..
        }) => {
            let helper_parser = Punctuated::<HelperOption, Comma>::parse_terminated;
            let helpers = helper_parser.parse(tokens.into()).unwrap();
            for helper_option in helpers {
                let option_name = helper_option.name.to_string();
                if option_name == "err_type" {
                    err_type = Some(helper_option.path);
                } else if option_name == "err_constructor" {
                    err_constructor = Some(helper_option.path);
                }
            }

            impl_from_u8(
                enum_ident,
                variant_names,
                err_type.unwrap(),
                err_constructor.unwrap(),
            )
        }
        _ => panic!("Enums which derive FromU8 must have a #[from_u8(...)] attribute"),
    }
}

fn impl_from_u8(
    enum_ident: Ident,
    variants: Vec<proc_macro2::TokenStream>,
    err_type: Path,
    err_constructor: Path,
) -> TokenStream {
    let trait_impl = quote! {
        impl TryFrom<u8> for #enum_ident {
            type Error = #err_type;

            fn try_from(byte: u8) -> Result<Self, Self::Error> {
                match byte {
                    #(x if x == #variants as u8 => Ok(#variants)),*,
                    _ => Err(#err_constructor(byte).into()),
                }
            }
        }
    };
    trait_impl.into()
}
