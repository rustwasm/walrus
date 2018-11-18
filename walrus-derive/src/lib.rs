#![recursion_limit = "128"]

extern crate proc_macro;

use self::proc_macro::TokenStream;
use heck::SnakeCase;
use proc_macro2::Span;
use quote::quote;
use std::iter::FromIterator;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(WalrusExpr)]
pub fn walrus_expr(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let variants = get_enum_variants(&input);

    let visit = create_visit(&input.ident, &variants);
    let matchers = create_matchers(&variants);

    let expanded = quote! {
        #visit
        #matchers
    };

    TokenStream::from(expanded)
}

fn get_enum_variants(input: &DeriveInput) -> Vec<syn::Variant> {
    match &input.data {
        syn::Data::Enum(en) => en.variants.iter().cloned().collect(),
        syn::Data::Struct(_) => {
            panic!("can only put derive(WalrusExpr) on an enum; found it on a struct")
        }
        syn::Data::Union(_) => {
            panic!("can only put derive(WalrusExpr) on an enum; found it on a struct")
        }
    }
}

fn create_visit(name: &syn::Ident, variants: &[syn::Variant]) -> impl quote::ToTokens {
    let visitor_trait_methods: Vec<_> = variants
        .iter()
        .map(|v| {
            let name = &v.ident;

            let mut method_name = "visit_".to_string();
            method_name.push_str(&name.to_string().to_snake_case());
            let method_name = syn::Ident::new(&method_name, Span::call_site());

            let params = match &v.fields {
                syn::Fields::Named(fs) => fs
                    .named
                    .iter()
                    .map(|f| {
                        let p = f.ident.as_ref().unwrap().to_string().to_snake_case();
                        let p = syn::Ident::new(&p, Span::call_site());
                        let ty = &f.ty;
                        quote! { #p : &#ty }
                    })
                    .collect(),
                syn::Fields::Unnamed(fs) => fs
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(i, f)| {
                        let p = syn::Ident::new(&format!("arg{}", i), Span::call_site());
                        let ty = &f.ty;
                        quote! { #p : &#ty }
                    })
                    .collect(),
                syn::Fields::Unit => vec![],
            };

            let doc = format!("Visit `{}`.", name.to_string());

            quote! {
                #[doc=#doc]
                #[inline]
                fn #method_name(&mut self #( , #params )* ) {}
            }
        })
        .collect();

    let visit_impl: Vec<_> = variants
        .iter()
        .map(|v| {
            let name = &v.ident;

            let args = match &v.fields {
                syn::Fields::Named(fs) => fs
                    .named
                    .iter()
                    .map(|f| {
                        let p = f.ident.as_ref().unwrap().to_string().to_snake_case();
                        syn::Ident::new(&p, Span::call_site())
                    })
                    .collect(),
                syn::Fields::Unnamed(fs) => fs
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(i, _)| syn::Ident::new(&format!("arg{}", i), Span::call_site()))
                    .collect(),
                syn::Fields::Unit => vec![],
            };
            let args = &args;

            let pattern = match &v.fields {
                syn::Fields::Named(_) => quote! { { #( #args ),* } },
                syn::Fields::Unnamed(_) => quote! { ( #( #args ),* ) },
                syn::Fields::Unit => quote!{},
            };

            let mut method_name = "visit_".to_string();
            method_name.push_str(&name.to_string().to_snake_case());
            let method_name = syn::Ident::new(&method_name, Span::call_site());

            quote! {
                Expr::#name #pattern => {
                    visitor.#method_name( #( #args ),* );
                }
            }
        })
        .collect();

    quote! {
        /// TODO
        pub trait Visitor {
            #( #visitor_trait_methods )*
        }

        /// TODO
        pub trait Visit {
            /// TODO
            fn visit<V>(&self, visitor: &mut V)
            where
                V: Visitor;
        }

        impl Visit for #name {
            fn visit<V>(&self, visitor: &mut V)
            where
                V: Visitor
            {
                match self {
                    #( #visit_impl )*
                }
            }
        }
    }
}

fn create_matchers(variants: &[syn::Variant]) -> impl quote::ToTokens {
    use syn::punctuated::Punctuated;

    let matchers: Vec<_> = variants
        .iter()
        .map(|v| {
            let doc = format!("Match a `{}`", v.ident);
            let name = syn::Ident::new(&format!("{}Matcher", v.ident), Span::call_site());

            let make_matcher_ty_param = |i| match i {
                0 => quote! { T },
                1 => quote! { U },
                2 => quote! { V },
                _ => panic!("should have at most 3 exprs referenced in a single expr"),
            };

            let mut generics = vec![];
            let mut generic_tys = vec![];
            let mut fields = vec![];
            let mut args = vec![];
            let pattern;

            let expr_id_path = syn::TypePath {
                qself: None,
                path: syn::Path {
                    leading_colon: None,
                    segments: Punctuated::from_iter(vec![syn::PathSegment {
                        ident: syn::Ident::new("ExprId", Span::call_site()),
                        arguments: syn::PathArguments::None,
                    }]),
                },
            };

            match &v.fields {
                syn::Fields::Named(fs) => {
                    for (i, f) in fs
                        .named
                        .iter()
                        .filter(|f| match &f.ty {
                            syn::Type::Path(p) => *p == expr_id_path,
                            _ => false,
                        })
                        .enumerate()
                    {
                        let ty = make_matcher_ty_param(i);
                        generics.push(quote! { #ty : Matcher });

                        let arg = f.ident.clone().unwrap();
                        fields.push(quote! { #arg : #ty });

                        args.push(arg);
                        generic_tys.push(ty);
                    }
                    let args = &args;
                    pattern = quote! { { #( #args , )* .. } };
                }
                syn::Fields::Unnamed(fs) => {
                    for (i, _) in fs
                        .unnamed
                        .iter()
                        .filter(|f| match &f.ty {
                            syn::Type::Path(p) => *p == expr_id_path,
                            _ => false,
                        })
                        .enumerate()
                    {
                        let ty = make_matcher_ty_param(i);
                        generics.push(quote! { #ty : Matcher });

                        let arg = syn::Ident::new(&format!("arg{}", i), Span::call_site());
                        fields.push(quote! { #arg : #ty });

                        args.push(arg);
                        generic_tys.push(ty);
                    }
                    let args = &args;
                    pattern = quote! { ( #( #args , )* .. ) };
                }
                syn::Fields::Unit => {
                    pattern = quote!{};
                }
            };

            let new_doc = format!("Construct a new `{}`", name);
            let expr = &v.ident;
            let self_args: Vec<_> = args.iter().map(|a| quote! { self.#a }).collect();

            let fields = &fields;
            let generics = &generics;
            let generic_tys = &generic_tys;
            let args = &args;

            quote! {
                #[doc=#doc]
                pub struct #name < #( #generics ),* > {
                    #( #fields ),*
                }

                impl< #( #generics ),* > #name < #( #generic_tys ),* > {
                    #[doc=#new_doc]
                    pub fn new( #( #fields ),* ) -> Self {
                        #name {
                            #( #args ),*
                        }
                    }
                }

                impl< #( #generics ),* > Matcher for #name < #( #generic_tys ),* > {
                    fn is_match(&self, func: &Function, expr: &Expr) -> bool {
                        match expr {
                            Expr::#expr #pattern => {
                                true #(
                                    && #self_args.is_match(func, &func.exprs[*#args])
                                )*
                            }
                            _ => false,
                        }
                    }
                }
            }
        })
        .collect();

    quote! {
        pub(crate) mod generated_matchers {
            use crate::ir::Expr;
            use crate::function::Function;
            use super::matcher::Matcher;

            #( #matchers )*
        }
    }
}
