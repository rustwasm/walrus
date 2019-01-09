#![recursion_limit = "128"]

extern crate proc_macro;

use self::proc_macro::TokenStream;
use heck::SnakeCase;
use proc_macro2::Span;
use quote::quote;
use std::iter::FromIterator;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_attribute]
pub fn walrus_expr(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let variants = get_enum_variants(&input);

    assert_eq!(input.ident.to_string(), "Expr");

    let types = create_types(&input.attrs, &variants);
    let visit = create_visit(&variants);
    let matchers = create_matchers(&variants);

    let expanded = quote! {
        #types
        #visit
        #matchers
    };

    TokenStream::from(expanded)
}

fn get_enum_variants(input: &DeriveInput) -> Vec<syn::Variant> {
    match &input.data {
        syn::Data::Enum(en) => en.variants.iter().cloned().collect(),
        syn::Data::Struct(_) => {
            panic!("can only put #[walrus_expr] on an enum; found it on a struct")
        }
        syn::Data::Union(_) => {
            panic!("can only put #[walrus_expr] on an enum; found it on a union")
        }
    }
}

fn create_types(attrs: &[syn::Attribute], variants: &[syn::Variant]) -> impl quote::ToTokens {
    let types: Vec<_> = variants
        .iter()
        .map(|v| {
            let name = &v.ident;
            let id_name = {
                let mut s = name.to_string();
                s.push_str("Id");
                &syn::Ident::new(&s, Span::call_site())
            };
            let attrs = &v.attrs;
            let fields: Vec<_> = match v.fields {
                syn::Fields::Named(ref fs) => fs
                    .named
                    .iter()
                    .map(|f| {
                        let name = &f.ident;
                        let attrs = &f.attrs;
                        let ty = &f.ty;
                        quote! {
                            #( #attrs )*
                            pub #name : #ty,
                        }
                    })
                    .collect(),
                _ => panic!("#[walrus_expr] expects only named fields in enum variant"),
            };
            quote! {
                /// An identifier to a `#name` expression.
                #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
                pub struct #id_name(ExprId);

                impl From<#id_name> for ExprId {
                    #[inline]
                    fn from(id: #id_name) -> ExprId {
                        id.0
                    }
                }

                impl #id_name {
                    /// Construct a `#id_name` from an `ExprId`.
                    ///
                    /// It is the caller's responsibility to ensure that the
                    /// `Expr` referenced by the given `ExprId` is a `#name`.
                    #[inline]
                    pub fn new(id: ExprId) -> #id_name {
                        #id_name(id)
                    }
                }

                #( #attrs )*
                #[derive(Clone, Debug)]
                pub struct #name {
                    #( #fields )*
                }

                impl From<#name> for Expr {
                    #[inline]
                    fn from(x: #name) -> Expr {
                        Expr::#name(x)
                    }
                }

                impl Ast for #name {
                    type Id = #id_name;

                    #[inline]
                    fn new_id(id: ExprId) -> #id_name {
                        #id_name::new(id)
                    }
                }
            }
        })
        .collect();

    let methods: Vec<_> = variants
        .iter()
        .map(|v| {
            let name = &v.ident;
            let snake_name = name.to_string().to_snake_case();

            let is_name = format!("is_{}", snake_name);
            let is_name = syn::Ident::new(&is_name, Span::call_site());

            let ref_name = format!("{}_ref", snake_name);
            let ref_name = syn::Ident::new(&ref_name, Span::call_site());

            let mut_name = format!("{}_mut", snake_name);
            let mut_name = syn::Ident::new(&mut_name, Span::call_site());

            let unwrap_name = format!("unwrap_{}", snake_name);
            let unwrap_name = syn::Ident::new(&unwrap_name, Span::call_site());

            let unwrap_mut_name = format!("unwrap_{}_mut", snake_name);
            let unwrap_mut_name = syn::Ident::new(&unwrap_mut_name, Span::call_site());

            quote! {
                /// If this expression is a `#name`, get a shared reference to it.
                ///
                /// Returns `None` otherwise.
                #[inline]
                fn #ref_name(&self) -> Option<&#name> {
                    if let Expr::#name(ref x) = *self {
                        Some(x)
                    } else {
                        None
                    }
                }

                /// If this expression is a `#name`, get an exclusive reference to
                /// it.
                ///
                /// Returns `None` otherwise.
                #[inline]
                pub fn #mut_name(&mut self) -> Option<&mut #name> {
                    if let Expr::#name(ref mut x) = *self {
                        Some(x)
                    } else {
                        None
                    }
                }

                /// Is this expression a `#name`?
                #[inline]
                pub fn #is_name(&self) -> bool {
                    self.#ref_name().is_some()
                }

                /// Get a shared reference to the underlying `#name`.
                ///
                /// Panics if this expression is not a `#name`.
                #[inline]
                pub fn #unwrap_name(&self) -> &#name {
                    self.#ref_name().unwrap()
                }

                /// Get an exclusive reference to the underlying `#name`.
                ///
                /// Panics if this expression is not a `#name`.
                #[inline]
                pub fn #unwrap_mut_name(&mut self) -> &mut #name {
                    self.#mut_name().unwrap()
                }
            }
        })
        .collect();

    let variants: Vec<_> = variants
        .iter()
        .map(|v| {
            let name = &v.ident;
            let attrs = &v.attrs;
            quote! {
                #( #attrs )*
                #name(#name)
            }
        })
        .collect();

    quote! {
        #( #types )*

        #( #attrs )*
        pub enum Expr {
            #(#variants),*
        }

        impl Expr {
            #( #methods )*
        }
    }
}

fn create_visit(variants: &[syn::Variant]) -> impl quote::ToTokens {
    let visitor_trait_methods: Vec<_> = variants
        .iter()
        .map(|v| {
            let name = &v.ident;

            let mut method_name = "visit_".to_string();
            method_name.push_str(&name.to_string().to_snake_case());
            let method_name = syn::Ident::new(&method_name, Span::call_site());
            let doc = format!("Visit `{}`.", name.to_string());

            quote! {
                #[doc=#doc]
                #[inline]
                fn #method_name(&mut self, expr: &#name ) -> Self::Return;
            }
        })
        .collect();

    let visit_impl: Vec<_> = variants
        .iter()
        .map(|v| {
            let name = &v.ident;
            let mut method_name = "visit_".to_string();
            method_name.push_str(&name.to_string().to_snake_case());
            let method_name = syn::Ident::new(&method_name, Span::call_site());
            quote! {
                Expr::#name( ref e ) => {
                    visitor.#method_name(e)
                }
            }
        })
        .collect();

    quote! {
        /// A visitor walks over an IR expression tree.
        pub trait Visitor {
            /// The return type of the visitor.
            type Return;

            #( #visitor_trait_methods )*
        }

        /// Anything that can be visited by a `Visitor`.
        pub trait Visit {
            /// Visit this thing with the given visitor.
            fn visit<V>(&self, visitor: &mut V) -> V::Return
            where
                V: Visitor;
        }

        impl Visit for Expr {
            fn visit<V>(&self, visitor: &mut V) -> V::Return
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
                    pattern = quote! {};
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
                    fn is_match(&self, func: &LocalFunction, expr: &Expr) -> bool {
                        match expr {
                            Expr::#expr( #expr #pattern ) => {
                                true #(
                                    && #self_args.is_match(
                                        func,
                                        &func.exprs[*#args]
                                    )
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
            use crate::ir::*;
            use crate::module::functions::LocalFunction;
            use super::matcher::Matcher;

            #( #matchers )*
        }
    }
}
