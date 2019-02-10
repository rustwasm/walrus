#![recursion_limit = "128"]

extern crate proc_macro;

use self::proc_macro::TokenStream;
use heck::SnakeCase;
use proc_macro2::Span;
use quote::quote;
use std::iter::FromIterator;
use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::DeriveInput;
use syn::Error;
use syn::{parse_macro_input, Ident, Result, Token};

#[proc_macro_attribute]
pub fn walrus_expr(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let variants = match get_enum_variants(&input) {
        Ok(v) => v,
        Err(e) => return e.to_compile_error().into(),
    };

    assert_eq!(input.ident.to_string(), "Expr");

    let types = create_types(&input.attrs, &variants);
    let visit = create_visit(&variants);
    let matchers = create_matchers(&variants);
    let display = create_display(&variants);
    let dot = create_dot(&variants);
    let builder = create_builder(&variants);

    let expanded = quote! {
        #types
        #visit
        #matchers
        #display
        #dot
        #builder
    };

    TokenStream::from(expanded)
}

struct WalrusVariant {
    syn: syn::Variant,
    fields: Vec<WalrusFieldOpts>,
    opts: WalrusVariantOpts,
}

#[derive(Default)]
struct WalrusVariantOpts {
    display_name: Option<syn::Ident>,
    display_extra: Option<syn::Ident>,
    dot_name: Option<syn::Ident>,
}

#[derive(Default)]
struct WalrusFieldOpts {
    skip_visit: bool,
}

fn get_enum_variants(input: &DeriveInput) -> Result<Vec<WalrusVariant>> {
    let en = match &input.data {
        syn::Data::Enum(en) => en,
        syn::Data::Struct(_) => {
            panic!("can only put #[walrus_expr] on an enum; found it on a struct")
        }
        syn::Data::Union(_) => {
            panic!("can only put #[walrus_expr] on an enum; found it on a union")
        }
    };
    en.variants
        .iter()
        .cloned()
        .map(|mut variant| {
            let attrs = walrus_attrs(&mut variant.attrs);

            Ok(WalrusVariant {
                fields: variant
                    .fields
                    .iter_mut()
                    .map(|field| syn::parse(walrus_attrs(&mut field.attrs)))
                    .collect::<Result<_>>()?,
                syn: variant,
                opts: syn::parse(attrs)?,
            })
        })
        .collect()
}

impl Parse for WalrusFieldOpts {
    fn parse(input: ParseStream) -> Result<Self> {
        enum Attr {
            SkipVisit,
        }

        let attrs = Punctuated::<_, syn::token::Comma>::parse_terminated(input)?;
        let mut ret = WalrusFieldOpts::default();
        for attr in attrs {
            match attr {
                Attr::SkipVisit => ret.skip_visit = true,
            }
        }
        return Ok(ret);

        impl Parse for Attr {
            fn parse(input: ParseStream) -> Result<Self> {
                let attr: Ident = input.parse()?;
                if attr == "skip_visit" {
                    return Ok(Attr::SkipVisit);
                }
                return Err(Error::new(attr.span(), "unexpected attribute"));
            }
        }
    }
}

impl Parse for WalrusVariantOpts {
    fn parse(input: ParseStream) -> Result<Self> {
        enum Attr {
            DisplayName(syn::Ident),
            DisplayExtra(syn::Ident),
            DotName(syn::Ident),
        }

        let attrs = Punctuated::<_, syn::token::Comma>::parse_terminated(input)?;
        let mut ret = WalrusVariantOpts::default();
        for attr in attrs {
            match attr {
                Attr::DisplayName(ident) => ret.display_name = Some(ident),
                Attr::DisplayExtra(ident) => ret.display_extra = Some(ident),
                Attr::DotName(ident) => ret.dot_name = Some(ident),
            }
        }
        return Ok(ret);

        impl Parse for Attr {
            fn parse(input: ParseStream) -> Result<Self> {
                let attr: Ident = input.parse()?;
                if attr == "display_name" {
                    input.parse::<Token![=]>()?;
                    let name = input.call(Ident::parse_any)?;
                    return Ok(Attr::DisplayName(name));
                }
                if attr == "display_extra" {
                    input.parse::<Token![=]>()?;
                    let name = input.call(Ident::parse_any)?;
                    return Ok(Attr::DisplayExtra(name));
                }
                if attr == "dot_name" {
                    input.parse::<Token![=]>()?;
                    let name = input.call(Ident::parse_any)?;
                    return Ok(Attr::DotName(name));
                }
                return Err(Error::new(attr.span(), "unexpected attribute"));
            }
        }
    }
}

fn walrus_attrs(attrs: &mut Vec<syn::Attribute>) -> TokenStream {
    let mut ret = proc_macro2::TokenStream::new();
    let ident = syn::Path::from(syn::Ident::new("walrus", Span::call_site()));
    for i in (0..attrs.len()).rev() {
        if attrs[i].path != ident {
            continue;
        }
        let attr = attrs.remove(i);
        let group = match attr.tts.into_iter().next().unwrap() {
            proc_macro2::TokenTree::Group(g) => g,
            _ => panic!("#[walrus(...)] expected"),
        };
        ret.extend(group.stream());
        ret.extend(quote! { , });
    }
    return ret.into();
}

fn create_types(attrs: &[syn::Attribute], variants: &[WalrusVariant]) -> impl quote::ToTokens {
    let types: Vec<_> = variants
        .iter()
        .map(|v| {
            let name = &v.syn.ident;
            let id_name = {
                let mut s = name.to_string();
                s.push_str("Id");
                &syn::Ident::new(&s, Span::call_site())
            };
            let attrs = &v.syn.attrs;
            let fields = v.syn.fields.iter().map(|f| {
                let name = &f.ident;
                let attrs = &f.attrs;
                let ty = &f.ty;
                quote! {
                    #( #attrs )*
                    pub #name : #ty,
                }
            });
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

                impl<'expr> Visit<'expr> for #id_name {
                    fn visit<V: Visitor<'expr>>(&self, v: &mut V) {
                        v.visit_expr_id(&self.0);
                    }
                }

                impl VisitMut for #id_name {
                    fn visit_mut<V: VisitorMut>(&mut self, v: &mut V) {
                        v.visit_expr_id_mut(&mut self.0);
                    }
                }
            }
        })
        .collect();

    let methods: Vec<_> = variants
        .iter()
        .map(|v| {
            let name = &v.syn.ident;
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
            let name = &v.syn.ident;
            let attrs = &v.syn.attrs;
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

fn visit_fields<'a>(
    variant: &'a WalrusVariant,
    allow_skip: bool,
) -> impl Iterator<Item = (syn::Ident, proc_macro2::TokenStream, bool)> + 'a {
    return variant
        .syn
        .fields
        .iter()
        .zip(&variant.fields)
        .enumerate()
        .filter(move |(_, (_, info))| !allow_skip || !info.skip_visit)
        .map(move |(i, (field, _info))| {
            let field_name = match &field.ident {
                Some(name) => quote! { #name },
                None => quote! { #i },
            };
            let (ty_name, list) = extract_name_and_if_list(&field.ty);
            let mut method_name = "visit_".to_string();
            method_name.push_str(&ty_name.to_string().to_snake_case());
            let method_name = syn::Ident::new(&method_name, Span::call_site());
            (method_name, field_name, list)
        });

    fn extract_name_and_if_list(ty: &syn::Type) -> (&syn::Ident, bool) {
        let path = match ty {
            syn::Type::Path(p) => &p.path,
            _ => panic!("field types must be paths"),
        };
        let segment = path.segments.last().unwrap().into_value();
        let args = match &segment.arguments {
            syn::PathArguments::None => return (&segment.ident, false),
            syn::PathArguments::AngleBracketed(a) => &a.args,
            _ => panic!("invalid path in #[walrus_expr]"),
        };
        let mut ty = match args.first().unwrap().into_value() {
            syn::GenericArgument::Type(ty) => ty,
            _ => panic!("invalid path in #[walrus_expr]"),
        };
        if let syn::Type::Slice(t) = ty {
            ty = &t.elem;
        }
        match ty {
            syn::Type::Path(p) => {
                let segment = p.path.segments.last().unwrap().into_value();
                (&segment.ident, true)
            }
            _ => panic!("invalid path in #[walrus_expr]"),
        }
    }
}

fn create_visit(variants: &[WalrusVariant]) -> impl quote::ToTokens {
    let mut visit_impls = Vec::new();
    let mut visitor_trait_methods = Vec::new();
    let mut visitor_mut_trait_methods = Vec::new();
    let mut visit_impl = Vec::new();
    let mut visit_mut_impl = Vec::new();

    for variant in variants {
        let name = &variant.syn.ident;
        let name_id = syn::Ident::new(&format!("{}Id", name), Span::call_site());

        let mut method_name = "visit_".to_string();
        method_name.push_str(&name.to_string().to_snake_case());
        let method_name = syn::Ident::new(&method_name, Span::call_site());
        let method_id_name = syn::Ident::new(&format!("{}_id", method_name), Span::call_site());
        let method_name_mut = syn::Ident::new(&format!("{}_mut", method_name), Span::call_site());
        let method_id_name_mut =
            syn::Ident::new(&format!("{}_id_mut", method_name), Span::call_site());

        let recurse_fields = visit_fields(variant, true).map(|(method_name, field_name, list)| {
            if list {
                quote! {
                    for item in self.#field_name.iter() {
                        visitor.#method_name(item);
                    }
                }
            } else {
                quote! { visitor.#method_name(&self.#field_name); }
            }
        });
        let recurse_fields_mut =
            visit_fields(variant, true).map(|(method_name, field_name, list)| {
                let name = format!("{}_mut", method_name);
                let method_name = syn::Ident::new(&name, Span::call_site());
                if list {
                    quote! {
                        for item in self.#field_name.iter_mut() {
                            visitor.#method_name(item);
                        }
                    }
                } else {
                    quote! { visitor.#method_name(&mut self.#field_name); }
                }
            });

        visit_impls.push(quote! {
            impl<'expr> Visit<'expr> for #name {
                fn visit<V: Visitor<'expr>>(&self, visitor: &mut V) {
                    #(#recurse_fields);*
                }
            }
            impl VisitMut for #name {
                fn visit_mut<V: VisitorMut>(&mut self, visitor: &mut V) {
                    #(#recurse_fields_mut);*
                }
            }
        });

        let doc = format!("Visit `{}`.", name.to_string());
        let doc_id = format!("Visit `{}Id`.", name.to_string());
        visitor_trait_methods.push(quote! {
            #[doc=#doc]
            fn #method_name(&mut self, expr: &#name) {
                expr.visit(self);
            }

            #[doc=#doc_id]
            fn #method_id_name(&mut self, id: &#name_id) {
                id.visit(self);
            }
        });
        visitor_mut_trait_methods.push(quote! {
            #[doc=#doc]
            fn #method_name_mut(&mut self, expr: &mut #name) {
                expr.visit_mut(self);
            }

            #[doc=#doc_id]
            fn #method_id_name_mut(&mut self, id: &mut #name_id) {
                id.visit_mut(self);
            }
        });

        let mut method_name = "visit_".to_string();
        method_name.push_str(&name.to_string().to_snake_case());
        let method_name = syn::Ident::new(&method_name, Span::call_site());
        visit_impl.push(quote! {
            Expr::#name(e) => visitor.#method_name(e),
        });
        visit_mut_impl.push(quote! {
            Expr::#name(e) => visitor.#method_name_mut(e),
        });
    }

    quote! {
        /// A visitor walks over an IR expression tree.
        pub trait Visitor<'expr>: Sized {
            /// Return the local function we're visiting
            fn local_function(&self) -> &'expr crate::LocalFunction;

            /// Visit `Expr`.
            fn visit_expr(&mut self, expr: &'expr Expr) {
                expr.visit(self)
            }

            /// Visit `ExprId`.
            fn visit_expr_id(&mut self, expr: &ExprId) {
                expr.visit(self);
            }

            /// Visit `Local`.
            fn visit_local_id(&mut self, local: &crate::LocalId) {
                // ...
            }

            /// Visit `Memory`.
            fn visit_memory_id(&mut self, memory: &crate::MemoryId) {
                // ...
            }

            /// Visit `Table`.
            fn visit_table_id(&mut self, table: &crate::TableId) {
                // ...
            }

            /// Visit `GlobalId`.
            fn visit_global_id(&mut self, global: &crate::GlobalId) {
                // ...
            }

            /// Visit `FunctionId`.
            fn visit_function_id(&mut self, function: &crate::FunctionId) {
                // ...
            }

            /// Visit `DataId`.
            fn visit_data_id(&mut self, function: &crate::DataId) {
                // ...
            }

            /// Visit `TypeId`
            fn visit_type_id(&mut self, ty: &crate::TypeId) {
                // ...
            }

            /// Visit `Value`.
            fn visit_value(&mut self, value: &crate::ir::Value) {
                // ...
            }

            #( #visitor_trait_methods )*
        }

        /// A visitor walks over a mutable IR expression tree.
        pub trait VisitorMut: Sized {
            /// Return the local function we're visiting
            fn local_function_mut(&mut self) -> &mut crate::LocalFunction;

            /// Visit `Expr`.
            fn visit_expr_mut(&mut self, expr: &mut Expr) {
                expr.visit_mut(self)
            }

            /// Visit `ExprId`.
            fn visit_expr_id_mut(&mut self, expr: &mut ExprId) {
                expr.visit_mut(self);
            }

            /// Visit `Local`.
            fn visit_local_id_mut(&mut self, local: &mut crate::LocalId) {
                // ...
            }

            /// Visit `Memory`.
            fn visit_memory_id_mut(&mut self, memory: &mut crate::MemoryId) {
                // ...
            }

            /// Visit `Table`.
            fn visit_table_id_mut(&mut self, table: &mut crate::TableId) {
                // ...
            }

            /// Visit `GlobalId`.
            fn visit_global_id_mut(&mut self, global: &mut crate::GlobalId) {
                // ...
            }

            /// Visit `FunctionId`.
            fn visit_function_id_mut(&mut self, function: &mut crate::FunctionId) {
                // ...
            }

            /// Visit `DataId`.
            fn visit_data_id_mut(&mut self, function: &mut crate::DataId) {
                // ...
            }

            /// Visit `TypeId`
            fn visit_type_id_mut(&mut self, ty: &mut crate::TypeId) {
                // ...
            }

            /// Visit `Value`.
            fn visit_value_mut(&mut self, value: &mut crate::ir::Value) {
                // ...
            }

            #( #visitor_mut_trait_methods )*
        }


        impl<'expr> Visit<'expr> for Expr {
            fn visit<V>(&self, visitor: &mut V) where V: Visitor<'expr> {
                match self {
                    #( #visit_impl )*
                }
            }
        }

        impl VisitMut for Expr {
            fn visit_mut<V>(&mut self, visitor: &mut V) where V: VisitorMut {
                match self {
                    #( #visit_mut_impl )*
                }
            }
        }

        #( #visit_impls )*
    }
}

fn create_matchers(variants: &[WalrusVariant]) -> impl quote::ToTokens {
    let matchers: Vec<_> = variants
        .iter()
        .map(|v| {
            let doc = format!("Match a `{}`", v.syn.ident);
            let name = syn::Ident::new(&format!("{}Matcher", v.syn.ident), Span::call_site());

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

            match &v.syn.fields {
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
            let expr = &v.syn.ident;
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
                    fn is_match(&self, local_func: &LocalFunction, expr: &Expr) -> bool {
                        match expr {
                            Expr::#expr( #expr #pattern ) => {
                                true #(
                                    && #self_args.is_match(
                                        local_func,
                                        &local_func.exprs[*#args]
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
            use crate::LocalFunction;
            use super::matcher::Matcher;

            #( #matchers )*
        }
    }
}

fn create_display(variants: &[WalrusVariant]) -> impl quote::ToTokens {
    let mut display_methods = Vec::new();
    for variant in variants {
        let name = &variant.syn.ident;

        let mut method_name = "visit_".to_string();
        method_name.push_str(&name.to_string().to_snake_case());
        let method_name = syn::Ident::new(&method_name, Span::call_site());

        let instr_name = match &variant.opts.display_name {
            Some(f) => quote! { #f(expr, self) },
            None => {
                let instr = name.to_string().to_snake_case().replace("_", ".");
                quote! { self.f.push_str(#instr) }
            }
        };
        let extra = match &variant.opts.display_extra {
            Some(extra) => quote! { #extra(expr, self); },
            None => quote! {},
        };
        display_methods.push(quote! {
            fn #method_name(&mut self, expr: &#name) {
                #instr_name;
                #extra
                expr.visit(self);
            }
        });
    }
    quote! {
        impl<'expr> Visitor<'expr> for crate::module::DisplayExpr<'expr, '_> {
            fn local_function(&self) -> &'expr crate::LocalFunction {
                self.func
            }

            fn visit_expr_id(&mut self, expr: &ExprId) {
                self.expr_id(*expr)
            }

            fn visit_local_id(&mut self, local: &crate::LocalId) {
                self.id(*local);
            }

            fn visit_memory_id(&mut self, memory: &crate::MemoryId) {
                self.id(*memory);
            }

            fn visit_table_id(&mut self, table: &crate::TableId) {
                self.id(*table);
            }

            fn visit_global_id(&mut self, global: &crate::GlobalId) {
                self.id(*global);
            }

            fn visit_function_id(&mut self, function: &crate::FunctionId) {
                self.id(*function);
            }

            fn visit_type_id(&mut self, ty: &crate::TypeId) {
                self.id(*ty);
            }

            fn visit_data_id(&mut self, data: &crate::DataId) {
                self.id(*data);
            }

            fn visit_value(&mut self, value: &crate::ir::Value) {
                self.f.push_str(" ");
                self.f.push_str(&value.to_string());
            }

            #(#display_methods)*
        }
    }
}

fn create_dot(variants: &[WalrusVariant]) -> impl quote::ToTokens {
    let mut dot_methods = Vec::new();
    for variant in variants {
        let name = &variant.syn.ident;

        let mut method_name = "visit_".to_string();
        method_name.push_str(&name.to_string().to_snake_case());
        let method_name = syn::Ident::new(&method_name, Span::call_site());

        let instr_name = match &variant.opts.dot_name {
            Some(f) => quote! { #f(expr, self) },
            None => {
                let instr = name.to_string().to_snake_case().replace("_", ".");
                quote! { self.out.push_str(#instr) }
            }
        };
        let field_edges = visit_fields(variant, false)
            .filter(|(method, _, _)| method == "visit_expr_id" || method == "visit_block_id")
            .map(|(_ty, accessor, list)| {
                if list {
                    quote! {
                        for (i, item) in expr.#accessor.iter().enumerate() {
                            let name = format!(concat!(stringify!(#accessor), "[{}]"), i);
                            self.edge(*item, &name);
                        }
                    }
                } else {
                    quote! {
                        self.edge(expr.#accessor, stringify!(#accessor));
                    }
                }
            });
        dot_methods.push(quote! {
            fn #method_name(&mut self, expr: &#name) {
                #instr_name;
                expr.visit(self);
                #(#field_edges)*
            }
        });
    }
    quote! {
        impl<'expr> Visitor<'expr> for crate::module::DotExpr<'_, 'expr> {
            fn local_function(&self) -> &'expr crate::LocalFunction {
                self.func
            }

            fn visit_expr_id(&mut self, expr: &ExprId) {
                self.expr_id(*expr);
            }

            fn visit_local_id(&mut self, local: &crate::LocalId) {
                self.id(*local);
            }

            fn visit_memory_id(&mut self, memory: &crate::MemoryId) {
                self.id(*memory);
            }

            fn visit_table_id(&mut self, table: &crate::TableId) {
                self.id(*table);
            }

            fn visit_global_id(&mut self, global: &crate::GlobalId) {
                self.id(*global);
            }

            fn visit_function_id(&mut self, function: &crate::FunctionId) {
                self.id(*function);
            }

            fn visit_type_id(&mut self, ty: &crate::TypeId) {
                self.id(*ty);
            }

            fn visit_data_id(&mut self, data: &crate::DataId) {
                self.id(*data);
            }

            fn visit_value(&mut self, value: &crate::ir::Value) {
                self.out.push_str(" ");
                self.out.push_str(&value.to_string());
            }

            #(#dot_methods)*
        }
    }
}

fn create_builder(variants: &[WalrusVariant]) -> impl quote::ToTokens {
    let mut builder_methods = Vec::new();
    for variant in variants {
        let name = &variant.syn.ident;

        let mut method_name = name.to_string().to_snake_case();
        if method_name == "return" || method_name == "const" {
            method_name.push('_');
        } else if method_name == "block" {
            continue;
        }
        let method_name = syn::Ident::new(&method_name, Span::call_site());

        let mut args = Vec::new();
        let mut arg_names = Vec::new();

        for field in variant.syn.fields.iter() {
            let name = field.ident.as_ref().expect("can't have unnamed fields");
            arg_names.push(name);
            let ty = &field.ty;
            args.push(quote! { #name: #ty });
        }

        builder_methods.push(quote! {
            pub fn #method_name(&mut self, #(#args),*) -> ExprId {
                self.alloc(#name { #(#arg_names),* }).into()
            }
        });
    }
    quote! {
        #[allow(missing_docs)]
        impl crate::FunctionBuilder {
            #(#builder_methods)*
        }
    }
}
