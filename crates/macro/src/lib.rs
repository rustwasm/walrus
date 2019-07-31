#![recursion_limit = "128"]

extern crate proc_macro;

use self::proc_macro::TokenStream;
use heck::SnakeCase;
use proc_macro2::Span;
use quote::quote;
use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::DeriveInput;
use syn::Error;
use syn::{parse_macro_input, Ident, Result, Token};

#[proc_macro_attribute]
pub fn walrus_instr(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let variants = match get_enum_variants(&input) {
        Ok(v) => v,
        Err(e) => return e.to_compile_error().into(),
    };

    assert_eq!(input.ident.to_string(), "Instr");

    let types = create_types(&input.attrs, &variants);
    let visit = create_visit(&variants);
    let builder = create_builder(&variants);

    let expanded = quote! {
        #types
        #visit
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
    skip_builder: bool,
}

#[derive(Default)]
struct WalrusFieldOpts {
    skip_visit: bool,
}

fn get_enum_variants(input: &DeriveInput) -> Result<Vec<WalrusVariant>> {
    let en = match &input.data {
        syn::Data::Enum(en) => en,
        syn::Data::Struct(_) => {
            panic!("can only put #[walrus_instr] on an enum; found it on a struct")
        }
        syn::Data::Union(_) => {
            panic!("can only put #[walrus_instr] on an enum; found it on a union")
        }
    };
    en.variants
        .iter()
        .cloned()
        .map(|mut variant| {
            Ok(WalrusVariant {
                opts: syn::parse(walrus_attrs(&mut variant.attrs))?,
                fields: variant
                    .fields
                    .iter_mut()
                    .map(|field| syn::parse(walrus_attrs(&mut field.attrs)))
                    .collect::<Result<_>>()?,
                syn: variant,
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
            SkipBuilder,
        }

        let attrs = Punctuated::<_, syn::token::Comma>::parse_terminated(input)?;
        let mut ret = WalrusVariantOpts::default();
        for attr in attrs {
            match attr {
                Attr::DisplayName(ident) => ret.display_name = Some(ident),
                Attr::DisplayExtra(ident) => ret.display_extra = Some(ident),
                Attr::SkipBuilder => ret.skip_builder = true,
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
                if attr == "skip_builder" {
                    return Ok(Attr::SkipBuilder);
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
                #( #attrs )*
                #[derive(Clone, Debug)]
                pub struct #name {
                    #( #fields )*
                }

                impl From<#name> for Instr {
                    #[inline]
                    fn from(x: #name) -> Instr {
                        Instr::#name(x)
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

            let ref_name_doc = format!(
                "
                If this instruction is a `{}`, get a shared reference to it.

                Returns `None` otherwise.
            ",
                name
            );

            let mut_name_doc = format!(
                "
                If this instruction is a `{}`, get an exclusive reference to it.

                Returns `None` otherwise.
            ",
                name
            );

            let is_name_doc = format!("Is this instruction a `{}`?", name);

            let unwrap_name_doc = format!(
                "
                Get a shared reference to the underlying `{}`.

                Panics if this instruction is not a `{}`.
            ",
                name, name
            );

            let unwrap_mut_name_doc = format!(
                "
                Get an exclusive reference to the underlying `{}`.

                Panics if this instruction is not a `{}`.
            ",
                name, name
            );

            quote! {
                #[doc=#ref_name_doc]
                #[inline]
                fn #ref_name(&self) -> Option<&#name> {
                    if let Instr::#name(ref x) = *self {
                        Some(x)
                    } else {
                        None
                    }
                }

                #[doc=#mut_name_doc]
                #[inline]
                pub fn #mut_name(&mut self) -> Option<&mut #name> {
                    if let Instr::#name(ref mut x) = *self {
                        Some(x)
                    } else {
                        None
                    }
                }

                #[doc=#is_name_doc]
                #[inline]
                pub fn #is_name(&self) -> bool {
                    self.#ref_name().is_some()
                }

                #[doc=#unwrap_name_doc]
                #[inline]
                pub fn #unwrap_name(&self) -> &#name {
                    self.#ref_name().unwrap()
                }

                #[doc=#unwrap_mut_name_doc]
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
        pub enum Instr {
            #(#variants),*
        }

        impl Instr {
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
            _ => panic!("invalid path in #[walrus_instr]"),
        };
        let mut ty = match args.first().unwrap().into_value() {
            syn::GenericArgument::Type(ty) => ty,
            _ => panic!("invalid path in #[walrus_instr]"),
        };
        if let syn::Type::Slice(t) = ty {
            ty = &t.elem;
        }
        match ty {
            syn::Type::Path(p) => {
                let segment = p.path.segments.last().unwrap().into_value();
                (&segment.ident, true)
            }
            _ => panic!("invalid path in #[walrus_instr]"),
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

        let mut method_name = "visit_".to_string();
        method_name.push_str(&name.to_string().to_snake_case());
        let method_name = syn::Ident::new(&method_name, Span::call_site());
        let method_name_mut = syn::Ident::new(&format!("{}_mut", method_name), Span::call_site());

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
            impl<'instr> Visit<'instr> for #name {
                fn visit<V: Visitor<'instr>>(&self, visitor: &mut V) {
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
        visitor_trait_methods.push(quote! {
            #[doc=#doc]
            fn #method_name(&mut self, instr: &#name) {
                instr.visit(self);
            }
        });
        visitor_mut_trait_methods.push(quote! {
            #[doc=#doc]
            fn #method_name_mut(&mut self, instr: &mut #name) {
                instr.visit_mut(self);
            }
        });

        let mut method_name = "visit_".to_string();
        method_name.push_str(&name.to_string().to_snake_case());
        let method_name = syn::Ident::new(&method_name, Span::call_site());
        visit_impl.push(quote! {
            Instr::#name(e) => visitor.#method_name(e),
        });
        visit_mut_impl.push(quote! {
            Instr::#name(e) => visitor.#method_name_mut(e),
        });
    }

    quote! {
        /// A visitor walks over an IR instruction tree.
        pub trait Visitor<'instr>: Sized {
            /// Return the local function we're visiting
            fn local_function(&self) -> &'instr crate::LocalFunction;

            /// Visit `Instr`.
            fn visit_instr(&mut self, instr: &'instr Instr) {
                instr.visit(self)
            }

            /// Visit `InstrSeq`.
            fn visit_instr_seq(&mut self, instr_seq: &'instr InstrSeq) {
                for i in &instr_seq.instrs {
                    self.visit_instr(i);
                }
            }

            /// Visit `InstrSeqId`.
            fn visit_instr_seq_id(&mut self, instr_seq_id: &InstrSeqId) {
                instr_seq_id.visit(self);
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

        /// A visitor walks over a mutable IR instruction tree.
        pub trait VisitorMut: Sized {
            /// Return the local function we're visiting
            fn local_function_mut(&mut self) -> &mut crate::LocalFunction;

            /// Visit `Instr`.
            fn visit_instr_mut(&mut self, instr: &mut Instr) {
                instr.visit_mut(self);
            }

            /// Visit `InstrSeq`.
            fn visit_instr_seq_mut(&mut self, instr_seq: &mut InstrSeq) {
                for i in &mut instr_seq.instrs {
                    i.visit_mut(self);
                }
            }

            /// Visit `InstrSeqId`.
            fn visit_instr_seq_id_mut(&mut self, instr_seq_id: &mut InstrSeqId) {
                // ...
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

        impl<'instr> Visit<'instr> for Instr {
            fn visit<V>(&self, visitor: &mut V) where V: Visitor<'instr> {
                match self {
                    #( #visit_impl )*
                }
            }
        }

        impl VisitMut for Instr {
            fn visit_mut<V>(&mut self, visitor: &mut V) where V: VisitorMut {
                match self {
                    #( #visit_mut_impl )*
                }
            }
        }

        #( #visit_impls )*
    }
}

fn create_builder(variants: &[WalrusVariant]) -> impl quote::ToTokens {
    let mut builder_methods = Vec::new();
    for variant in variants {
        if variant.opts.skip_builder {
            continue;
        }

        let name = &variant.syn.ident;

        let mut method_name = name.to_string().to_snake_case();

        let mut method_name_at = method_name.clone();
        method_name_at.push_str("_at");
        let method_name_at = syn::Ident::new(&method_name_at, Span::call_site());

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

        let doc = format!(
            "Push a new `{}` instruction onto this builder's block.",
            name.to_string()
        );
        let at_doc = format!(
            "Splice a new `{}` instruction into this builder's block at the given index.\n\n\
             # Panics\n\n\
             Panics if `position > self.instrs.len()`.",
            name.to_string()
        );

        let arg_names = &arg_names;
        let args = &args;

        builder_methods.push(quote! {
            #[inline]
            #[doc=#doc]
            pub fn #method_name(&mut self, #(#args),*) -> &mut Self {
                self.instr(#name { #(#arg_names),* })
            }

            #[inline]
            #[doc=#at_doc]
            pub fn #method_name_at(&mut self, position: usize, #(#args),*) -> &mut Self {
                self.instr_at(position, #name { #(#arg_names),* })
            }
        });
    }
    quote! {
        #[allow(missing_docs)]
        impl crate::InstrSeqBuilder<'_> {
            #(#builder_methods)*
        }
    }
}
