use std::unimplemented;

use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, AngleBracketedGenericArguments, Attribute,
    Data, DataEnum, DataStruct, DeriveInput, Error, Expr, Field, Fields, GenericArgument, Ident,
    ItemEnum, ItemStruct, Meta, MetaList, NestedMeta, PathArguments, Stmt, Type, TypeArray,
    TypeParen, TypePath, TypeReference, TypeSlice, Variant,
};

#[proc_macro_derive(Parse, attributes(len, structr))]
pub fn derive_parse_from_item_stream(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    // Hand the output tokens back to the compiler
    match try_derive_parse_from_item(input) {
        Ok(stream) => stream.into(),
        Err(mut err) => {
            err.combine(Error::new(
                Span::call_site(),
                "... while parsing this macro",
            ));
            err.into_compile_error().into()
        }
    }
}

#[derive(Default)]
struct FieldAttributeSpec {
    with: Option<Type>,
    run: Option<Stmt>,
    parse: Option<Expr>,
    len: Option<Type>,
    eq: Option<Expr>,
}

impl FieldAttributeSpec {
    fn get_structr_attribute(attr: &Attribute) -> Result<Meta, syn::Error> {
        let meta = attr.parse_meta()?;
        let ident = meta
            .path()
            .get_ident()
            .ok_or_else(|| Error::new(meta.span(), "Expected path to be identifier"))?;
        if ident.to_string() != "structr" {
            return Err(Error::new(meta.span(), "Expected ident to be \"structr\""));
        }
        Ok(meta)
    }

    fn get_structr_list(meta: Meta) -> Result<MetaList, syn::Error> {
        match meta {
            Meta::List(list) => Ok(list),
            _ => Err(Error::new(
                meta.span(),
                "Expected structr attribute to be a list: #[structr(like, this)]",
            )),
        }
    }

    fn parse_attrs(attrs: &[Attribute]) -> Result<Self, syn::Error> {
        let mut field_structr_attrs = Self::default();
        for structr_meta in attrs
            .iter()
            .map(Self::get_structr_attribute)
            .filter_map(Result::ok)
        {
            let list = Self::get_structr_list(structr_meta)?;
            for item in list.nested {
                let nested_meta = match item {
                    NestedMeta::Meta(meta) => meta,
                    _ => {
                        return Err(Error::new(
                            item.span(),
                            "Unexpected string literal in structr attribute",
                        ))
                    }
                };
                let name_value = match nested_meta {
                    Meta::NameValue(name_value) => name_value,
                    _ => {
                        return Err(Error::new(
                            nested_meta.span(),
                            "Expected structr(name = value)",
                        ))
                    }
                };
                let ident = name_value.path.get_ident().ok_or_else(|| {
                    Error::new(
                        name_value.span(),
                        "Unexpected name to be path in structr(name = value)",
                    )
                })?;
                let get_str_lit = || match &name_value.lit {
                    syn::Lit::Str(str) => Ok(str),
                    _ => {
                        return Err(Error::new(
                            ident.span(),
                            format!(
                                "Expected structr \"{}\" attribute to have string literal as value",
                                ident.to_string()
                            ),
                        ))
                    }
                };
                match ident.to_string().as_ref() {
                    "with" => field_structr_attrs.with = Some(get_str_lit()?.parse()?),
                    "run" => field_structr_attrs.run = Some(get_str_lit()?.parse()?),
                    "len" => field_structr_attrs.len = Some(get_str_lit()?.parse()?),
                    "eq" => field_structr_attrs.eq = Some(get_str_lit()?.parse()?),
                    "parse" => {
                        field_structr_attrs.parse = Some(get_str_lit()?.parse()?);
                    }
                    other => {
                        return Err(Error::new(
                            ident.span(),
                            format!("Unexpected structr attribute {}", other),
                        ))
                    }
                }
            }
        }
        Ok(field_structr_attrs)
    }
}

enum DeriveInputItem {
    Struct(ItemStruct),
    Enum(ItemEnum),
    //Union(ItemUnion)
}

impl From<DeriveInput> for DeriveInputItem {
    fn from(value: DeriveInput) -> Self {
        let DeriveInput {
            attrs,
            vis,
            ident,
            generics,
            data,
        } = value;
        match data {
            Data::Struct(DataStruct {
                struct_token,
                fields,
                semi_token,
            }) => DeriveInputItem::Struct(ItemStruct {
                attrs,
                fields,
                generics,
                ident,
                semi_token,
                struct_token,
                vis,
            }),
            Data::Enum(DataEnum {
                enum_token,
                brace_token,
                variants,
            }) => DeriveInputItem::Enum(ItemEnum {
                attrs,
                vis,
                enum_token,
                ident,
                generics,
                brace_token,
                variants,
            }),
            Data::Union(_) => {
                unimplemented!()
            }
        }
    }
}

struct FieldInfo<'a> {
    field: &'a Field,
    some_ident: Ident,
    _field_index: usize,
    attrs: FieldAttributeSpec,
    _fields: &'a Fields,
    _debugging: bool,
    _binding: bool,
    _hidden: bool,
}

fn get_type_ident(type_: &Type) -> String {
    match type_ {
        Type::Array(TypeArray { elem, len, .. }) => {
            format!("[{}; {}]", elem.to_token_stream(), len.to_token_stream())
        }
        Type::Paren(TypeParen { elem, .. }) => get_type_ident(elem),
        Type::Path(TypePath { path, .. }) => {
            let mut s = String::new();
            for segment in path.segments.iter() {
                s += (&segment.ident).into_token_stream().to_string().as_ref();
                match &segment.arguments {
                    PathArguments::None => {}
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        args, ..
                    }) => {
                        s += "<";
                        for arg in args.iter() {
                            match arg {
                                GenericArgument::Type(type_) => {
                                    s += &get_type_ident(type_);
                                }
                                GenericArgument::Lifetime(lifetime) => {
                                    s += &format!("&{}", lifetime.ident.to_token_stream())
                                }
                                other => unimplemented!("{:?}", other),
                            }
                        }
                        s += ">";
                    }
                    PathArguments::Parenthesized(_) => {
                        unimplemented!("Parenthesized")
                    }
                }
            }
            format!("{}", path.to_token_stream())
        }
        Type::Reference(TypeReference {
            and_token,
            elem,
            mutability,
            ..
        }) => {
            format!(
                "{}{}{}",
                and_token.to_token_stream(),
                if let Some(mutability) = mutability {
                    mutability.to_token_stream().to_string() + " "
                } else {
                    "".to_string()
                },
                elem.to_token_stream()
            )
        }
        Type::Slice(TypeSlice { elem, .. }) => {
            format!("[{}]", elem.to_token_stream())
        }
        Type::Tuple(_) => "unknown".to_owned(),
        _ => "unknown".to_owned(),
    }
}

fn derive_field_value(
    FieldInfo {
        field: Field { ty, .. },
        some_ident: ident,
        attrs,
        ..
    }: &FieldInfo,
) -> Result<TokenStream, Error> {
    let mut stream = TokenStream::new();
    let ty_ident = get_type_ident(ty);
    quote!(parser.ctx_field_start(stringify!(#ident), #ty_ident);).to_tokens(&mut stream);
    attrs.run.to_tokens(&mut stream);
    let parse = if let Some(parse) = attrs.parse.as_ref() {
        quote! {#parse?}
    } else if let Some(len) = &attrs.len {
        quote_spanned!(len.span()=>
            {
                let mut #ident = Vec::new();
                let len: #len = parser.parse()?;
                for _ in 0..len {
                    #ident.push(parser.parse()?);
                }
                #ident
            }
        )
    } else {
        quote_spanned! {ident.span()=> parser.parse()? }
    };
    quote!(
        let #ident: #ty = #parse;
    )
    .to_tokens(&mut stream);
    if let Some(eq) = &attrs.eq {
        quote!(
            if #ident != #eq {
                return Err(Parser::error(ParseErrorKind::NotEqual(format!("{:?}", #ident), format!("{:?}", #eq))))
            }
        )
        .to_tokens(&mut stream);
    }
    quote!(
        parser.ctx_field_end();
        parser.ctx_index();
    )
    .to_tokens(&mut stream);
    Ok(stream)
}

fn derive_field_assignment(
    FieldInfo {
        field,
        attrs,
        some_ident,
        ..
    }: &FieldInfo,
) -> Result<TokenStream, Error> {
    let Field { ident, .. } = field;
    Ok(if let Some(with_type) = &attrs.with {
        quote_spanned! {with_type.span()=>
            #ident: #ident.into()
        }
    } else {
        if let Some(ident) = ident {
            quote_spanned! {field.span()=>
                #ident
            }
        } else {
            quote_spanned! {field.span()=>
                #some_ident
            }
        }
    })
}

fn derive_struct(struct_input: ItemStruct) -> Result<TokenStream, Error> {
    let mut stream = TokenStream::new();
    let complete_ident = &struct_input.ident;
    let field_info = struct_input
        .fields
        .iter()
        .enumerate()
        .map(|(i, field)| -> Result<FieldInfo, Error> {
            let attrs = FieldAttributeSpec::parse_attrs(&field.attrs)?;
            let some_ident = if let Some(ident) = &field.ident {
                ident.to_owned()
            } else {
                Ident::new(&format!("unnamed_{}", i), field.ty.span())
            };
            Ok(FieldInfo {
                attrs,
                field,
                some_ident,
                _field_index: i,
                _fields: &struct_input.fields,
                _debugging: false,
                _hidden: false,
                _binding: false,
            })
        })
        .collect::<Result<Vec<FieldInfo>, Error>>()?;
    let field_assignment = field_info
        .iter()
        .map(derive_field_assignment)
        .collect::<Result<Vec<_>, _>>()?;
    let field_parsing = field_info
        .iter()
        .map(derive_field_value)
        .collect::<Result<Vec<_>, _>>()?;
    let required_lifetime = if let Some(lifetime_def) = struct_input.generics.lifetimes().next() {
        let lifetime = &lifetime_def.lifetime;
        quote_spanned! {lifetime.span()=>
            #lifetime
        }
    } else {
        quote! {
            'p
        }
    };
    let (_g_impl, g_ty, g_where) = struct_input.generics.split_for_impl();
    let result_fields = if let Fields::Unnamed(_) = &struct_input.fields {
        quote!(
            (#(#field_assignment),*)
        )
    } else {
        quote!(
            {
                #(#field_assignment),*
            }
        )
    };
    stream.extend(quote!{
        impl<#required_lifetime> Parse<#required_lifetime> for #complete_ident#g_ty #g_where {
            fn parse<'q>(parser: &mut Parser<#required_lifetime>) -> Result<Self, ParseError<#required_lifetime>> where #required_lifetime: 'q {
                parser.ctx_struct_start(stringify!(#complete_ident));
                #(#field_parsing)*
                parser.ctx_struct_end();
                Ok(#complete_ident #result_fields)
            }
        }
    });
    Ok(stream)
}

fn generate_variants_only_enum(complete: &ItemEnum) -> Result<(ItemEnum, TokenStream), Error> {
    let complete_ident = &complete.ident;
    let mut variants_only = complete.to_owned();
    variants_only.ident = format_ident!("{}{}", complete_ident, "Variants");
    let (g_impl, g_ty, g_where) = complete.generics.split_for_impl();
    variants_only.generics.params.clear();
    let mut what = Vec::new();
    for Variant {
        ref mut fields,
        ident,
        ..
    } in variants_only.variants.iter_mut()
    {
        what.push(quote! { #complete_ident::#ident(_) => Self::#ident, });
        *fields = Fields::Unit;
    }
    variants_only.attrs.push(parse_quote! { #[derive(Debug)] });
    let variants_only_ident = variants_only.ident.to_owned();
    Ok((variants_only, quote! {
        impl#g_impl From<&#complete_ident#g_ty> for #variants_only_ident #g_where {
            fn from(full: &#complete_ident#g_ty) -> #variants_only_ident {
                match full {
                    #(#what)*
                }
            }
        }
    }))
}

fn try_get_discriminants(enum_: &ItemEnum) -> Option<(Type, Vec<(Expr, Ident)>)> {
    let mut other = None;
    let mut type_: Option<Type> = None;
    for attr in enum_.attrs.iter() {
        if let Some(ident) = attr.path.get_ident() {
            if ident == "repr" {
                type_ = Some(attr.parse_args().unwrap())
            }
        }
    }
    let mut expr_variant_map = Vec::<(Expr, Ident)>::new();
    for variant in enum_.variants.iter() {
        if let Fields::Unit = variant.fields {
            if let Some((_, expr)) = &variant.discriminant {
                expr_variant_map.push((expr.to_owned(), variant.ident.to_owned()));
            }
        } else {
            if let None = other {
                other = Some(variant.to_owned());
            } else {
                return None;
            }
        }
    }
    if let Some(type_) = type_ {
        Some((type_, expr_variant_map))
    } else {
        None
    }
}

fn derive_enum(enum_: ItemEnum) -> Result<TokenStream, Error> {
    let mut stream = TokenStream::new();
    let enum_ident = &enum_.ident;
    if let Some((type_, expr_variant_map)) = try_get_discriminants(&enum_) {
        let variant_parse = expr_variant_map
            .iter()
            .map(|(expr, ident)| {
                quote_spanned! {ident.span()=>
                    #expr => Self::#ident,
                }
            })
            .collect::<Vec<_>>();
        let required_lifetime = if let Some(lifetime_def) = enum_.generics.lifetimes().next() {
            let lifetime = &lifetime_def.lifetime;
            quote_spanned! {lifetime.span()=>
                #lifetime
            }
        } else {
            quote! {
                'p
            }
        };
        let (_g_impl, g_ty, g_where) = enum_.generics.split_for_impl();
        (quote!{
            impl<#required_lifetime> Parse<#required_lifetime> for #enum_ident#g_ty #g_where {
                fn parse<'q>(parser: &mut Parser<#required_lifetime>) -> Result<Self, ParseError<#required_lifetime>> where #required_lifetime: 'q {
                    Ok(match parser.parse::<#type_>()? {
                        #(#variant_parse)*
                        other => return Err(Parser::error(ParseErrorKind::NoReprIntMatch(other.try_into().unwrap())))
                    })
                }
            }
        }).to_tokens(&mut stream);
    } else {
        let (variants_only, variants_only_impl) = generate_variants_only_enum(&enum_)?;
        let variants_only_ident = &variants_only.ident;
        variants_only.to_tokens(&mut stream);
        variants_only_impl.to_tokens(&mut stream);
        let variant_parse = enum_
            .variants
            .iter()
            .map(|variant| {
                let ident = &variant.ident;
                quote_spanned! {ident.span()=>
                    #variants_only_ident::#ident => Self::#ident(parser.parse()?)
                }
            })
            .collect::<Vec<_>>();
        let required_lifetime = if let Some(lifetime_def) = enum_.generics.lifetimes().next() {
            let lifetime = &lifetime_def.lifetime;
            quote_spanned! {lifetime.span()=>
                #lifetime
            }
        } else {
            quote! {
                'p
            }
        };
        let (_g_impl, g_ty, g_where) = enum_.generics.split_for_impl();
        (quote!{
            impl<#required_lifetime> #enum_ident#g_ty #g_where {
                fn parse<'q>(parser: &mut Parser<#required_lifetime>, variant: #variants_only_ident) -> Result<Self, ParseError<#required_lifetime>> where #required_lifetime: 'q {
                    Ok(match variant {
                        #(#variant_parse),*
                    })
                }
            }
        }).to_tokens(&mut stream);
    }
    Ok(stream)
}

fn try_derive_parse_from_item(input: DeriveInput) -> Result<TokenStream, Error> {
    Ok(match DeriveInputItem::from(input) {
        DeriveInputItem::Struct(struct_) => derive_struct(struct_)?,
        DeriveInputItem::Enum(enum_) => derive_enum(enum_)?,
    })
}
