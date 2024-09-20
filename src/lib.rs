/*****************************************************************************
 MIT License

Copyright (c) 2024 Bruce Skingle

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
******************************************************************************/

use proc_macro2::{Ident, Literal, TokenStream};
use quote::{quote, quote_spanned};
use syn::{DeriveInput, Data, Fields, Type};
use syn::spanned::Spanned;
use inflections::case::to_camel_case;

struct ParsedType {
    type_name: String, 
    scalar: bool,
    page_forward: bool,
    page_reverse: bool
}

fn is_graphql_simple_type_name(name: &str) -> bool {
    match name {
        "ID" => true,
        "String" => true,
        "Int" => true,
        "i32" => true,
        "i16" => true,
        "i8" => true,
        "Float" => true,
        "f64" => true,
        "f32" => true,
        "Boolean" => true,
        "bool" => true,
        "Decimal" => true,
        "Date" => true,
        "DateTime" => true,
        _ => false,
    }
}

fn parse_type(ty: &Type) -> Option<ParsedType> {
    if let Type::Path(path_type) = ty {
        if path_type.qself.is_some() {
            return None
        }

        let path = &path_type.path;

        if path.leading_colon.is_some() {
            return None
        }

        if path.segments.len() != 1 {
            return None
        }

        let segment = &path.segments.iter().next().unwrap();

        let name = segment.ident.to_string();

        match &segment.arguments {
            syn::PathArguments::None => {},
            syn::PathArguments::AngleBracketed(args) => {
                if args.args.len() != 1 {
                    return None
                }
                let arg = args.args.iter().next().unwrap();

                if let syn::GenericArgument::Type(item) = arg {
                    if let Type::Path(path_item) = item {
                        if path_item.path.segments.len() != 1 {
                            return None
                        }
                
                        let segment = &path_item.path.segments.iter().next().unwrap();
                
                        let parameter_name = segment.ident.to_string();

                        return Some(match name.as_str() {
                            "Option" =>  ParsedType{
                                scalar: is_graphql_simple_type_name(&parameter_name),
                                type_name: parameter_name,
                                page_forward: false,
                                page_reverse: false,
                            },
                            "Vec" =>  ParsedType{
                                scalar: is_graphql_simple_type_name(&parameter_name),
                                type_name: parameter_name,
                                page_forward: false,
                                page_reverse: false,
                            },
                            "ForwardPageOf" => ParsedType{
                                type_name: parameter_name,
                                scalar: false,
                                page_forward: true,
                                page_reverse: false,
                            },
                            "ReversePageOf" => ParsedType{
                                type_name: parameter_name,
                                scalar: false,
                                page_forward: false,
                                page_reverse: true,
                            },
                            "PageOf" => ParsedType{
                                type_name: parameter_name,
                                scalar: false,
                                page_forward: true,
                                page_reverse: true,
                            },
                            _ => ParsedType{
                                type_name: format!("{}<{}>", name, parameter_name),
                                scalar: false,
                                page_forward: false,
                                page_reverse: false,
                            },
                        });
                    }
                    else {
                        return None
                    }
                }
                else {
                    return None
                }
            },
            syn::PathArguments::Parenthesized(_) => {
                return None
            },
        }

        return Some(ParsedType{
            scalar: is_graphql_simple_type_name(&name),
            type_name: name,
            page_forward: false,
            page_reverse: false,
        });

    }
    else {
        // Not interesting to us
        return None
    }
}

#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(graphql))]
struct GraphQLDeriveParams {
    params: String,
    super_type: Option<Vec<String>>
}

#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(serde))]
struct SerdeDeriveParams {
    rename_all: Option<String>,
    rename_all_fields: Option<String>,
    deny_unknown_fields: Option<String>,
    tag: Option<String>,
    content: Option<String>,
    untagged: Option<bool>,
    bound: Option<String>,
    default: Option<bool>,
    remote: Option<String>,
    transparent: Option<bool>,
    from: Option<String>,
    try_from: Option<String>,
    into: Option<String>,
    // r#crate: Option<String>,
    expecting: Option<String>,
}

#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(graphql))]
struct GraphQLDeriveAttributeParams {
    #[deluxe(default = false)]
    required: bool,

    #[deluxe(default = false)]
    scalar: bool,

    #[deluxe(default = false)]
    no_params: bool,

    rename: Option<String>
}

#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(serde))]
struct SerdeDeriveAttributeParams {
    #[deluxe(default = false)]
    flatten: bool,
    rename: Option<String>
}

fn derive_graphql_type2(item: proc_macro2::TokenStream) -> deluxe::Result<proc_macro2::TokenStream> {
    let mut ast: DeriveInput = syn::parse2(item)?;

    // Extract the attributes!
    let derive_params: GraphQLDeriveParams = deluxe::extract_attributes(&mut ast)?;
    let serde_derive_params: SerdeDeriveParams = deluxe::extract_attributes(&mut ast)?;

    let params_ident = Ident::new(&derive_params.params, ast.span());
    // define impl variables

    let ident = ast.ident;

    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();


    let (names, args) = impl_graphql_type_params(&mut ast.data, &ident, &derive_params, &serde_derive_params)?;


    let lit = proc_macro2::Literal::string(&names);
    let expanded = quote! {
        impl #impl_generics GraphQLType<#params_ident> for #ident #ty_generics #where_clause {
            fn get_query_attributes(params: &#params_ident, prefix: &str) -> String {
                // #lit.to_string(#args)
                format!(#lit, #args)
            }
        }};




        eprintln!("\n\n\n\nTYPE TOKENS\n{}\n\n\n\n\n", expanded);
    
    
    
        // Hand the output tokens back to the compiler.
       Ok(expanded)
}

#[proc_macro_derive(GraphQLType, attributes(graphql))]
pub fn derive_graphql_type(item: proc_macro::TokenStream) -> proc_macro::TokenStream {

    derive_graphql_type2(item.into()).unwrap().into()
}

fn get_graphql_type_parts(fields: &mut syn::punctuated::Punctuated<syn::Field, syn::token::Comma>, struct_name: &Ident) -> 
deluxe::Result<(String, TokenStream)> {
    let mut names = String::new();
    let mut args: Vec<TokenStream> = Vec::new();
    let mut has_fields = false;
    let mut has_flattened = false;

         
    // names.push_str("{{\n");
    for f in fields {
        let attrs: GraphQLDeriveAttributeParams = deluxe::extract_attributes(f)?;
        let serde_attrs: SerdeDeriveAttributeParams = deluxe::extract_attributes(f)?;

       

        let ident = &f.ident;

        let ident = if let Some(ident) = ident {
            ident
        }
        else {
            panic!("Unnamed struct field");
        };

        let mut name = ident.to_string();

if serde_attrs.flatten {
    eprintln!("FLATTEN {}", name)
}

        if !name.starts_with("__") {
            name = to_camel_case(&name);
        }
        let field_name: Literal = Literal::string(&name);
        let camel_name = Literal::string(&to_camel_case(&name));

        eprintln!(" name {}", &name);
        eprintln!(" field_name {}", &field_name);
        eprintln!(" name {}", &camel_name);

        eprintln!("FIELD {}", name);

        let parsed_type = parse_type(&f.ty);
        if let Some(parsed_type) = parsed_type {
            if serde_attrs.flatten {
                has_flattened = true;
                names.push_str(&format!("# flattened {}\n", name))
            }
            else {
                has_fields = true;
                if attrs.scalar || parsed_type.scalar {
                    names.push_str(&name);
                    names.push('\n');
        
                    eprintln!(" GQL type {}", parsed_type.type_name.to_string());
                }
                else {
                    // eprintln!(" other type container {:?}", info.container_type);
                    eprintln!(" other type name {}", parsed_type.type_name);
                    let type_name = Ident::new(&parsed_type.type_name, f.span().clone());

                    names.push_str(&name);
                    // names.push_str("{}{{\n");
                    names.push_str("{}\n");

                    if parsed_type.page_forward || parsed_type.page_reverse {
                        names.push_str(&format!("  # pageOf {}\n", camel_name));
                        names.push_str("  {{ # pageOf\n");
                        names.push_str("    pageInfo {{\n");
                        if parsed_type.page_forward {
                            names.push_str("        startCursor\n");
                            names.push_str("        hasNextPage\n");
                        }
                        if parsed_type.page_reverse {

                            names.push_str("        endCursor\n");
                            names.push_str("        hasPreviousPage\n");
                        }
                        names.push_str("    }}\n");
                        names.push_str("    edges {{ # pageOf.edges\n");

                        names.push_str(&format!("  # pageOf.node {}\n", camel_name));
                        names.push_str("        node {}\n");

                        names.push_str(&format!("  # /pageOf.node {}\n", camel_name));
                        names.push_str("    }} # /pageOf.edges\n");
                        names.push_str("  }} # /pageOf\n");

                        names.push_str(&format!("  # /pageOf {}\n", camel_name));
                    }
                    else {          
                        names.push_str(&format!("  # object {}\n", camel_name));     
                        // names.push_str("  {{ # object\n");     
                        names.push_str("    {}\n");
                        // names.push_str("  }} # /object\n");
                            
                        names.push_str(&format!("  # /object {}\n", camel_name));  
                    }
                    // names.push_str("}}\n");
        
                    let arg = if attrs.no_params {
                        quote_spanned! {f.span()=>
                            "",
                            #type_name::get_query_part(&NoParams, &GraphQL::prefix(prefix, #field_name))
                        }

                    } 
                    else { 
                        quote_spanned! {f.span()=>
                            params.#ident.get_actual(&GraphQL::prefix(prefix, #field_name)),
                            #type_name::get_query_part(&params.#ident, &GraphQL::prefix(prefix, #field_name))
                        }
                    };
        
                    eprintln!(" other type {}", &name);
                    eprintln!("        arg {}", &arg);
                
                    args.push(arg);
                }
            }
        }
        else {
            panic!("Unrecognised type {:?}", &f.ty);
        }
    }
    // names.push_str("}}\n");

    if has_fields && has_flattened {
        names = format!("...on {} {{{{\n{}\n}}}}\n", struct_name.to_string(), names);
    }
    
    Ok((names, quote!{#(#args,)*}))
}

fn get_graphql_type_enum_parts(variants: &mut syn::punctuated::Punctuated<syn::Variant, syn::token::Comma>, struct_name: &Ident, derive_params: &GraphQLDeriveParams, serde_derive_params: &SerdeDeriveParams) -> 
deluxe::Result<(String, TokenStream)> {
    let mut names = String::new();
    let mut args: Vec<TokenStream> = Vec::new();

    if let Some(tag) = &serde_derive_params.tag {
        names.push_str(&tag);
        names.push('\n');
    }
    // names.push_str(&format!("/* super {:?}*/", derive_params.super_type));
    if let Some(super_types) = &derive_params.super_type {
        for super_type in super_types {
            names.push_str("{}\n");

            let super_type_name = Ident::new(&super_type, variants.span().clone());
            
            
            let arg = 
                quote_spanned! {variants.span()=>
                    #super_type_name::get_query_attributes(params, prefix) //&GraphQL::prefix(prefix, #variant_name))
                };
        
            args.push(arg);
        }
    }
         
    // names.push_str("{{\n");
    for v in variants {
        let attrs: GraphQLDeriveAttributeParams = deluxe::extract_attributes(v)?;
        let serde_attrs: SerdeDeriveAttributeParams = deluxe::extract_attributes(v)?;

       

        let ident = &v.ident;

        let mut name = ident.to_string();

if serde_attrs.flatten {
    eprintln!("FLATTEN {}", name)
}

        if !name.starts_with("__") {
            name = to_camel_case(&name);
        }
        let variant_name: Literal = Literal::string(&name);
        let camel_name = Literal::string(&to_camel_case(&name));

        eprintln!(" name {}", &name);
        eprintln!(" variant_name {}", &variant_name);
        eprintln!(" name {}", &camel_name);

        eprintln!("FIELD {}", name);

        let fields = &v.fields;

        match &mut v.fields {
            Fields::Named(fields) => {
                panic!("Named fields in enum not supported");
            }
            Fields::Unnamed(ref fields) => {

                let mut i = 0;
                for f in &fields.unnamed {
                    if i == 0 {
                        i = 1;
                    }
                    else {
                        panic!("Multiple fields in enum not supported");
                    }
                }

                let the_field = fields.unnamed.iter().next().unwrap();

                

                let parsed_type = parse_type(&the_field.ty);
                if let Some(parsed_type) = parsed_type {
                    if attrs.scalar || parsed_type.scalar {
                        panic!("Scalar filed in enum not supported");

                        // maybe this would work:
                        // names.push_str(&name);
                        // names.push('\n');
            
                        // eprintln!(" GQL type {}", parsed_type.type_name.to_string());
                    }
                    else {
                        eprintln!(" other type name {}", parsed_type.type_name);
                        let type_name = Ident::new(&parsed_type.type_name, v.span().clone());

                        names.push_str(&format!("  # enum variant {}\n", name));
                        names.push_str("  {}\n");


                        names.push_str(&format!("  # /enum variant {}\n", name));


                        // names.push_str(&name);
                        // // names.push_str("{}{{\n");
                        // names.push_str("{}\n");

                        // if parsed_type.page_forward || parsed_type.page_reverse {
                        //     names.push_str(&format!("  # pageOf {}\n", camel_name));
                        //     names.push_str("  {{ # pageOf\n");
                        //     names.push_str("    pageInfo {{\n");
                        //     if parsed_type.page_forward {
                        //         names.push_str("        startCursor\n");
                        //         names.push_str("        hasNextPage\n");
                        //     }
                        //     if parsed_type.page_reverse {

                        //         names.push_str("        endCursor\n");
                        //         names.push_str("        hasPreviousPage\n");
                        //     }
                        //     names.push_str("    }}\n");
                        //     names.push_str("    edges {{ # pageOf.edges\n");

                        //     names.push_str(&format!("  # pageOf.node {}\n", camel_name));
                        //     names.push_str("        node {}\n");

                        //     names.push_str(&format!("  # /pageOf.node {}\n", camel_name));
                        //     names.push_str("    }} # /pageOf.edges\n");
                        //     names.push_str("  }} # /pageOf\n");

                        //     names.push_str(&format!("  # /pageOf {}\n", camel_name));
                        // }
                        // else {          
                        //     names.push_str(&format!("  # object {}\n", camel_name));     
                        //     // names.push_str("  {{ # object\n");     
                        //     names.push_str("    {}\n");
                        //     // names.push_str("  }} # /object\n");
                                
                        //     names.push_str(&format!("  # /object {}\n", camel_name));  
                        // }
                        // // names.push_str("}}\n");
            
                        let arg = 
                            quote_spanned! {v.span()=>
                                #type_name::get_query_attributes(params, prefix) //&GraphQL::prefix(prefix, #variant_name))
                            };
                    
                        args.push(arg);
                    }
                }
                else {
                    panic!("Unrecognised type {:?}", &the_field.ty);
                }
            }
            Fields::Unit => {
                panic!("Unit field in enum not supported");
            }
        }
        
    }
    
    Ok((names, quote!{#(#args,)*}))
}



fn impl_graphql_type_params(data: &mut Data, struct_name: &Ident, derive_params: &GraphQLDeriveParams, serde_derive_params: &SerdeDeriveParams) -> deluxe::Result<(String, TokenStream)> {
    match data {
        Data::Struct(data) => {
            match &mut data.fields {
                Fields::Named(fields) => {
                    get_graphql_type_parts(&mut fields.named, struct_name)
                }
                Fields::Unnamed(ref _fields) => {
                    panic!("Unnamed struct fields not supported");
                }
                Fields::Unit => {
                    panic!("Unit struct fields not supported");
                }
            }
        },
        Data::Enum(data) => {
            get_graphql_type_enum_parts(&mut data.variants, struct_name, derive_params, serde_derive_params)
        },
        Data::Union(_) => panic!("Unions not supported"),
    }
}


fn derive_graphql_query_params2(item: proc_macro2::TokenStream) -> deluxe::Result<proc_macro2::TokenStream> {
    let mut ast: DeriveInput = syn::parse2(item)?;

    // Extract the attributes!
    // let GraphQLDeriveParams { params } = deluxe::extract_attributes(&mut ast)?;
    // let params_ident = Ident::new(&params, ast.span());

    // define impl variables

    let ident = ast.ident;

    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    // generate

    
    let (formal, actual, variable) = impl_graphql_query_params(&mut ast.data)?;

    let expanded = quote! {
        // The generated impl.
        impl #impl_generics GraphQLQueryParams for #ident #ty_generics #where_clause {
            fn get_formal_part(&self, params: &mut ParamBuffer, prefix: &str) {
                #formal
            }

            fn get_actual_part(&self, params: &mut ParamBuffer, prefix: &str){
                #actual
                
            }
        
            fn get_variables_part(&self, variables: &mut VariableBuffer, prefix: &str) -> Result<(), serde_json::Error> {
                #variable
                Ok(())
            }
        }
    };


    eprintln!("\n\n\n\nTOKENS\n{}\n\n\n\n\n", expanded);
    
    
    
        // Hand the output tokens back to the compiler.
       Ok(expanded)
}

#[proc_macro_derive(GraphQLQueryParams, attributes(graphql))]
pub fn derive_graphql_query_params(item: proc_macro::TokenStream) -> proc_macro::TokenStream {

    derive_graphql_query_params2(item.into()).unwrap().into()
}

fn get_graphql_query_enum_parts(variants: &syn::punctuated::Punctuated<syn::Variant, syn::token::Comma>)  -> 
    deluxe::Result<(TokenStream, TokenStream, TokenStream)> {
    let mut formal: Vec<TokenStream> = Vec::new();
    let mut actual: Vec<TokenStream> = Vec::new();
    let mut variable: Vec<TokenStream> = Vec::new();

    Ok((quote!{#(#formal;)*}, quote!{#(#actual;)*}, quote!{#(#variable?;)*}))
}

fn get_graphql_query_parts(fields: &mut syn::punctuated::Punctuated<syn::Field, syn::token::Comma>) -> 
    deluxe::Result<(TokenStream, TokenStream, TokenStream)> {
    let mut formal: Vec<TokenStream> = Vec::new();
    let mut actual: Vec<TokenStream> = Vec::new();
    let mut variable: Vec<TokenStream> = Vec::new();

    for f in fields.iter_mut() {
        let attrs: GraphQLDeriveAttributeParams = deluxe::extract_attributes(f)?;
        let ident = &f.ident;

        let ident = if let Some(ident) = ident {
            ident
        }
        else {
            panic!("Un identified type");
        };

        let name = ident.to_string();
        //let field_name: Literal = Literal::string(&name);

        let parsed_type = parse_type(&f.ty);
        if let Some(parsed_type) = parsed_type {
            if parsed_type.scalar {
                let mut type_name_string = parsed_type.type_name;

                if attrs.required {
                    type_name_string.push('!');
                }
                let type_name = Literal::string(&&type_name_string);
                let camel_name = if let Some(rename) = attrs.rename {
                    Literal::string(&rename)
                }
                else {
                    Literal::string(&to_camel_case(&name))
                };
                
                actual.push(quote_spanned! {f.span()=>
                    params.push_actual(prefix, #camel_name)
                    // Actual
                });

                formal.push(quote_spanned! {f.span()=>
                    params.push_formal(prefix, #camel_name, #type_name)
                    // Formal
                });

                variable.push(quote_spanned! {f.span()=>
                    variables.push_variable(prefix, #camel_name, &self.#ident)
                    // Variable
                });
            }
            else {
                actual.push(quote_spanned! {f.span()=>
                    // // params.push_actual(#camel_name, &GraphQL::prefix(prefix, #name));
                    // // self.properties.get_formal_part(params, Self::prefix(prefix, #name))
                    // self.#ident.get_actual_part(params, &GraphQL::prefix(prefix, #name))
                });

                formal.push(quote_spanned! {f.span()=>
                    self.#ident.get_formal_part(params, &GraphQL::prefix(prefix, #name))
                });

                variable.push(quote_spanned! {f.span()=>
                    self.#ident.get_variables_part(variables, &GraphQL::prefix(prefix, #name))
                    // self.properties.get_formal_part(params, Self::prefix(prefix, #name))
                });
            }
        }
        else {
            panic!("Unrecognised type {:?}", &f.ty);
        }
    }
    
    Ok((quote!{#(#formal;)*}, quote!{#(#actual;)*}, quote!{#(#variable?;)*}))
}



fn impl_graphql_query_params(data: &mut Data) -> deluxe::Result<(TokenStream, TokenStream, TokenStream)> {
    match data {
        Data::Struct(data) => {
            match &mut data.fields {
                Fields::Named(fields) => {

                    get_graphql_query_parts(&mut fields.named)
                }
                Fields::Unnamed(ref _fields) => {
                    panic!("Unnamed query param struct fields not supported");
                }
                Fields::Unit => {
                    panic!("Unit query params struct fields not supported");
                }
            }
        }
        Data::Enum(data_enum) => {
            get_graphql_query_enum_parts(&data_enum.variants)
        },
        Data::Union(_) => panic!("Union queries struct fields not supported"),
    }
}



// #[cfg(test)]
// mod tests {
//     use super::*;


//     fn parse_string(s: &str) {
//         let stream: proc_macro::TokenStream = s.parse().unwrap();

//         let ast: DeriveInput = syn::parse(stream).unwrap();

//         for t in ast {
//             eprintln!("Token {?:}, t");
//         }
//     }

//     #[test]
//     fn test_parse() {
//         parse_string("Optin<Int>");


//         panic!("Done");
//     }
// }