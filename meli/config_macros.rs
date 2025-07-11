/*
 * meli -
 *
 * Copyright  Manos Pitsidianakis
 *
 * This file is part of meli.
 *
 * meli is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * meli is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with meli. If not, see <http://www.gnu.org/licenses/>.
 */

use std::{
    fs::File,
    io::prelude::*,
    process::{Command, Stdio},
};

use quote::{format_ident, quote};
use regex::Regex;

// Write ConfigStructOverride to overrides.rs
pub(crate) fn override_derive(filenames: &[(&str, &str)]) {
    let mut output_file =
        File::create("src/conf/overrides.rs").expect("Unable to open output file");
    let mut output_string = r##"// @generated
/*
 * meli - conf/overrides.rs
 *
 * Copyright 2020 Manos Pitsidianakis
 *
 * This file is part of meli.
 *
 * meli is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * meli is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with meli. If not, see <http://www.gnu.org/licenses/>.
 */

#![allow(clippy::derivable_impls)]

//! This module is automatically generated by `config_macros.rs`.

use melib::HeaderName;

use indexmap::IndexSet;

use crate::conf::{*, data_types::*};

"##
    .to_string();

    let cfg_attr_default_attr_regex = Regex::new(r"\s*default\s*[,]").unwrap();
    let cfg_attr_default_val_attr_regex = Regex::new(r#"\s*default\s*=\s*"[^"]*"\s*,\s*"#).unwrap();
    let cfg_attr_skip_ser_attr_regex =
        Regex::new(r#"\s*,?\s*skip_serializing_if\s*=\s*"[^"]*"\s*,?\s*"#).unwrap();
    let cfg_attr_feature_regex = Regex::new(r"[(](?:not[(]\s*)?feature").unwrap();

    'file_loop: for (filename, ident) in filenames {
        println!("cargo:rerun-if-changed={filename}");
        let mut file = File::open(filename)
            .unwrap_or_else(|err| panic!("Unable to open file `{filename}` {err}"));

        let mut src = String::new();
        file.read_to_string(&mut src).expect("Unable to read file");

        let syntax = syn::parse_file(&src).expect("Unable to parse file");
        if syntax.items.iter().any(|item| {
            if let syn::Item::Struct(s) = item {
                if s.ident.to_string().ends_with("Override") {
                    println!("ident {ident} exists, skipping {filename}");
                    return true;
                }
            }
            false
        }) {
            continue 'file_loop;
        }

        for item in syntax.items.iter() {
            if let syn::Item::Struct(s) = item {
                if s.ident != ident {
                    continue;
                }
                if s.ident.to_string().ends_with("Override") {
                    unreachable!();
                }
                let override_ident: syn::Ident = format_ident!("{}Override", s.ident);
                let mut field_tokentrees = vec![];
                let mut attrs_tokens = vec![];
                for attr in &s.attrs {
                    if let Ok(syn::Meta::List(ml)) = attr.parse_meta() {
                        if ml.path.get_ident().is_some() && ml.path.get_ident().unwrap() == "cfg" {
                            attrs_tokens.push(attr);
                        }
                    }
                }
                let mut field_idents = vec![];
                for f in &s.fields {
                    let ident = &f.ident;
                    let ty = &f.ty;
                    let attrs = f
                        .attrs
                        .iter()
                        .filter_map(|f| {
                            let mut new_attr = f.clone();
                            if let proc_macro2::TokenTree::Group(g) =
                                f.tokens.clone().into_iter().next().unwrap()
                            {
                                let mut attr_inner_value = f.tokens.to_string();
                                if attr_inner_value.contains("skip_serializing_if") {
                                    attr_inner_value = cfg_attr_skip_ser_attr_regex
                                        .replace_all(&attr_inner_value, "")
                                        .to_string();
                                    let new_toks: proc_macro2::TokenStream =
                                        attr_inner_value.parse().unwrap();
                                    new_attr.tokens = quote! { #new_toks };
                                }
                                if cfg_attr_feature_regex.is_match(&attr_inner_value) {
                                    attr_inner_value = cfg_attr_default_val_attr_regex
                                        .replace_all(&attr_inner_value, "")
                                        .to_string();
                                    if attr_inner_value.contains("default") {
                                        attr_inner_value = cfg_attr_default_attr_regex
                                            .replace_all(&attr_inner_value, "")
                                            .to_string();
                                    }
                                    let new_toks: proc_macro2::TokenStream =
                                        attr_inner_value.parse().unwrap();
                                    new_attr.tokens = quote! { #new_toks };
                                }
                                if !attr_inner_value.starts_with("( default")
                                    && !attr_inner_value.starts_with("( default =")
                                    && !attr_inner_value.starts_with("(default")
                                    && !attr_inner_value.starts_with("(default =")
                                {
                                    return Some(new_attr);
                                }
                                if attr_inner_value.starts_with("( default =")
                                    || attr_inner_value.starts_with("(default =")
                                {
                                    let rest = g.stream().into_iter().skip(4);
                                    new_attr.tokens = quote! { ( #(#rest)*) };
                                    match new_attr.tokens.to_string().as_str() {
                                        "( )" | "()" => {
                                            return None;
                                        }
                                        _ => {}
                                    }
                                } else if attr_inner_value.starts_with("( default")
                                    || attr_inner_value.starts_with("(default")
                                {
                                    if attr_inner_value.ends_with("default)")
                                        || attr_inner_value.ends_with("default )")
                                    {
                                        return None;
                                    }
                                    let rest = g.stream().into_iter().skip(2);
                                    new_attr.tokens = quote! { ( #(#rest)*) };
                                    match new_attr.tokens.to_string().as_str() {
                                        "( )" | "()" => {
                                            return None;
                                        }
                                        _ => {}
                                    }
                                }
                            }

                            Some(new_attr)
                        })
                        .collect::<Vec<_>>();
                    let t = quote! {
                        #(#attrs)*
                        #[serde(default)]
                        pub #ident : Option<#ty>
                    };
                    if !field_idents.contains(&ident) {
                        field_idents.push(ident);
                    }
                    field_tokentrees.push(t);
                }
                //let fields = &s.fields;

                let literal_struct = quote! {
                    #(#attrs_tokens)*
                    #[derive(Debug, Serialize, Deserialize, Clone)]
                    #[serde(deny_unknown_fields)]
                    pub struct #override_ident {
                        #(#field_tokentrees),*
                    }


                    #(#attrs_tokens)*
                    impl Default for #override_ident {
                        fn default() -> Self {
                            Self {
                                #(#field_idents: None),*
                            }
                        }
                    }
                };
                output_string.push_str(&literal_struct.to_string());
                output_string.push_str("\n\n");
            }
        }
    }

    let rustfmt_closure = move |output_file: &mut File, output_string: &str| {
        let mut rustfmt = Command::new("rustfmt")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|err| format!("failed to execute rustfmt {err}"))?;

        {
            // limited borrow of stdin
            let stdin = rustfmt
                .stdin
                .as_mut()
                .ok_or("failed to get rustfmt stdin")?;
            stdin
                .write_all(output_string.as_bytes())
                .map_err(|err| format!("failed to write to rustfmt stdin {err}"))?;
        }

        let output = rustfmt
            .wait_with_output()
            .map_err(|err| format!("failed to wait on rustfmt child {err}"))?;
        if !output.stderr.is_empty() {
            return Err(format!(
                "rustfmt invocation replied with: `{}`",
                String::from_utf8_lossy(&output.stderr)
            ));
        }

        output_file
            .write_all(&output.stdout)
            .expect("failed to write to src/conf/overrides.rs");
        Ok(())
    };
    if let Err(err) = rustfmt_closure(&mut output_file, &output_string) {
        println!("Tried rustfmt on overrides module, got error: {err}");
        output_file.write_all(output_string.as_bytes()).unwrap();
    }
}
