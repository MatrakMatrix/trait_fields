#![allow(non_snake_case, unused_imports)]
extern crate proc_macro;

#[macro_use]
extern crate fstrings;
use ::proc_macro::{TokenStream};
use std::str::FromStr;
use ::proc_macro2::{Span, TokenStream as TokenStream2};
use ::quote::{quote, quote_spanned};
use ::syn::{*, parse::{Parse, ParseStream}, spanned::Spanned};
use ::std::{*, convert::{TryInto, TryFrom}, iter::FromIterator, result::Result};

use regex::Regex;

#[macro_use]
mod macros;

#[proc_macro]
pub fn field(item: TokenStream) -> TokenStream {
	item
}

#[proc_macro_attribute]
pub fn has_fields(params: TokenStream, input: TokenStream) -> TokenStream {
	let source = input.to_string();
	let re = Regex::new(r"field!\s*\(\s*(.+?)\s*:\s*(.+?)\s*\)\s*;").unwrap();
	let mut new_input = input.to_string();
	let mut fields = Vec::new();
	for cap in re.captures_iter(&source) {
		let cap_str = cap.get(0).unwrap().as_str();
		let field_name = cap.get(1).unwrap().as_str();
		let field_type = cap.get(2).unwrap().as_str();
		new_input = new_input.replace(cap_str, &format!("fn {}(self: &Self) -> {}; fn set_{}(self: &mut Self, new: {});", field_name, field_type, field_name, field_type));
		fields.push([field_name, field_type]);
	}
	let input: TokenStream = new_input.parse().unwrap();

	// === Parse / extraction logic ===
	#[cfg_attr(feature = "verbose-expansions", derive(Debug))]
	#[allow(dead_code)] // dumb compiler...
	struct Trait {
		ident: Ident,
		methods: Vec<TraitItemMethod>,
	}

	impl Parse for Trait {
		fn parse (input: ParseStream) -> syn::Result<Self>
		{Ok({
			let ItemTrait {
					ident,
					items,
					generics,
					..
				} = input.parse()?
			;
			match (
				generics.type_params().next(),
				generics.lifetimes().next(),
				generics.const_params().next(),
			)
			{
				| (None, None, None) => {},

				| _ => parse_error!(
					generics.span(),
					"Trait generics are not supported (yet)",
				),
			}
			let methods: Vec<TraitItemMethod> =
				items
					.into_iter()
					//  error on non-function items
					.map(|trait_item| match trait_item {
						| TraitItem::Method(method) => Ok(method),
						| _ => parse_error!(
							trait_item.span(),
							"`#[inheritable]` currently only supports methods"),
					})
					// error on non-method functions
					.map(|x| x.and_then(|method| {
						let ref sig = method.sig;
						let mut span = sig.ident.span();
						match sig.inputs.iter().next() {
							// & [mut] self
							| Some(&FnArg::Receiver(Receiver {
								reference: Some(_),
								..
							}))
							=> {},

							// self: & [mut] _
							| Some(&FnArg::Typed(PatType {
								ref pat,
								ref ty,
								..
							}))
								if match (&**pat, &**ty) {
									| (
										&Pat::Ident(PatIdent { ref ident, .. }),
										&Type::Reference(_),
									) => {
										ident == "self"
									},

									| _ => false,
								}
							=> {},

							// otherwise
							| opt_arg => {
								if let Some(arg) = opt_arg {
									span = arg.span();
								}
								parse_error!(span, concat!(
									"associated function requires a ",
									"`&self` or `&mut self` receiver",
								));
							},
						}
						Ok(method)
					}))
					.collect::<Result<_, _>>()?
			;
			Self {
				ident,
				methods,
			}
		})}
	}


	set_output!( render => ret );

	debug!(concat!(
		"-------------------------\n",
		"#[inheritable({params})]\n",
		"{input}\n",
	), params=params, input=input);


	// === This macro does not expect params ===
	let params = TokenStream2::from(params);
	if params.clone().into_iter().next().is_some() {
		error!(params.span(), "Unexpected parameter(s)");
	}

	// === Parse the input ===
	let Trait {
		ident: Trait,
		mut methods,
	} = {
		let input = input.clone();
		parse_macro_input!(input)
	};


	// === Render the decorated trait itself (as is) ===
	ret.extend(input);


	// === Render the helper `Inherits#Trait` trait ===
	let InheritsTrait = Ident::new(&f!(
		"__Inherits{Trait}__"
	), Span::call_site());

	ret.extend({
		// Due to a bug in `quote!`, we need to render
		// `#[doc(hidden)]`
		// manually
		use ::proc_macro::*;
		iter::once(TokenTree::Punct(Punct::new(
			'#',
		Spacing::Alone))).chain(TokenStream::from(::quote::quote! {
			[doc(hidden)]
		}))
	});
	render! {
		pub(in crate)
		trait #InheritsTrait {
			type __Parent__
				: #Trait
			;
			fn __parent__ (self: &'_ Self)
			  -> &'_ Self::__Parent__
			;
			fn __parent_mut__ (self: &'_ mut Self)
			  -> &'_ mut Self::__Parent__
			;
		}
	};


	// === Render the default impl of `Trait` for `InheritsTrait` implementors ===
	methods
		.iter_mut()
		.for_each(|method| {
			let &mut TraitItemMethod {
				sig: Signature {
					ref ident,
					ref generics,
					ref mut inputs,
					..
				},
				ref mut default,
				ref mut semi_token,
				ref mut attrs,
			} = method;
			*attrs = vec![];
			*semi_token = None;
			let mut args: Vec<Ident> =
				Vec::with_capacity(
					inputs
						.len()
						.saturating_sub(1)
				)
			;
			let mut inputs_iter = take(inputs).into_iter();
			let mut parent_mb_mut = TokenStream2::default();
			*inputs =
				inputs_iter
					.next()
					.map(|first_arg| {
						if match first_arg {
							| FnArg::Receiver(Receiver {
								ref mutability,
								..
							}) => {
								mutability.is_some()
							},
						// with box patterns we'd be able to merge both cases...
							| FnArg::Typed(PatType { ref ty, .. }) => {
								match &**ty {
									| &Type::Reference(TypeReference {
										ref mutability,
										..
									}) => {
										mutability.is_some()
									},

									| _ => unreachable!(),
								}
							},
						} {
							parent_mb_mut = quote!( __parent_mut__ );
						} else {
							parent_mb_mut = quote!( __parent__ );
						}
						first_arg
					})
					.into_iter()
					.chain(
						inputs_iter
							.zip(1 ..)
							.map(|(mut arg, i)| match arg {
								| FnArg::Typed(PatType { ref mut pat, .. }) => {
									let ident = Ident::new(&f!(
										"arg_{i}"
									), Span::call_site());
									*pat = parse_quote! {
										#ident
									};
									args.push(ident);
									arg
								},

								| _ => unreachable!("Invalid method signature"),
							})
					)
					.collect()
			;
			let generics = generics.split_for_impl().1;
			let generics = generics.as_turbofish();
			// method body
			*default = Some(parse_quote! {
				{
					/* 100% guaranteed unambiguous version */
					// <
					//     <Self as #InheritsTrait>::__Parent__
					//     as #Trait
					// >::#ident #generics (
					//     #InheritsTrait :: #parent_mb_mut(self),
					//     #(#args),*
					// )
					/* This should nevertheless also be unambiguous */
					self.#parent_mb_mut()
						.#ident #generics (
							#(#args),*
						)
				}
			});
		})
	;
	let default_if_specialization =
		if cfg!(feature = "specialization") {
			quote!( default )
		} else {
			TokenStream2::new()
		}
	;
	render! {
		impl<__inheritable_T__ : #InheritsTrait> #Trait
			for __inheritable_T__
		{
			#(
				#[inline]
				#default_if_specialization
				#methods
			)*
		}
	}
	debug!("=> becomes =>\n\n{}\n-------------------------\n", ret);

	let mut struct_def_str = format!("struct {}Struct {{", Trait);
	for cap in fields.iter() {
		struct_def_str += &format!("{}: {},", cap[0], cap[1]);
	}
	struct_def_str += "}";
	// println!("STRUCT DEFINITION: {}", struct_def_str);

	let mut trait_impl_str = format!("impl {} for {}Struct {{", Trait, Trait);
	for cap in fields.iter() {
		trait_impl_str += &format!("fn {}(self: &Self) -> {} {{ self.{} }}", cap[0], cap[1], cap[0]);
		trait_impl_str += &format!("fn set_{}(self: &mut Self, new: {}) {{ self.{} = new; }}", cap[0], cap[1], cap[0]);
	}
	trait_impl_str += "}";
	// println!("TRAIT IMPLEMENTATION: {}", trait_impl_str);

	let mut final_ret = ret.to_string();
	final_ret += &struct_def_str;
	final_ret += &trait_impl_str;
	// println!("FINAL: {}", final_ret);
	final_ret.parse().unwrap()
}

#[inline]
fn take<T : Default> (x: &'_ mut T) -> T {
	mem::replace(x, T::default())
}

#[proc_macro_derive(trait_fields, attributes(implements))]
pub fn derive_trait_fields (input: TokenStream) -> TokenStream {
	debug!(concat!(
		"-------------------------\n",
		"#[derive(Inheritance)]\n",
		"{input}\n",
		"\n",
	), input=input);

	set_output!( render => ret );

	let DeriveInput {
			ident: Struct,
			generics,
			data,
			..
		} = parse_macro_input!(input)
	;
	let fields = match data {
		| Data::Struct(DataStruct { fields, .. }) => fields,
		| Data::Enum(r#enum) => {
			error!(r#enum.enum_token.span(),
				"enums are not supported"
			);
		},
		| Data::Union(r#union) => {
			error!(r#union.union_token.span(),
				"unions are not supported"
			);
		},
	};
	let (mut iter1, mut iter2);
	let fields: &mut dyn Iterator<Item = Field> = match fields {
		| Fields::Unit => {
			iter1 = iter::empty();
			&mut iter1
		},
		| Fields::Unnamed(fields) => {
			iter2 = fields.unnamed.into_iter();
			&mut iter2
		},
		| Fields::Named(fields) => {
			iter2 = fields.named.into_iter();
			&mut iter2
		},
	};
	let ref implements: Ident = parse_quote! {
		implements
	};
	for (i, mut field) in fields.enumerate() {
		let (path_to_InheritsTrait, span) =
			match take(&mut field.attrs)
					.into_iter()
					.find_map(|attr| if attr.path.is_ident(implements) { Some({
						let span = attr.span();
						attr.parse_args_with(Path::parse_mod_style)
							.map(|mut path| {
								let last =
									path.segments
										.iter_mut()
										.last()
										.expect("path has at least one segment")
								;
								let ref Trait = last.ident;
								let InheritsTrait = Ident::new(&f!(
									"__Inherits{Trait}__"
								), span);
								*last = parse_quote! {
									#InheritsTrait
								};
								(path, span)
							})
					})} else {
						None
					})
			{
				| None => continue,
				| Some(Err(err)) => return err.to_compile_error().into(),
				| Some(Ok(inner)) => inner,
			}
		;
		let field_name =
			if let Some(ref ident) = field.ident {
				quote! {
					#ident
				}
			} else {
				let i: Index = i.into();
				quote! {
					#i
				}
			}
		;
		let ref FieldType = field.ty;
		let (impl_generics, ty_generics, where_clause) =
			generics.split_for_impl()
		;
		render! { span =>
			impl #impl_generics #path_to_InheritsTrait
				for #Struct #ty_generics
			#where_clause
			{
				type __Parent__ = #FieldType;

				#[inline]
				fn __parent__ (self: &'_ Self)
				  -> &'_ Self::__Parent__
				{
					&self.#field_name
				}

				#[inline]
				fn __parent_mut__ (self: &'_ mut Self)
				  -> &'_ mut Self::__Parent__
				{
					&mut self.#field_name
				}
			}
		}
	}
	debug!("=> generates =>\n\n{}\n-------------------------\n", ret);
	ret
}
