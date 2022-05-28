(* Copyright 2022 Kotoi-Xie Consultancy

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. *)

open Ppxlib
open Ast_helper
open Utils

type variant_type_flavor = [
  | `regular_variant (** the default *)
  | `polymorphic_variant
  (* | `extensible_variant (* future work *) *)
  ]

type ('pos, 'flavor) flavor_config +=
  | Flvconfig_variant_flavor :
    variant_type_flavor
    -> ([ `type_decl ], [ `variant_flavor ]) flavor_config

let get_variant_type =
  FlavorConfigs.find_or_default ~default:`regular_variant (function | Flvconfig_variant_flavor f -> Some f | _ -> None)

type generic_kind' = [
  | `Record_kind  of record_type_desc
  | `Variant_kind of variant_type_desc * variant_type_flavor
]

let get_kind_with_flavor (td: type_decl) : generic_kind' with_docstr =
  let kind, doc = td.td_kind in
  match kind, get_variant_type td.td_flvconfigs with
  | Record_kind r, _ -> `Record_kind r, doc
  | Variant_kind v, f -> `Variant_kind (v, f), doc

let rec type_declaration_of_type_decl : ?show:bool -> type_decl -> type_declaration =
  fun ?(show=false) td ->
  let kind, doc = get_kind_with_flavor td in
  let attrs =
    if show then show_attribute @ doc_attribute doc
    else doc_attribute doc
  in
  Type.mk
    ~kind:(type_kind_of_generic_kind kind)
    ?manifest:(type_manifest_of_generic_kind kind)
    ~attrs
    (locmk td.td_name)

and type_kind_of_generic_kind : generic_kind' -> type_kind = function
  | `Record_kind record ->
    Ptype_record  (record |> label_declarations_of_record_type_desc)
  | `Variant_kind (variant, `regular_variant) ->
    Ptype_variant (variant |> constructor_declarations_of_variant_type_desc)
  | `Variant_kind (_, `polymorphic_variant) ->
    Ptype_abstract

and type_manifest_of_generic_kind : generic_kind' -> core_type option = function
  | `Record_kind _
  | `Variant_kind (_, `regular_variant) -> None
  | `Variant_kind (pvariant, `polymorphic_variant) ->
    let fields =
      pvariant |&> fun (ctor, doc) ->
        match ctor with
        | Cstr_tuple ct ->
          Rf.tag ~attrs:(doc_attribute doc)
            (locmk ct.ct_name)
            (List.empty ct.ct_args)
            (match ct.ct_args with
             | [] -> []
             | [arg] -> [typcons arg]
             | args -> [Typ.tuple (args |&> typcons)])
        | Cstr_record cr ->
          failwith' "case '%s' with an inline record cannot be used in a polymorphic variant" cr.cr_name
    in
    Some (Typ.variant fields Closed None)

and label_declarations_of_record_type_desc : record_type_desc -> label_declaration list =
  fun record ->
  record |&> fun (field, doc) ->
    Type.field
      ~attrs:(doc_attribute doc)
      (locmk field.rf_name)
      (typcons field.rf_type)

and constructor_declarations_of_variant_type_desc : variant_type_desc -> constructor_declaration list =
  fun variant ->
  variant |&> fun (ctor, doc) ->
    let name, args =
      match ctor with
      | Cstr_tuple  { ct_name; ct_args; _ } ->
        ct_name, Pcstr_tuple  (ct_args |&> typcons)
      | Cstr_record { cr_name; cr_fields; _ } ->
        cr_name, Pcstr_record (cr_fields |> label_declarations_of_record_type_desc)
    in
    Type.constructor ~attrs:(doc_attribute doc) ~args (locmk name)
