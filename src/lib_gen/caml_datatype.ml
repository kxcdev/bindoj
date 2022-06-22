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
open Bindoj_typedesc.Type_desc

let rec type_declaration_of_type_decl :
          ?type_name:string
          -> ?show:bool
          -> type_decl
          -> type_declaration =
  fun ?type_name ?(show=false) td ->
  let self_name = type_name |? td.td_name in
  let doc = td.td_doc in
  let attrs =
    if show then show_attribute @ doc_attribute doc
    else doc_attribute doc
  in
  Type.mk
    ~kind:(type_kind_of_type_decl_kind ~self_name td)
    ?manifest:(type_manifest_of_type_decl_kind ~self_name td)
    ~attrs
    (locmk self_name)

and type_kind_of_type_decl_kind : self_name:string -> type_decl -> type_kind =
  fun ~self_name td ->
  match td.td_kind with
  | Alias_decl _cty -> failwith "TODO" (* TODO #127: implement Alias_decl *)
  | Record_decl fields ->
    Ptype_record (fields |> label_declarations_of_record_fields ~self_name)
  | Variant_decl ctors ->
    match Caml_config.get_variant_type td.td_configs with
    | `regular ->
      Ptype_variant (ctors |> constructor_declarations_of_variant_constructors ~self_name)
    | `polymorphic -> Ptype_abstract

and type_manifest_of_type_decl_kind : self_name:string -> type_decl -> core_type option =
  fun ~self_name td ->
  match td.td_kind with
  | Alias_decl _cty -> failwith "TODO" (* TODO #127: implement Alias_decl *)
  | Record_decl _ -> None
  | Variant_decl ctors ->
    match Caml_config.get_variant_type td.td_configs with
    | `regular -> None
    | `polymorphic ->
      let fields =
      ctors |&> fun ctor ->
        match ctor.vc_param with
        | `no_param ->
          Rf.tag ~attrs:(doc_attribute ctor.vc_doc)
            (locmk ctor.vc_name)
            false
            []
        | `tuple_like ts ->
          Rf.tag ~attrs:(doc_attribute ctor.vc_doc)
            (locmk ctor.vc_name)
            true
            (match ts with
            | [] -> failwith "impossible"
            | [arg] -> [type_of_coretype ~self_name arg]
            | args -> [Typ.tuple (args |&> type_of_coretype ~self_name)])
        | `inline_record _ ->
          failwith' "case '%s' with an inline record cannot be used in a polymorphic variant" ctor.vc_name
      in
      Some (Typ.variant fields Closed None)

and label_declarations_of_record_fields ~self_name fields =
  fields |&> fun field ->
    Type.field
      ~attrs:(doc_attribute field.rf_doc)
      (locmk field.rf_name)
      (type_of_coretype ~self_name field.rf_type)

and constructor_declarations_of_variant_constructors ~self_name ctors =
  ctors |&> fun ctor ->
    let name, args =
      match ctor.vc_param with
      | `no_param ->
        ctor.vc_name, Pcstr_tuple []
      | `tuple_like ts ->
        ctor.vc_name, Pcstr_tuple (ts |&> type_of_coretype ~self_name)
      | `inline_record fields ->
        ctor.vc_name, Pcstr_record (fields |> label_declarations_of_record_fields ~self_name)
    in
    Type.constructor ~attrs:(doc_attribute ctor.vc_doc) ~args (locmk name)

and type_of_coretype : self_name:string -> coretype -> core_type =
  fun ~self_name { ct_desc; _ } ->
  let open Coretype in
  let type_of_prim = function
    | `unit -> typcons "unit"
    | `bool -> typcons "bool"
    | `int -> typcons "int"
    | `float -> typcons "float"
    | `string -> typcons "string"
    | `uchar -> typcons "Uchar.t"
    | `byte -> typcons "char"
    | `bytes -> typcons "Bytes.t"
  in
  let rec go = function
    | Prim p -> type_of_prim p
    | Inhabitable -> typcons "unit"
    | Ident s -> typcons s.id_name
    | Option t -> typcons "option" ~args:[go t]
    | List t -> typcons "list" ~args:[go t]
    | Map (k, v) ->
      let k = desc_of_map_key k in
      typcons "list" ~args:[Typ.tuple [go k; go v]]
    | Tuple ts -> ts |> List.map go |> Typ.tuple
    | StringEnum cs ->
      let cases = cs |> List.map (fun k -> Rf.tag (locmk (escape_as_constructor_name k)) true []) in
      Typ.variant cases Closed None
    | Self -> typcons self_name
    in go ct_desc

