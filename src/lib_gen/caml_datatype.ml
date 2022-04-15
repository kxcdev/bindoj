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
open Ast_builder.Default
open Utils

let rec type_declaration_of_type_decl : type_decl -> type_declaration = fun { td_name; td_kind; } ->
  let loc = Location.none in
  let (kind, doc) = td_kind in
  { (type_declaration ~loc ~params:[] ~cstrs:[] ~private_:Public ~manifest:None
       ~name:(locmk ~loc td_name)
       ~kind:(type_kind_of_generic_kind kind))
    with ptype_attributes = doc_attributes doc }

and type_kind_of_generic_kind : generic_kind -> type_kind = function
  | Record_kind record ->
    Ptype_record (record |&> fun (field, doc) ->
        { (label_declaration_of_record_field_desc field)
          with pld_attributes = doc_attributes doc; })
  | Variant_kind variant ->
    Ptype_variant (variant |&> fun (constructor, doc) ->
        { (constructor_declaration_of_variant_constructor_desc constructor)
          with pcd_attributes = doc_attributes doc; })

and label_declaration_of_record_field_desc : record_field_desc -> label_declaration =
  fun { rf_name; rf_type; _; } ->
  let loc = Location.none in
  label_declaration ~loc ~mutable_:Immutable
    ~name:(locmk ~loc rf_name)
    ~type_:(ptyp_constr ~loc (lidloc ~loc rf_type) [])

and constructor_declaration_of_variant_constructor_desc : variant_constructor_desc -> constructor_declaration =
  let loc = Location.none in
  function
  | Cstr_tuple { ct_name; ct_args; _; } ->
    constructor_declaration ~loc ~res:None
      ~name:(locmk ~loc ct_name)
      ~args:(Pcstr_tuple (ct_args |&> fun arg ->
          ptyp_constr ~loc (lidloc ~loc arg) []))
  | Cstr_record { cr_name; cr_fields; _; } ->
    constructor_declaration ~loc ~res:None
      ~name:(locmk ~loc cr_name)
      ~args:(match type_kind_of_generic_kind (Record_kind cr_fields) with
          | Ptype_record fields -> Pcstr_record fields
          | _ -> failwith "type_kind but record is invalid in record constructor declaration")
