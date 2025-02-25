(* Copyright 2022-2023 Kotoi-Xie Consultancy, Inc. This file is a part of the

==== Bindoj (https://kxc.dev/bindoj) ====

software project that is developed, maintained, and distributed by
Kotoi-Xie Consultancy, Inc. (https://kxc.inc) which is also known as KXC.

Licensed under the Apache License, Version 2.0 (the "License"); you may not
use this file except in compliance with the License. You may obtain a copy
of the License at http://www.apache.org/licenses/LICENSE-2.0. Unless required
by applicable law or agreed to in writing, software distributed under the
License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS
OF ANY KIND, either express or implied. See the License for the specific
language governing permissions and limitations under the License.
                                                                              *)
(* Acknowledgements  --- AnchorZ Inc. ---  The current/initial version or a
significant portion of this file is developed under the funding provided by
AnchorZ Inc. to satisfy its needs in its product development workflow.
                                                                              *)
open Bindoj_typedesc.Type_desc
open Bindoj_objintf_shared
open Bindoj_codec_config

let is_self_coretype : simple_type -> bool = function
  | `direct Coretype.{ ct_desc; _ }
  | `nested ({ td_kind = Alias_decl { ct_desc; _ }; _ }, _) -> ct_desc = Self
  | _ -> false

let is_unit_type = function
| `direct Coretype.{ ct_desc = Prim `unit; _ }
| `nested ({ td_kind = Alias_decl { ct_desc = Prim `unit; _ }; _ }, _) -> true
| _ -> false

let party_with_polarity ~polarity kind =
  match polarity, kind with
  | Cis_party, `endemic | Trans_party, `peer -> `endemic
  | Cis_party, `peer | Trans_party, `endemic -> `peer

let ord_coordinate_to_coretype : coordinate_type_desc -> coretype = function
  | `prim p -> Coretype.(mk_prim (p :> prim))
  | `string_enum cases -> Coretype.mk_string_enum cases

let name_of_bridgeable : ([> `complex | `simple], 'bridgeable_ident) bridgeable_descriptor -> string =
  function
  | Method_bundle_bridgeable { name; _ } -> name
  | Sole_method_bridgeable desc ->
    (match desc with
    | Simple_method { name; _ } -> name
    | Complex_method { name; _ } -> name)

let complex_descriptor_of_bridgeable :
  ([> `simple | `complex ], 'bridgeable_ident) bridgeable_decl
  -> ([< `complex | `simple ], 'bridgeable_ident) bridgeable_descriptor
  = function
  | Simple_bridgeable { desc } ->
    ((* TODO - we need type ninja to figure this out. *)
      Obj.magic (desc : ([ `simple ], 'bridgeable_ident) bridgeable_descriptor)
      : ([ `complex | `simple ], 'bridgeable_ident) bridgeable_descriptor)
  | Complex_bridgeable { desc } ->
    ((* TODO - we need type ninja to figure this out. *)
      Obj.magic (desc : ([< `complex | `simple ], 'bridgeable_ident) bridgeable_descriptor)
      : ([ `complex | `simple ], 'bridgeable_ident) bridgeable_descriptor)

let body_of_method_descriptor :
  ([> `complex | `simple ], 'bi) method_descriptor
  -> 'bi complex_type method_descriptor_body =
  fun desc ->
  match desc with
  | Simple_method body -> (body :> 'bi complex_type method_descriptor_body)
  | Complex_method body -> body

let extract_type_decls : 'bi sameworld_objintf -> (type_decl * Coretype.codec) list =
  let tds_of_method :
    [> `nested of type_decl * Coretype.codec ] method_descriptor_body
    -> (type_decl * Coretype.codec) list =
    fun body ->
    (body.positional_arguments |&?> (function
      | Parg_regular { typ = `nested (td, codec); _ } ->
        Some (td, codec)
      | _ -> None))
    @ (body.labeled_arguments |&?> (function
        | Larg_regular { typ = `nested (td, codec); _ } -> Some (td, codec)
        | Larg_optional_with_default { typ = td; codec; _ } ->
          Some (Bindoj_typedesc.Typed_type_desc.Typed.decl td, codec)
        | _ -> None))
    @ (match body.return_type with
      | Mret_regular (`nested (td, codec)) -> [ (td, codec) ]
      | _ -> [])
  in
  fun objintf ->
    (objintf.so_bridgeable_decls
    |&>> (complex_descriptor_of_bridgeable &> function
      | Sole_method_bridgeable (Simple_method body) ->
        tds_of_method body
      | Sole_method_bridgeable (Complex_method body) ->
        tds_of_method body
      | Method_bundle_bridgeable { descs; _ } ->
        descs |&>> (function
        | Simple_method body -> tds_of_method body
        | Complex_method body -> tds_of_method body)
    ))
    @ (objintf.so_named_objects
      |&?> (function
        | { nod_typ = `nested(td, codec); _ } -> Some (td, codec)
        | _ -> None))
    @ (objintf.so_object_registries
    |&?> (function
      | { ord_typ = `nested(td, codec); _ } -> Some (td, codec)
      | _ -> None))
    |> List.sort_uniq compare

let nested_type_to_ident_opt
  : [> `direct of Coretype.t | `nested of type_decl * Coretype.codec ]
  -> (Coretype.ident * Json_config.json_mangling_style) option = function
  | `direct { ct_desc = Ident i; ct_configs } ->
    Some (i, Json_config.(get_mangling_style_opt ct_configs |? default_mangling_style))
  | `nested ({ td_kind = Alias_decl { ct_desc = Ident i; ct_configs }; td_configs; _ }, _) ->
    Some (i, Json_config.(
      get_mangling_style_opt ct_configs
      ||?! (fun () -> get_mangling_style_opt td_configs)
      |? default_mangling_style)
  )
  | _ -> None

let rec coretype_to_idents ?(inherited=Json_config.default_mangling_style) =
  let rec go = function
    | Coretype.Ident ident -> [ident]
    | Option d -> go d
    | Tuple ds -> ds |&>> go
    | List d -> go d
    | Map (_, d) -> go d
    | _ -> []
  in
  fun { Coretype.ct_desc; ct_configs } ->
    go ct_desc |&> (fun i ->
      i, Json_config.(get_mangling_style_opt ct_configs |? inherited))
and record_field_to_idents ?(inherited=Json_config.default_mangling_style) =
  fun { rf_type; rf_configs; _ } ->
    let inherited =
      Json_config.get_mangling_style_opt rf_configs
      |? inherited
    in
    match rf_type with
    | `direct ct -> coretype_to_idents ~inherited ct
    | `nested (td, _) -> type_decl_to_idents ~inherited td
and type_decl_to_idents ?(inherited=Json_config.default_mangling_style) =
  function
  | { td_kind = Alias_decl ct; td_configs; _ } ->
    coretype_to_idents
      ~inherited:Json_config.(
        get_mangling_style_opt td_configs
        |? inherited)
      ct
  | _ -> []
and nested_type_to_idents
  : [> `direct of Coretype.t | `nested of type_decl * Coretype.codec ]
  -> (Coretype.ident * Json_config.json_mangling_style) list
  =
  function
  | `direct ct -> coretype_to_idents ct
  | `nested (td, _) -> type_decl_to_idents td
  | _ -> []

let extract_idents : 'bridgeable_ident sameworld_objintf -> (Coretype.ident * Json_config.json_mangling_style) list =
  let idents_of_method :
    _ method_descriptor_body
    -> _ =
    fun body ->
      (body.positional_arguments |&>> (function
        | Parg_regular { typ; _ } -> nested_type_to_idents typ))
      @ (body.labeled_arguments |&>> (function
        | Larg_regular { typ; _ } -> nested_type_to_idents typ
        | Larg_optional_with_default { typ; _ } ->
          type_decl_to_idents (Bindoj_typedesc.Typed_type_desc.Typed.decl typ)))
      @ (match body.return_type with
        | Mret_regular typ -> nested_type_to_idents typ)
  in
  fun objintf ->
    (objintf.so_bridgeable_decls
    |&>> (complex_descriptor_of_bridgeable &> function
      | Sole_method_bridgeable (Simple_method body) -> idents_of_method body
      | Sole_method_bridgeable (Complex_method body) -> idents_of_method body
      | Method_bundle_bridgeable { descs; _ } ->
        descs |&>> (function
        | Simple_method body -> idents_of_method body
        | Complex_method body -> idents_of_method body)
      ))
    @ (objintf.so_named_objects |&>> (fun { nod_typ; _ } -> nested_type_to_idents nod_typ))
    @ (objintf.so_object_registries |&>> (fun { ord_typ; _ } -> nested_type_to_idents ord_typ))
    |> List.sort_uniq compare

let map_bridgeables_of :
  [ `simple | `complex ]
  -> (([ `complex | `simple ], 'bridgeable_ident) bridgeable_descriptor -> 'a)
  -> 'bi sameworld_objintf
  -> 'a list =
  fun kind f objintf ->
    objintf.so_bridgeable_decls |&?> (match kind with
    | `simple -> (function
      | Simple_bridgeable { desc } ->
        some & f
          ((* TODO - we need type ninja to figure this out. *) 
            Obj.magic (desc : ([ `simple ], 'brideable_ident) bridgeable_descriptor)
              : ([ `complex | `simple ], 'brideable_ident) bridgeable_descriptor)
      | Complex_bridgeable _ -> None)
    | `complex -> (function
      | Simple_bridgeable _ -> None
      | Complex_bridgeable  { desc } ->
        some & f
          ((* TODO - we need type ninja to figure this out. *)
            Obj.magic (desc : ([< `complex | `simple ], 'brideable_ident) bridgeable_descriptor)
            : ([ `complex | `simple ], 'brideable_ident) bridgeable_descriptor)
    ))

let peer_setup_needed objintf =
  let polarity = objintf.so_polarity in
  (objintf.so_named_objects
    |> List.exists (fun nod ->
      party_with_polarity ~polarity nod.nod_party = `peer))
  || (objintf.so_object_registries
    |> List.exists (fun ord ->
    party_with_polarity ~polarity ord.ord_party = `peer))

let coordinate_desc_to_map_key_type_desc
  : [ `prim of [ `string | `int53p ]
  | `string_enum of Coretype.string_enum_case list
  ] -> Bindoj_runtime.Map_key.map_key_type_desc =
  function
  | `prim x -> (x :> Bindoj_runtime.Map_key.map_key_type_desc)
  | `string_enum cases ->
    `StringEnum (cases |&> (fun (label, _, _) -> label))
