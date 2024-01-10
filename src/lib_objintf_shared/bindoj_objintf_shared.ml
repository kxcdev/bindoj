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
open Bindoj_runtime
open Bindoj_typedesc.Typed_type_desc

include Objintf_common

module Objintf_config = Bindoj_objintf_shared_config.Objintf_config

type party_polarity =
  | Cis_party
  | Trans_party

type +'typ positional_argument_descriptor =
  | Parg_regular : {
      name : string option;
      typ : 'typ;
      doc : doc;
    } -> 'typ positional_argument_descriptor

type +'arg_type labeled_argument_descriptor =
  | Larg_regular : {
      label : string;
      optional : bool;
      typ : 'arg_type;
      doc : doc;
    } -> 'arg_type labeled_argument_descriptor
  | Larg_optional_with_default : {
      label : string;
      typ : 'x typed_type_decl;
      codec : Coretype.codec;
      default : 'x;
      doc : doc;
    } -> _ labeled_argument_descriptor

type +'typ method_return_type_descriptor =
  | Mret_regular : 'typ -> 'typ method_return_type_descriptor

type simple_type = [ `direct of coretype | `nested of type_decl * Coretype.codec ]

type +'bridgeable_ident complex_type = [
  | `direct of coretype
  | `nested of type_decl * Coretype.codec
  | `bridgeable of [ `endemic | `peer ] * 'bridgeable_ident
  | `self_bridgeable of [ `endemic | `peer ]
]

type +'bridgeable_ident registry_type = [
  | `direct of coretype
  | `nested of type_decl * Coretype.codec
  | `bridgeable of [ `endemic | `peer ] * 'bridgeable_ident
]

type (_, 'bridgeable_ident) method_descriptor =
  | Simple_method :
      simple_type method_descriptor_body
      -> ([> `simple ], _) method_descriptor
  | Complex_method :
      'bridgeable_ident complex_type method_descriptor_body
      -> ([> `complex ], 'bridgeable_ident) method_descriptor
and (+'typ) method_descriptor_body = {
    name : string;
    positional_arguments : 'typ positional_argument_descriptor list;
    labeled_arguments : 'typ labeled_argument_descriptor list;
    return_type: 'typ method_return_type_descriptor;
    doc : doc;
  }

type (_, 'bridgeable_ident) bridgeable_descriptor =
  | Sole_method_bridgeable :
      ('cplx, 'bridgeable_ident) method_descriptor
      -> ('cplx, 'bridgeable_ident) bridgeable_descriptor
  | Method_bundle_bridgeable : {
      name : string;
      descs : ('cplx, 'bridgeable_ident) method_descriptor list;
      configs : [ `method_bundle_brideable_decl ] configs;
      doc : doc;
    } -> ('cplx, 'bridgeable_ident) bridgeable_descriptor

type (_, 'bridgeable_ident) bridgeable_decl =
  | Simple_bridgeable : {
      desc : ([ `simple ], 'bridgeable_ident) bridgeable_descriptor;
    } -> ([> `simple ], 'bridgeable_ident) bridgeable_decl

  | Complex_bridgeable : {
      desc : ([< `simple | `complex ], 'bridgeable_ident) bridgeable_descriptor;
    } -> ([> `complex ], 'bridgeable_ident) bridgeable_decl

type ('party, 'bridgeable_ident) named_object_decl = {
    nod_name : string;
    nod_party : 'party;
    nod_typ : 'bridgeable_ident registry_type;
    nod_doc : doc;
  }

type coordinate_type_desc = [
  | `prim of [ `string | `int53p ]
  | `string_enum of Coretype.string_enum_case list
  ]

type ('party, 'bridgeable_ident) object_registry_decl = {
    ord_name : string;
    ord_coordinate_desc : (string * coordinate_type_desc) list;
    ord_party : 'party;
    ord_typ : 'bridgeable_ident registry_type;
    ord_doc : doc;
  }

type 'bridgeable_ident sameworld_objintf = {
  so_name : string;
  so_polarity : party_polarity;
  so_named_objects : ([ `endemic | `peer ], 'bridgeable_ident) named_object_decl list;
  so_object_registries : ([ `endemic | `peer ], 'bridgeable_ident) object_registry_decl list;
  so_bridgeable_decls : ([ `simple | `complex ], 'bridgeable_ident) bridgeable_decl list;
  so_doc : doc;
}

type 'bridgeable_ident bridgeable_ident_resolver =
  'bridgeable_ident
  -> ([ `simple | `complex ], 'bridgeable_ident) bridgeable_decl

let positional_argument_regular =
  fun ?(doc=`nodoc) ?name typ ->
    Parg_regular { name; typ; doc }

let labeled_argument_regular =
  fun ?(doc=`nodoc) ~optional label typ ->
    Larg_regular { label; optional; typ; doc }

let labeled_argument_with_default =
  fun ?(doc=`nodoc) ?(codec=`default) ~default label typ ->
    Larg_optional_with_default { label; typ; codec; default; doc }

let method_return_type_regular =
  fun typ -> Mret_regular typ

let simple_method =
  fun ?(doc=`nodoc) name ?(pargs=[]) ?(largs=[]) return_type ->
    Simple_method { name; positional_arguments = pargs; labeled_arguments = largs; return_type; doc }

let complex_method =
  fun ?(doc=`nodoc) name ?(pargs=[]) ?(largs=[]) return_type ->
    Complex_method { name; positional_arguments = pargs; labeled_arguments = largs; return_type; doc }

let sole_method_bridgeable =
  fun mtd -> Sole_method_bridgeable mtd

let method_bundle_bridgeable =
  fun ?(doc=`nodoc) ?(configs = Configs.empty) name descs ->
    Method_bundle_bridgeable { name; descs; configs; doc }

let simple_bridgeable =
  fun desc ->
    Simple_bridgeable { desc }

let complex_bridgeable =
  fun desc ->
    Complex_bridgeable { desc }

let named_object_decl =
  fun ?(doc=`nodoc) name ~party ~typ ->
    { nod_name = name; nod_party = party; nod_typ = typ; nod_doc = doc }

let object_registry_decl =
  fun ?(doc=`nodoc) name ~coordinate_desc ~party ~typ ->
    { ord_name = name; ord_coordinate_desc = coordinate_desc; ord_party = party; ord_typ = typ; ord_doc = doc }

let sameworld_objintf =
  fun ~name ?(doc=`nodoc) ?(polarity=Cis_party)
    ?(named_objects=[]) ?(object_registries=[]) bridgeable_decls ->
    { so_name = name;
      so_polarity = polarity;
      so_named_objects = named_objects;
      so_object_registries = object_registries;
      so_bridgeable_decls = bridgeable_decls;
      so_doc = doc }

let validate_objintf : 'bridgeable_ident.
  bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
  -> 'bridgeable_ident sameworld_objintf
  -> ('bridgeable_ident sameworld_objintf, string) result =
  let not_duplicated_by f message xs =
    let rec go acc = function
      | [] -> Ok xs
      | x :: xs ->
        let k = f x in
        if List.mem k acc then
          Error (message k)
        else
          go (k :: acc) xs
    in
    go [] xs
  in
  let validate_method ({ name; positional_arguments; labeled_arguments; _ } as meth) =
    (positional_arguments |&?> (function
      | Parg_regular { name; _ } -> name))
    @ (labeled_arguments |&> (function
      | Larg_regular { label; _ } | Larg_optional_with_default { label; _ } -> label))
    |> not_duplicated_by identity (sprintf "Method '%s' has a duplicate name for argument '%s" name)
    |> Result.map (fun _ -> meth)
  in
  let open
    struct
      let (>>=) = Result.bind
      let (>|=) x f = Result.map f x
      let sequence_list = Result.concat
    end
  in
  fun ~bridgeable_ident_resolver objintf ->
    objintf.so_bridgeable_decls |&> (function
      | Simple_bridgeable { desc } ->
        begin match desc with
        | Sole_method_bridgeable (Simple_method meth) ->
          validate_method meth
          >|= constant meth.name
        | Method_bundle_bridgeable { name; descs; _ } ->
          descs
          |&> (function
            | Simple_method meth -> validate_method meth >|= constant meth.name)
          |> sequence_list
          >>= not_duplicated_by identity (sprintf "Bridgeable decl '%s' has a duplicate name for method '%s'" name)
          >|= constant name
        end
      | Complex_bridgeable { desc } ->
        begin match desc with
        | Sole_method_bridgeable (Simple_method meth) ->
          validate_method meth
          >|= constant meth.name
        | Sole_method_bridgeable (Complex_method meth) ->
          validate_method meth
          >|= constant meth.name
        | Method_bundle_bridgeable { name; descs; _ } ->
          descs
          |&> (function
            | Simple_method meth -> validate_method meth >|= constant meth.name
            | Complex_method meth -> validate_method meth >|= constant meth.name)
          |> sequence_list
          >>= not_duplicated_by identity (sprintf "Bridgeable decl '%s' has a duplicate name for method '%s'" name)
          >|= constant name
        end
        )
    |> sequence_list
    >>= not_duplicated_by identity (sprintf "The objintf '%s' has a duplicate name for bridgeable decl '%s'" objintf.so_name)
    >>= fun _ ->
      let validate_typs label xs =
        xs |&> (function
          | name, `bridgeable (_, bi) ->
            let name_of_bridgeable = function
              | Simple_bridgeable {
                desc =
                  ( Sole_method_bridgeable (Simple_method { name; _ })
                  | Method_bundle_bridgeable { name; _ }) } -> name
              | Complex_bridgeable {
                desc =
                  ( Sole_method_bridgeable (Simple_method { name; _ } | Complex_method { name; _ })
                  | Method_bundle_bridgeable { name; _ })
              } -> name
            in
            let bd = bridgeable_ident_resolver bi in
            let bd_name = name_of_bridgeable bd in
            objintf.so_bridgeable_decls
            |> List.exists (fun x -> name_of_bridgeable x = bd_name)
            |> (function
            | true -> Ok name
            | false -> Error (sprintf "The bridgeable decl '%s' referenced by the %s '%s' is not defined in the given objintf '%s'." bd_name label name objintf.so_name))
          | name, _ -> Ok name)
        |> sequence_list
        >>= not_duplicated_by identity (sprintf "The objintf '%s' has a duplicate name for %s '%s'" objintf.so_name label)
      in
      objintf.so_named_objects
      |> List.group_by (fun x -> x.nod_party)
      |&> (fun (_, ns) -> ns |&> (fun n -> (n.nod_name, n.nod_typ)) |> validate_typs "named object")
      |> sequence_list
      >>= fun _ ->
        objintf.so_object_registries
        |> List.group_by (fun x -> x.ord_party)
        |&> (fun (_, ns) -> ns |&> (fun n -> (n.ord_name, n.ord_typ)) |> validate_typs "object registry")
        |> sequence_list
        >|= constant objintf
