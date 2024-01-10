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

include module type of Objintf_common

module Objintf_config = Bindoj_objintf_shared_config.Objintf_config

type party_polarity =
  | Cis_party (** endemic is endemic and peer is peer *)
  | Trans_party (** endemic becomes peer and peer becomes endemic *)

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
  (* future:
    | Mret_multiple : 'typ list (* invariant: more than one *) -> 'typ method_return_type_descriptor
    | Mret_bag .. *)

type simple_type = [
  | `direct of coretype
  | `nested of type_decl * Coretype.codec
]

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
  (** simple bridgeable does not have bridgeables in args / return values *)

  | Complex_bridgeable : {
      desc : ([< `simple | `complex ], 'bridgeable_ident) bridgeable_descriptor;
    } -> ([> `complex ], 'bridgeable_ident) bridgeable_decl
  (** complex bridgeable may have bridgeables in args / return values *)

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
    ord_coordinate_desc : (string (* name *) * coordinate_type_desc) list; (** invariant: must be more than one *)
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

val positional_argument_regular :
  ?doc:doc
  -> ?name:string
  -> 'typ
  -> 'typ positional_argument_descriptor

val labeled_argument_regular :
  ?doc:doc
  -> optional:bool
  -> string
  -> 'arg_typ
  -> 'arg_typ labeled_argument_descriptor

val labeled_argument_with_default :
  ?doc:doc
  -> ?codec:Coretype.codec
  -> default:'x
  -> string
  -> 'x typed_type_decl
  -> _ labeled_argument_descriptor

val method_return_type_regular : 'typ -> 'typ method_return_type_descriptor

val simple_method :
  ?doc:doc
  -> string
  -> ?pargs:simple_type positional_argument_descriptor list
  -> ?largs:simple_type labeled_argument_descriptor list
  -> simple_type method_return_type_descriptor
  -> ([> `simple ], _) method_descriptor

val complex_method :
  ?doc:doc
  -> string
  -> ?pargs:'bridgeable_ident complex_type positional_argument_descriptor list
  -> ?largs:'bridgeable_ident complex_type labeled_argument_descriptor list
  -> 'bridgeable_ident complex_type method_return_type_descriptor
  -> ([> `complex ], 'bridgeable_ident) method_descriptor

val sole_method_bridgeable :
  ('cplx, 'bridgeable_ident) method_descriptor
  -> ('cplx, 'bridgeable_ident) bridgeable_descriptor

val method_bundle_bridgeable :
  ?doc:doc
  -> ?configs:[ `method_bundle_brideable_decl ] configs
  -> string
  -> ('cplx, 'bridgeable_ident) method_descriptor list
  -> ('cplx, 'bridgeable_ident) bridgeable_descriptor

val simple_bridgeable :
  ([ `simple ], 'bridgeable_ident) bridgeable_descriptor
  -> ([> `simple ], 'bridgeable_ident) bridgeable_decl

val complex_bridgeable :
  ([< `simple | `complex ], 'bridgeable_ident) bridgeable_descriptor
  -> ([> `complex ], 'bridgeable_ident) bridgeable_decl

val named_object_decl :
  ?doc:doc
  -> string
  -> party:'party
  -> typ: 'bridgeable_ident registry_type
  -> ('party, 'bridgeable_ident) named_object_decl

val object_registry_decl :
  ?doc:doc
  -> string
  -> coordinate_desc:(string * coordinate_type_desc) list
  -> party:'party
  -> typ:'bridgeable_ident registry_type
  -> ('party, 'bridgeable_ident) object_registry_decl

val sameworld_objintf :
  name:string
  -> ?doc:doc
  -> ?polarity:party_polarity
  -> ?named_objects:([ `endemic | `peer ], 'bridgeable_ident) named_object_decl list
  -> ?object_registries:([ `endemic | `peer ], 'bridgeable_ident) object_registry_decl list
  -> ([ `simple | `complex ], 'bridgeable_ident) bridgeable_decl list
  -> 'bridgeable_ident sameworld_objintf

val validate_objintf :
  bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
  -> 'bridgeable_ident sameworld_objintf
  -> ('bridgeable_ident sameworld_objintf, string) result
