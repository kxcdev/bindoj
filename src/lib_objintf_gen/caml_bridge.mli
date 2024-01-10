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
open Ppxlib
open Bindoj_objintf_shared
open Bindoj_typedesc.Typed_type_desc

module Bridge_labels = Bridge_labels

(** Specifies how the referenced [type_decl] is resolved. *)
type resolution_strategy = [
  | `no_resolution
  | `inline_type_definition
  | `infile_type_definition of [`path of string | `expr of expression] option
]

val type_of_type_decl :
  ?attrs:attribute list
  -> resolution_strategy:(type_decl -> Coretype.codec -> [> `inline_type_definition ])
  -> type_decl
  -> Coretype.codec
  -> core_type

(** Generate type definitions. *)
val gen_structure :
  ?generators:(
    resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
    -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
    -> 'bridgeable_ident sameworld_objintf
    -> structure
  ) list
  -> ?resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
  -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
  -> 'bridgeable_ident sameworld_objintf
  -> structure

(** Represents encoder/decoder expressions of Coretype. *)
type builtin_codec = {
  encoder: expression;
  decoder: expression;
}

module type Full_bridge_impl = sig
  (** Represents an implementation for generating a full bridge.
      For example, [Bindoj_objintf_gen_jsoo.Jsoo_full_bridge.jsoo_full_bridge_impl] creates an implementation to generate a full bridge for js_of_ocaml.
  *)

  type bridgeable_ident

  (** Specify the module name of the Full bridge. *)
  val get_module_name : string -> string

  val get_encoder_name : string -> string
  val get_decoder_name : string -> string

  (** Checks whether the setup of the peer full bridge has been called, and if not, generates code to call it. *)
  val gen_ensure_peer_setup : unit -> expression

  (** Generate common variables for use with encode/decoder. *)
  val gen_setup_common_codecs : unit -> value_binding list

  val gen_builtin_codec : string -> builtin_codec option
  
  (** Generate encoder of the given [type_decl]. [resolution_strategy] should be applied within this function. *)
  val gen_type_decl_encoder : type_decl -> Coretype.codec -> value_binding option
  
  (** Generate decoder of the given [type_decl]. [resolution_strategy] should be applied within this function. *)
  val gen_type_decl_decoder : type_decl -> Coretype.codec -> value_binding option
  
  val gen_encoder : ([> `simple | `complex ], bridgeable_ident) bridgeable_descriptor -> expression

  val gen_decoder : ([> `simple | `complex ], bridgeable_ident) bridgeable_descriptor -> expression

  val gen_peer_object : ([ `endemic | `peer ], bridgeable_ident) named_object_decl -> expression
  
  (** Generate code to set up endemic objects. *)
  val gen_setup_endemic_objects : unit -> expression

  (** Generate code to set up peer object registry. *)
  val gen_setup_peer_object_registry : unit -> expression

  (** Generate code to set up endemic object registry. *)
  val gen_setup_endemic_object_registry : unit -> expression

  (** Generates variables for the set-up process with the Peer. *)
  val gen_endemic_full_bridge : setup_called:expression -> setup:expression -> expression
end

type 'bridgeable_ident full_bridge_impl = (module Full_bridge_impl with type bridgeable_ident = 'bridgeable_ident)

(** Generate full bridge. *)
val gen_full_bridge_impl :
  resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
  -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
  -> impl:(
    resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
    -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
    -> 'bridgeable_ident sameworld_objintf
    -> 'bridgeable_ident full_bridge_impl)
  -> 'bridgeable_ident sameworld_objintf
  -> structure
