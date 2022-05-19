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

type variant_type_flavor = [
    `flat_kind
  (* | `tuple *)
  (* | `nested_kind *)
  ]
type ('pos, 'flavor) flavor_config +=
   | Flvconfig_flat_kind : {
       kind_fname : string option;
       arg_fname : string option;
     } -> ([ `branch ], [ `flat_kind ]) flavor_config

val gen_primitive_encoders : codec -> value_binding list

val gen_primitive_decoders : codec -> value_binding list

val gen_json_encoder : ?self_contained:bool -> ?flavor:variant_type_flavor -> ?codec:codec -> type_decl -> value_binding

val gen_json_decoder : ?self_contained:bool -> ?flavor:variant_type_flavor -> ?codec:codec -> type_decl -> value_binding

val kind_fname_value : string option -> string
val arg_fname_value : string option -> string
val default_kind_fname : string
val default_arg_fname : string
