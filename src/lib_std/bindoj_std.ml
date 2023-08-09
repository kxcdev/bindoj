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
(** Standard library for using Bindoj. *)
open Bindoj_base
open Bindoj_base.Typed_type_desc
open Bindoj_runtime
open Bindoj_gen_config

module Json_value : sig
  (** This module provides functionalities to represents and hanble a json value. *)

  include module type of Bindoj_std_runtime.Json_value

  val type_name : string
  (** ["json_value"] *)

  val json_shape_explanation : json_shape_explanation
  (** [`any_json_value] *)

  module Type_decl : sig
    val coretype : coretype
    val untyped : type_decl
    val typed : t typed_type_decl
    val boxed : boxed_type_decl
    val wrap_env : tdenv endo
  end
end = struct
  open Type_decl_environment
  include Bindoj_std_runtime.Json_value

  let type_name = "json_value"

  let primitive_descriptor : t user_primitive_descriptor =
    { external_format_codecs }

  let json_shape_explanation = `any_json_value

  module Type_decl = struct
    let coretype : coretype =
      let open Bindoj_gen_ts_config in
      Coretype.mk_ident
        ~configs:Configs.[
          Ts_config.typescript_type (`type_reference "any");
          Json_config.custom_json_schema (
            Bindoj_openapi.V3.Schema_object.any ()
          );
          Json_config.custom_shape_explanation Bindoj_std_runtime.json_value_json_shape_explanation;
          Json_config.no_mangling;
        ]
        ~codec:(`in_module "Bindoj_std_runtime.Json_value")
        type_name
    let untyped : type_decl = alias_decl ~configs:[
      Json_config.no_mangling;
    ] type_name coretype
    let typed : t Typed_type_desc.typed_type_decl =
      Typed_type_desc.Typed.mk untyped reflect
    let boxed = Boxed typed

    let wrap_env env =
      let replace key x =
        StringMap.update key ((constant % some) x) in
      let replace_me_with x = replace type_name x in
      { prim_ident_typemap =
          replace_me_with
            (Boxed_prim (primitive_descriptor, typed))
            env.prim_ident_typemap;
        alias_ident_typemap =
          replace_me_with boxed
            env.alias_ident_typemap;
      }
  end
end

type json_value = Json_value.t

let json_value_json_shape_explanation = Json_value.json_shape_explanation

module Coretypes = struct
  let json = Json_value.Type_decl.coretype
end

module Tdenv_wrappers = struct
  let json = Json_value.Type_decl.wrap_env
end
