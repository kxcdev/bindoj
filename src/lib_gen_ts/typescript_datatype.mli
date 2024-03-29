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
open Bindoj_gen_foreign.Foreign_datatype

include module type of Bindoj_gen_ts_config

type ts_fwrt_constructor_kind_annot = ts_fwrt_constructor_kind_info option
and ts_fwrt_constructor_kind_info =
  | Tfcki_reused_variant_inline_record of type_decl

val pp_ts_fwrt_constructor_kind_annot : ppf -> ts_fwrt_constructor_kind_annot -> unit
(** Pretty printer for the {!ts_fwrt_constructor_kind_annot}. *)

val string_of_ts_fwrt_constructor_kind_annot : ts_fwrt_constructor_kind_annot -> string
val equal_ts_fwrt_constructor_kind_annot : ts_fwrt_constructor_kind_annot -> ts_fwrt_constructor_kind_annot -> bool
(** Checks equality of two ts_fwrt_constructor_kind_annots. *)

val pp_ts_fwrt_constructor_kind_info : ppf -> ts_fwrt_constructor_kind_info -> unit
(** Pretty printer for the {!ts_fwrt_constructor_kind_info}. *)
val string_of_ts_fwrt_constructor_kind_info : ts_fwrt_constructor_kind_info -> string
val equal_ts_fwrt_constructor_kind_info : ts_fwrt_constructor_kind_info -> ts_fwrt_constructor_kind_info -> bool
(** Checks equality of two ts_fwrt_constructor_kind_info. *)

val pp_ts_ast : ppf -> ts_ast -> unit
(** Pretty printer for the {!ts_ast}. *)
val show_ts_ast : ts_ast -> string
val equal_ts_ast : ts_ast -> ts_ast -> bool
(** Checks equality of two ts_asts. *)

val type_of_coretype : ?definitive:bool -> ?self_json_name:string -> Ts_config.json_mangling_style -> coretype -> ts_type_desc

type ('ann_d, 'ann_f, 'ann_va) ts_fwrt_decl = ('ann_d, 'ann_f, 'ann_va, unit*unit*ts_fwrt_constructor_kind_annot) fwrt_decl

type fwrt_decl_of_ts = (ts_modifier list, [`readonly] list, [`readonly] list) ts_fwrt_decl

val ts_fwrt_decl_of_type_decl : export:bool -> readonly:bool -> type_decl -> fwrt_decl_of_ts
(** Creates a fwrt declaration of the given type declaration. *)

val ts_ast_of_fwrt_decl : fwrt_decl_of_ts -> ts_ast
(** Creates a ts ast of the given fwrt declaration. *)

val ts_type_desc_of_fwrt_decl : ?self_json_name:string -> fwrt_decl_of_ts -> ts_type_desc
(** Creates a type description of the given fwrt declaration. *)

val ts_type_alias_decl_of_fwrt_decl : ?self_json_name:string -> fwrt_decl_of_ts -> ts_type_alias_decl
(** Creates a type alias declaration of the given fwrt declaration. *)

module Rope : sig
  type t
  val to_string : t -> string
end

module Internals : sig
  val rope_of_ts_ast : ts_ast -> Rope.t
  val rope_of_ts_statement : ts_statement -> Rope.t
  val rope_of_ts_type_desc : ts_type_desc -> Rope.t
  val rope_of_ts_expression : ts_expression -> Rope.t
end

val gen_ts_type : ?export:bool -> type_decl -> string
(** Generates a TypeScript code of the given type declaration. *)
val gen_ts_case_analyzer : ?export:bool -> ?name:string -> type_decl -> string
(** Generates a TypeScript code of case analyzer of the given type declaration of {!Bindoj_runtime.Variant_decl}. *)
