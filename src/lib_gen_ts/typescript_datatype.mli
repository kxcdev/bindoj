(* Copyright 2022 Kotoi-Xie Consultancy, Inc. This file is a part of the

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
val string_of_ts_fwrt_constructor_kind_annot : ts_fwrt_constructor_kind_annot -> string
val equal_ts_fwrt_constructor_kind_annot : ts_fwrt_constructor_kind_annot -> ts_fwrt_constructor_kind_annot -> bool

val pp_ts_fwrt_constructor_kind_info : ppf -> ts_fwrt_constructor_kind_info -> unit
val string_of_ts_fwrt_constructor_kind_info : ts_fwrt_constructor_kind_info -> string
val equal_ts_fwrt_constructor_kind_info : ts_fwrt_constructor_kind_info -> ts_fwrt_constructor_kind_info -> bool

val pp_ts_ast : ppf -> ts_ast -> unit
val show_ts_ast : ts_ast -> string
val equal_ts_ast : ts_ast -> ts_ast -> bool

type ('ann_d, 'ann_f) ts_fwrt_decl = ('ann_d, 'ann_f, unit*unit*ts_fwrt_constructor_kind_annot) fwrt_decl

type fwrt_decl_of_ts = (ts_modifier list, [`readonly] list) ts_fwrt_decl

val ts_fwrt_decl_of_type_decl : export:bool -> readonly:bool -> type_decl -> fwrt_decl_of_ts
val ts_ast_of_fwrt_decl : fwrt_decl_of_ts -> ts_ast
val ts_type_alias_decl_of_fwrt_decl : self_type_name:string -> fwrt_decl_of_ts -> ts_type_alias_decl

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
val gen_ts_case_analyzer : ?export:bool -> ?name:string -> type_decl -> string
