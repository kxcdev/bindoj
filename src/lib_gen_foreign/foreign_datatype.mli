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

type ('ann0, 'ann1) fwrt_desc = {
  fd_name : string;
  fd_kind_fname : string;
  fd_parent : string option;
  fd_children : string list;
  fd_fields : 'ann1 fwrt_field_desc with_docstr list;
  fd_annot : 'ann0;
}
and 'ann fwrt_field_desc = {
  ff_name : string;
  ff_type : string list;
  ff_annot : 'ann;
}

module FwrtTypeEnv : sig
  type ('ann0, 'ann1) t
  val init : ('ann0, 'ann1) t
  val bind : ?doc:([`docstr of string | `nodoc]) -> ?parent:(string option) -> ?kind_fname:(string) -> annot:'ann0 ->
    string -> 'ann1 fwrt_field_desc with_docstr list -> ('ann0, 'ann1) t -> ('ann0, 'ann1) t
  val lookup : string -> ('ann0, 'ann1) t -> ('ann0, 'ann1) fwrt_desc with_docstr
  val lookup_opt : string -> ('ann0, 'ann1) t -> ('ann0, 'ann1) fwrt_desc with_docstr  option
  val annotate : string -> ('ann0 * 'ann0) -> ('ann1 * 'ann1) -> (unit, unit) t -> ('ann0, 'ann1) t
  val bindings : ('ann0, 'ann1) t -> (string * ('ann0, 'ann1) fwrt_desc with_docstr) list
end

type ('ann0, 'ann1) fwrt_type_env = ('ann0, 'ann1) FwrtTypeEnv.t

type ('ann0, 'ann1) fwrt_decl = string * ('ann0, 'ann1) fwrt_type_env

type flavor = Bindoj_gen.Json_codec.variant_type_flavor

module TypeMap : sig
  type t
  val empty : t
  val add_convertion : string -> string -> t -> t
  val add_fixed_point : string -> t -> t
  val convert_type : t -> string -> string
end

type type_map = TypeMap.t

val fwrt_decl_of_type_decl : flavor -> type_decl -> (unit, unit) fwrt_decl
