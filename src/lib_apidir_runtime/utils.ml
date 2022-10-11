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
open Bindoj_apidir_shared

module Exceptions = struct
  exception Bad_request of string

  exception Unrecognized_route of string
end
include Exceptions

let bad_request fmt =
  Format.kasprintf (fun str -> raise (Bad_request str)) fmt

type backtrace_info = [
  | `ocaml_backtrace of Printexc.raw_backtrace
  | `string_stacktrace of string
]

let pp_backtrace_info ppf = function
  | `ocaml_backtrace rb ->
    Format.fprintf ppf "%s" (Printexc.raw_backtrace_to_string rb)
  | `string_stacktrace s ->
    Format.fprintf ppf "%s" s

module type IoStyle = sig

  (** exceptions thrown in executing second argument of [bind]
      is expected to be caught and could be retrieved using
      extract_error *)

  type 'x t
  val return : 'x -> 'x t
  val bind : 'x t -> ('x -> 'y t) -> 'y t

  val inject_error : exn -> 'x t
  val extract_error : 'x t -> ('x, exn*backtrace_info option) result t

  val pp : (ppf -> 'x -> unit) -> ppf -> 'x t -> unit
end

let show_jv jv = Json.to_yojson jv |> Yojson.Safe.to_string
let pp_jv ppf jv = pp_string ppf (show_jv jv)

let tdenv_of_registry_info registry_info =
  let open Bindoj_base in
  let open Typed_type_desc in
  let _, {
      type_declarations;
      type_decl_environment_wrappers
    } = registry_info in
  let alias_ident_typemap =
    type_declarations
    |> foldl (fun acc info ->
           acc |> StringMap.add info.tdi_name info.tdi_decl
         ) StringMap.empty in
  let env0 = Type_decl_environment.{
        alias_ident_typemap;
        prim_ident_typemap = StringMap.empty;
             } in
  type_decl_environment_wrappers |> List.foldl (|>) env0
