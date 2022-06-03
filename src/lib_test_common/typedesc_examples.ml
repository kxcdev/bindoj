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

include Bindoj_test_common_typedesc_examples
open Bindoj_base.Type_desc
open Bindoj_gen_foreign.Foreign_datatype
open Bindoj_gen_ts.Typescript_datatype

(** each example module should have this module type *)
module type T = sig
  val decl: type_decl
  val decl_with_docstr: type_decl
  val fwrt: (unit, unit) fwrt_decl
  val ts_ast: ts_ast option
end

(** this should contain all the example modules. *)
let all : (string * (module T)) list = [
  "ex01", (module Ex01);
  "ex02", (module Ex02);
  "ex03", (module Ex03);
  "ex04", (module Ex04);
]
