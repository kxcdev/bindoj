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
open Bindoj_base.Type_desc
open Bindoj_gen_foreign.Foreign_datatype
open Bindoj_gen_ts.Typescript_datatype

(** each example module should have this module type *)
module type T = sig
  val decl: type_decl
  val example_module_path: string

  val decl_with_docstr: type_decl
  val fwrt: (unit, unit) fwrt_decl
  val ts_ast: ts_ast option
end

(** this should contain all the example modules. *)
let all : (string * (module T)) list = [
  "ex01", (module Ex01);
  "ex02", (module Ex02);
  "ex02_reused", (module Ex02_reused);
  "ex03", (module Ex03);
  "ex03_objtuple", (module Ex03_objtuple);
  "ex04", (module Ex04);
  "ex05", (module Ex05);
  "ex05_notuple", (module Ex05_notuple);
  "ex06", (module Ex06);
  "ex07", (module Ex07);
  "ex08", (module Ex08);
  "ex09", (module Ex09);
  "ex10", (module Ex10);
]
