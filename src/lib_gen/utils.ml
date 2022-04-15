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
open Ast_builder.Default

let locmk = Located.mk
let lidloc ~loc x = locmk ~loc (lident x)
let typcons ~loc x = ptyp_constr ~loc (lidloc ~loc x) []

let doc_attributes = function
  | `docstr doc ->
    let loc = Location.none in
    [attribute ~loc
       ~name:(Located.mk ~loc "ocaml.doc")
       ~payload:(PStr [pstr_eval ~loc (estring ~loc doc) []])]
  | `nodoc -> []
  | _ -> failwith "unknown polymorphic variant for docstr"
let warning_attributes str =
  let loc = Location.none in
  [attribute ~loc
     ~name:(Located.mk ~loc "warning")
     ~payload:(PStr [pstr_eval ~loc (estring ~loc str) []])]

let sprintf fmt = Format.asprintf fmt
