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
open Ast_helper

let locmk ?loc txt = { txt; loc = loc |? !Ast_helper.default_loc }
let strloc ?loc x : label with_loc = locmk ?loc x
let lidloc ?loc x = locmk ?loc (Longident.parse x)

let typcons ?loc ?attrs x = Typ.constr ?loc ?attrs (lidloc ?loc x) []
let pvar ?loc ?attrs s = Pat.var ?loc ?attrs (strloc s)
let evar ?loc ?attrs s = Exp.ident ?loc ?attrs (lidloc s)

let attr name value =
  Attr.mk (locmk name) (PStr [Str.eval value])
let doc_attribute = function
  | `docstr doc -> [attr "ocaml.doc" (Exp.constant (Const.string doc))]
  | `nodoc -> []
  | _ -> failwith "unknown polymorphic variant for docstr"
let show_attribute = [attr "deriving" (Exp.ident (lidloc "show"))]
let warning_attribute str = [attr "warning" (Exp.constant (Const.string str))]

let sprintf fmt = Format.asprintf fmt
