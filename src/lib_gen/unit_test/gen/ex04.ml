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

module Ex = Bindoj_test_common_typedesc_examples.Ex04
open Bindoj_base.Type_desc
open Bindoj_gen.Caml_datatype
open Bindoj_gen.Json_codec

let name = "ex04"
let gen () = Utils.gen_with_json_codec ~self_contained:true Ex.decl

module Docstr = struct
  let name = "ex04_docstr"
  let gen () = Utils.gen_with_json_codec ~self_contained:true Ex.decl_with_docstr
end
