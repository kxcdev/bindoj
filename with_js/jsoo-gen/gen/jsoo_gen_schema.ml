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
open Bindoj_test_common.Typedesc_examples
open Bindoj_gen
open Bindoj_openapi.V3


let schema_to_json (module Ex : T) =
  Ex.decl_with_docstr
  |> Json_codec.gen_json_schema
  |> Schema_object.to_json
  |> Json.to_yojson
  |> Yojson.Safe.to_string
  |> fun s -> s ^ "\n"

open Bindoj_test_common_jsoo_utils
open Prr

let () =
  Js_of_ocaml.Js.export "jsoo_gen_schema" (object%js
    val generator_js = object%js
      val module_names_js = all |> Jv.(of_list (fst &> of_string)) |> cast
      method generate_js name =
        let name = ostr name in
        match List.assoc_opt name all with
        | None -> failwith' "unknown example %s" name
        | Some m -> schema_to_json m |> jstr
    end
  end)
