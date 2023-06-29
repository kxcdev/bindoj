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
open Bindoj_typedesc.Typed_type_desc
open Bindoj_codec.Json
open Bindoj_base

let () =
  let module Ex = Bindoj_example_ex02_typedesc.Typedesc in
  let module Dir = Bindoj_example_ex02_apidir.Apidir in
  let reg_info = Dir.registry_info () in
  let namespace = "bindoj" in
  Bindoj_apidir_typescript.gen_raw
    ~resolution_strategy:(Typed.unbox &> Typed.decl &> function
      | { td_name = name; _ } ->
        if Ex.decls |> List.exists (fun (_, { td_name; _ }) -> td_name = name) then
          `import_location "./ex02"
        else`inline_type_definition
    )
    ~bindoj_namespace:namespace
    ~mod_name:"ex02"
    reg_info
  |> sprintf "import { apidir as %s } from \"../../../with_js/public-packages/runtime/index\";\n%s" namespace
  |> print_endline
