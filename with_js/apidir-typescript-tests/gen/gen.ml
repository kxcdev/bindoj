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
open Bindoj_typedesc.Typed_type_desc
open Bindoj_test_common.Apidir_examples

let print_typescript name (module Dir : T) =
  let reg_info = Dir.registry_info () in
  let namespace = "bindoj" in
  Bindoj_apidir_typescript.gen_raw
    ~resolution_strategy:(Typed.unbox &> Typed.decl &> function
      | { td_kind = Alias_decl { ct_desc = Prim _; _ }; _ } -> `infile_type_definition `no_export
      | { td_kind = Alias_decl _; _ } -> `inline_type_definition
      | decl ->
        begin match decl.td_name |> String.split_on_char '_' with
        | "ex" :: ex_name :: _ -> `import_location (sprintf "../compile-tests/ex_%s_gen" ex_name)
        | _ -> `no_resolution
        end)
    ~bindoj_namespace:namespace
    ~mod_name:name
    reg_info
  |> sprintf "import { apidir as %s } from \"../public-packages/runtime/index\";\n%s" namespace
  |> print_endline

let mapping =
  all |> List.map (fun (name, m) -> sprintf "%s.ts" name, (name, m))

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [] | _ :: _ :: _ ->
    failwith "usage: gen <filename>"
  | [name] ->
    match List.assoc_opt name mapping with
    | None -> failwith (sprintf "unknown example %s" name)
    | Some (name, m) -> print_typescript name m
