(* Copyright 2022 Kotoi-Xie Consultancy, Inc.
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. *)

(* Acknowledgements - AnchorZ Inc.
The initial version or a significant portion of this file is developed
under the funding of AnchorZ Inc. to satisfy its needs in
product development. *)

open Bindoj_base
open Bindoj_base.Type_desc

let modules =
  let open Bindoj_test_common_typedesc_examples.All in
  all |> List.concat_map (fun (name, (module Ex : T)) -> [
    name, Ex.decl, Ex.example_module_path;
    name ^ "_docstr", Ex.decl_with_docstr, Ex.example_module_path;
  ])

let mapping =
  modules |> List.map (fun (s, m, p) -> sprintf "%s_gen.ml" s, (m, p))

let gen_with_json_codec ?self_contained ?codec ~gen_type_decl (decl, emp) =
  let open Bindoj_gen.Caml_datatype in
  let open Bindoj_gen.Json_codec in
  let type_decl =
    if gen_type_decl then (
      `path (emp^".decl") |> some
    ) else none in
  let structure =
    gen_structure
      ?type_decl
      ?codec
      ~generators:[
        gen_json_codec ?self_contained;
      ]
      decl
  in
  Astlib.Pprintast.structure Format.std_formatter structure

let () =
  let gen_type_decl = ArgOptions.has_flag "-gen-type-decl" in
  match Array.to_list Sys.argv |> List.tl with
  | [] ->
    failwith "usage: gen <filename> [-gen-type-decl]"
  | name :: _ ->
    match List.assoc_opt name mapping with
    | None -> failwith (sprintf "unknown example %s" name)
    | Some decl -> gen_with_json_codec ~self_contained:true decl ~gen_type_decl
