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

open Bindoj_typedesc.Type_desc

let modules =
  let open Bindoj_test_common_typedesc_examples.All in
  all |> List.concat_map (fun (name, (module Ex : T)) -> [
    name, Ex.decl;
    name ^ "_docstr", Ex.decl_with_docstr;
  ])

let mapping =
  modules |> List.map (fun (s, m) -> sprintf "%s_gen.ml" s, m)

let gen_with_json_codec ?self_contained decl =
  let open Ppxlib in
  let open Ast_helper in
  let open Bindoj_gen.Caml_datatype in
  let open Bindoj_gen.Json_codec in
  let recursive = if is_recursive decl then Recursive else Nonrecursive in
  Astlib.Pprintast.structure Format.std_formatter [
    Str.type_ Recursive [type_declaration_of_type_decl decl];
    Str.value recursive [gen_json_encoder ?self_contained decl];
    Str.value recursive [gen_json_decoder ?self_contained decl];
  ]

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [] | _ :: _ :: _ ->
    failwith "usage: gen <filename>"
  | [name] ->
    match List.assoc_opt name mapping with
    | None -> failwith (sprintf "unknown example %s" name)
    | Some decl -> gen_with_json_codec ~self_contained:true decl
