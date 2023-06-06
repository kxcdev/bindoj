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
open Bindoj_base
open Bindoj_base.Type_desc

let modules =
  let open Bindoj_test_common_typedesc_examples.All in
  all |> List.concat_map (fun (name, (module Ex : T)) -> [
    name, Ex.decl, Ex.example_module_path;
    name ^ "_docstr", Ex.decl_with_docstr, Ex.example_module_path;
  ])

type generate_target = [
  | `structure
  | `signature
]

let mapping : (string * (generate_target * (type_decl * string))) list =
  modules |> List.concat_map (fun (s, m, p) -> [
    sprintf "%s_gen.ml" s, (`structure, (m, p));
    sprintf "%s_gen.mli" s, (`signature, (m, p));
  ])

let gen_structure_with_json_codec
  ?self_contained
  ?gen_json_shape_explanation
  ?discriminator_value_accessor
  ?json_shape_explanation_resolution
  ?codec ~gen_type_decl (decl, emp) =
  let open Bindoj_gen in
  let type_decl =
    if gen_type_decl then (
      `path (emp^".decl") |> some
    ) else none in
  let structure =
    Caml_datatype.gen_structure
      ?type_decl
      ?codec
      ~generators:[
        Json_codec.gen_json_codec
          ?self_contained
          ?gen_json_shape_explanation
          ?json_shape_explanation_resolution
          ?discriminator_value_accessor;
      ]
      decl
  in
  Astlib.Pprintast.structure Format.std_formatter structure

let gen_signature_with_json_codec
  ?gen_json_shape_explanation
  ?discriminator_value_accessor
  ?codec ~gen_type_decl (decl, _) =
  let open Bindoj_gen in
  let structure =
    Caml_datatype.gen_signature
      ~type_decl:gen_type_decl
      ?codec
      ~generators:[
        Json_codec.gen_json_codec_signature
          ?gen_json_shape_explanation
          ?discriminator_value_accessor;
      ]
      decl
  in
  Astlib.Pprintast.signature Format.std_formatter structure

let () =
  let gen_type_decl = ArgOptions.has_flag "-gen-type-decl" in
  match Array.to_list Sys.argv |> List.tl with
  | [] ->
    failwith "usage: gen <filename> [-gen-type-decl]"
  | name :: _ ->
    let gen_json_shape_explanation = true in
    let discriminator_value_accessor = true in
    match List.assoc_opt name mapping with
    | None -> failwith (sprintf "unknown example %s" name)
    | Some (`structure, decl) -> gen_structure_with_json_codec ~self_contained:true ~gen_json_shape_explanation ~discriminator_value_accessor decl ~gen_type_decl
    | Some (`signature, decl) -> gen_signature_with_json_codec ~gen_json_shape_explanation ~discriminator_value_accessor decl ~gen_type_decl
