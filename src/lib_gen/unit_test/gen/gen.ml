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
open Bindoj_base
open Bindoj_base.Type_desc
open Bindoj_gen
open Bindoj_gen_test_gen_common

let () =
  let gen_type_decl = ArgOptions.has_flag "-gen-type-decl" in
  match Array.to_list Sys.argv |> List.tl with
  | [] ->
    failwith "usage: gen <filename> [-gen-type-decl]"
  | name :: _ ->
    let gen_json_shape_explanation = true in
    let discriminator_value_accessor = true in
    let formatter = Format.std_formatter in
    match List.assoc_opt name mapping with
    | None -> failwith (sprintf "unknown example %s" name)
    | Some (`structure, decls) ->
      decls |> generate ~formatter
        (fun (path, decl) ->
          let type_decl =
            if gen_type_decl then Some (`path path)
            else None
          in
          Generator.gen_structure_with_json_codec
            ~self_contained:true
            ~gen_json_shape_explanation
            ~discriminator_value_accessor
            ?type_decl
            decl
        )
    | Some (`signature, decls) ->
      decls |> generate ~formatter
        (fun (_, decl) ->
          Generator.gen_signature_with_json_codec
            ~gen_json_shape_explanation
            ~discriminator_value_accessor
            ~gen_type_decl
            decl
        )
