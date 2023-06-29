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
open Bindoj_gen

type generate_target = [
  | `structure
  | `signature
]

let mapping : (string * (generate_target * (type_decl * string))) list =
  let open Bindoj_test_common_typedesc_examples.All in
  all
  |> List.concat_map (fun (name, (module Ex : T)) -> [
    name, (Ex.decl, Ex.example_module_path);
    name ^ "_docstr", (Ex.decl_with_docstr, Ex.example_module_path) ])
  |> List.concat_map (fun (s, (m, p)) -> [
    sprintf "%s_gen.ml" s, (`structure, (m, p));
    sprintf "%s_gen.mli" s, (`signature, (m, p)) ])

let gen_structure_embed_full_impl =
  let open Ppxlib in
  let open Ppxlib.Ast_helper in
  let open Bindoj_gen in
  let open Bindoj_gen.Json_codec in
  let gen_json_shape_explanation' = gen_json_shape_explanation in
  fun
    ?self_contained
    ?(gen_json_shape_explanation=true)
    ?(discriminator_value_accessor=true)
    ?json_shape_explanation_resolution
    ?codec ~gen_type_decl ~formatter (decl, emp) ->
  let type_decl =
    if gen_type_decl then (
      `path (emp^".decl") |> some
    ) else none in
  let structure =
    Caml_datatype.gen_structure
      ?type_decl
      ?codec
      ~generators:[
        fun ?codec td ->
          let add_item_when cond f xs = if cond then (f ()) :: xs else xs in
          let bindings =
            [ gen_json_encoder ?self_contained ?codec td;
              gen_json_decoder_result
                ?self_contained
                ~json_shape_explanation_style:
                  ( if gen_json_shape_explanation then `reference
                    else `inline json_shape_explanation_resolution )
                ?codec
                td;
              gen_json_decoder_option
                ~implementation_style:(`embed_full_implementation (
                  match self_contained with
                  | None | Some false -> `non_self_contained
                  | Some true -> `self_contained
                ))
                ?codec td; ]
            in
            [ Str.value Recursive bindings ]
            |> add_item_when
                gen_json_shape_explanation
                (fun () ->
                  gen_json_shape_explanation' ?json_shape_explanation_resolution ?codec td
                  |> List.return |> Str.value Nonrecursive)
            |> add_item_when
                (discriminator_value_accessor && match td.td_kind with | Variant_decl _ -> true | _ -> false)
                (fun () ->
                  (gen_discriminator_value_accessor ?codec td)
                  |> List.return |> Str.value Nonrecursive)
      ]
      decl
  in
  Astlib.Pprintast.structure formatter structure

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
    | Some (`structure, decl) ->
      gen_structure_embed_full_impl
        ~self_contained:true
        ~gen_json_shape_explanation
        ~discriminator_value_accessor
        ~gen_type_decl
        ~formatter
        decl
    | Some (`signature, (decl, _)) ->
      Generator.gen_signature_with_json_codec
        ~gen_json_shape_explanation
        ~discriminator_value_accessor
        ~gen_type_decl
        ~formatter
        decl
