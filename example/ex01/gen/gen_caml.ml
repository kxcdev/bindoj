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

let generate ~formatter f decls =
  let rec loop = function
    | [] -> ()
    | hd :: tl ->
      (f hd) ~formatter;
      Format.pp_print_newline formatter ();
      if not (List.empty tl) then
        Format.pp_print_newline formatter ();
      loop tl
  in
  loop decls

let () =
  let self_contained = true in
  let gen_json_shape_explanation = true in
  let discriminator_value_accessor = true in

  let module Ex = Bindoj_example_ex01_typedesc.Typedesc in
  let module_name = "Bindoj_example_ex01_typedesc.Typedesc" in

  let gen_type_decl = ArgOptions.has_flag "-gen-type-decl" in
  match Array.to_list Sys.argv |> List.tl with
  | "ml" :: _ ->
      generate
        ~formatter:Format.std_formatter
        (fun (name, decl) ->
          let type_decl =
            if gen_type_decl then
              `path (module_name^"."^name) |> some
            else none
          in
          Generator.gen_structure_with_json_codec
            ?type_decl
            ~self_contained
            ~gen_json_shape_explanation
            ~discriminator_value_accessor
            decl)
        Ex.decls
  | "mli" :: _ ->
      generate
        ~formatter:Format.std_formatter
        (fun (_, decl) ->
            Generator.gen_signature_with_json_codec
              ~gen_type_decl
              ~gen_json_shape_explanation
              ~discriminator_value_accessor
              decl)
        Ex.decls
  | _ -> failwith "usage: gen_ocaml <ml|mli> [-gen-type-decl]"
