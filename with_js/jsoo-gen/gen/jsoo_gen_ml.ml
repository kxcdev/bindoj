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
open Bindoj_test_common_jsoo_utils
open Bindoj_gen
open Bindoj_gen_test_gen_common
open Prr

let modules =
  let open Bindoj_test_common_typedesc_examples.All in
  all |&> fst

let () =
  Js_of_ocaml.Js.export "jsoo_gen_ml" (object%js
    val coverage_helper_js = coverage_helper_js
    val generator_js = object%js
      val module_names_js = modules |> Jv.(of_list of_string) |> cast
      method generate_js name gen_type_decl =
        let name = ostr name in
        let gen_type_decl = Jv.to_bool gen_type_decl in

        match List.assoc_opt (name^"_gen.ml") mapping with
        | None -> failwith (sprintf "unknown example %s" name)
        | Some (_, decls) ->
          let gen_json_shape_explanation = true in
          let discriminator_value_accessor = true in
          let write_to_string (writer : formatter:ppf -> unit) =
            ignore (Format.flush_str_formatter());
            writer ~formatter:Format.str_formatter;
            Format.flush_str_formatter()
          in
          object%js
            val structure_js =
              decls |> generate (fun (path, decl) ->
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
              |> write_to_string |> jstr

            val signature_js =
              decls |> generate (fun (_, decl) ->
                  Generator.gen_signature_with_json_codec
                    ~gen_json_shape_explanation
                    ~discriminator_value_accessor
                    ~gen_type_decl
                    decl
                )
              |> write_to_string |> jstr
          end
    end
  end)
