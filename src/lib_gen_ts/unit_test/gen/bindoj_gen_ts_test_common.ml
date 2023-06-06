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
open Bindoj_typedesc.Type_desc
open Bindoj_gen_ts.Typescript_datatype
open Bindoj_test_common

type resolution_strategy = [
  | `import_location of string
  | `no_resolution
]

let resolution_strategy : type_decl -> resolution_strategy = function
  | { td_name = "teacher"; _ } -> `import_location "./reused_types/teacher"
  | _ -> `no_resolution

let modules =
  let module ExG = Typedesc_generated_examples in
  ExG.all |&> (fun (name, (module G: ExG.T)) ->
    let gen () =
      let reused_intersection_types : type_decl list =
        match G.decl.td_kind with
        | Variant_decl ctors ->
          ctors |&?> (function
            | { vc_param = `reused_inline_record d; vc_configs; _ } ->
              begin match Ts_config.get_reused_variant_inline_record_style_opt vc_configs with
              | Some `intersection_type -> Some d
              | _ -> None
              end
            | _ -> None
          )
        | _ -> []
      in
      let output =
        begin reused_intersection_types
        |> List.group_by resolution_strategy
        |&?> (function | (`import_location loc, tds) -> Some (loc, tds) | _ -> None)
        |> List.map (fun (loc, tds) ->
          let tnames =
            tds
            |&> Bindoj_codec.Json.Json_config.get_mangled_name_of_type
            |> List.sort_uniq compare
          in
          sprintf "import { %s } from \"%s\";" (String.concat ", " tnames) loc
        ) end @ begin
          let types = ref [ G.decl.td_name ] in
          G.env.alias_ident_typemap
          |> StringMap.to_list
          |&?> (fun (name, boxed) ->
            let typed_decl = Typed_type_desc.Typed.unbox boxed in
            let decl = Typed_type_desc.Typed.decl typed_decl in
            if List.mem name !types then None
            else begin
              refappend types name;
              Some (gen_ts_type ~export:false decl)
            end)
      end @ [
          gen_ts_type ~export:true G.decl
        ] @ begin match G.decl.td_kind with
          | Variant_decl _ -> [gen_ts_case_analyzer ~export:true G.decl ]
          | Record_decl _ -> []
          | Alias_decl _ -> []
        end
      in
      (output |> String.concat "\n")
    in
    name, gen)
