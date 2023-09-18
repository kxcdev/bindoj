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
open Bindoj_typedesc.Typed_type_desc
open Bindoj_typedesc.Type_desc
open Typescript_datatype

type resolution_strategy = [
  | `import_location of string
  | `no_resolution
]

let generate_import_and_env ~(env: tdenv) ~resolution_strategy ~formatter (decls: type_decl list): unit =
  let collect decls : type_decl list =
    let collect_nested f xs =
      let ds = xs |&?> (fun x ->
        match f x with
        | `nested (d, _) -> Some d
        | _ -> None)
      in
      ds
    in
    decls |&>> (function
      | { td_kind = Alias_decl _; _ } -> []
      | { td_kind = Record_decl fields; _ } -> collect_nested (fun r -> r.rf_type) fields
      | { td_kind = Variant_decl ctors; _ } ->
        ctors |&>> (function
          | { vc_param = `no_param; _ } -> []
          | { vc_param = `tuple_like args; _ } -> collect_nested (fun v -> v.va_type) args
          | { vc_param = `inline_record fields; _ } -> collect_nested (fun r -> r.rf_type) fields
          | { vc_param = `reused_inline_record d; vc_configs; _ } ->
            begin match Ts_config.get_reused_variant_inline_record_style_opt vc_configs with
            | Some `intersection_type -> [ d ]
            | _ -> []
            end))
  in

  let types = ref (decls |&> (fun { td_name; _ } -> td_name)) in

  collect decls
  |> List.group_by resolution_strategy
  |> List.iter (function
    | (`import_location loc, tds) ->
      tds
      |?> (fun td -> List.mem td.td_name !types |> not)
      |> (function
      | [] -> ()
      | tds ->
        types := tds |> List.foldl (fun ts { td_name; _ } -> td_name :: ts) !types;
        let tnames =
          tds
          |&> (Ts_config.get_mangled_name_of_type &> fst)
          |> List.sort_uniq compare
        in
        sprintf "import { %s } from \"%s\";" (String.concat ", " tnames) loc
        |> Format.pp_print_string formatter
        |> Format.pp_print_newline formatter)
    | _ -> ()
  );

  env.alias_ident_typemap
  |> StringMap.iter(fun name boxed ->
    let typed_decl = Typed.unbox boxed in
    let decl = Typed.decl typed_decl in
    if List.mem name !types then ()
    else begin
      refappend types name;
      gen_ts_type ~export:false decl
      |> Format.pp_print_string formatter
      |> Format.pp_print_newline formatter
    end)

let generate_decl ~formatter (decl: type_decl): unit =
  let () =
    gen_ts_type ~export:true decl
    |> Format.pp_print_string formatter
    |> Format.pp_print_newline formatter
  in
  match decl.td_kind with
  | Variant_decl _ ->
    gen_ts_case_analyzer ~export:true decl
    |> Format.pp_print_string formatter
    |> Format.pp_print_newline formatter
  | Record_decl _ | Alias_decl _ -> ()

let generate ~resolution_strategy ~(env: tdenv) ~formatter (decls: type_decl list): unit =
  generate_import_and_env ~env ~resolution_strategy ~formatter decls;
  let rec loop = function
    | [] -> ()
    | hd :: [] -> generate_decl ~formatter hd
    | hd :: tl ->
      generate_decl ~formatter hd
      |> Format.pp_print_newline formatter;
      loop tl
  in loop decls
