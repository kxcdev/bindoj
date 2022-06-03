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

open Bindoj_gen_ts.Typescript_datatype
open Bindoj_test_common


let testable_ts_ast =
  let same_contents a b =
    List.sort compare a = List.sort compare b in
  let rec ts_ast_equal : ts_ast -> ts_ast -> bool = fun ast1 ast2 ->
    List.for_all2 ts_statement_equal ast1 ast2
  and ts_statement_equal : ts_statement -> ts_statement -> bool = fun stat1 stat2 -> match stat1, stat2 with
    | `type_alias_declaration { tsa_modifiers = mods1; tsa_name = name1;
                                tsa_type_parameters = params1; tsa_type_desc = type1; },
      `type_alias_declaration { tsa_modifiers = mods2; tsa_name = name2;
                                tsa_type_parameters = params2; tsa_type_desc = type2; } ->
      same_contents mods1 mods2 && name1 = name2 && params1 = params2 && ts_type_desc_equal type1 type2
    | `function_declaration { tsf_modifiers = mods1; tsf_name = name1; tsf_type_parameters = typ_params1;
                              tsf_parameters = params1; tsf_type_desc = type1; tsf_body = body1; },
      `function_declaration { tsf_modifiers = mods2; tsf_name = name2; tsf_type_parameters = typ_params2;
                              tsf_parameters = params2; tsf_type_desc = type2; tsf_body = body2; } ->
      same_contents mods1 mods2 && name1 = name2 && typ_params1 = typ_params2 &&
      List.for_all2 ts_parameter_equal params1 params2 && ts_type_desc_equal type1 type2 && ts_ast_equal body1 body2
    | `return_statement expr1, `return_statement expr2 -> expr1 = expr2
    | `if_statement (expr1, then1, else1), `if_statement (expr2, then2, else2) ->
      expr1 = expr2 && ts_statement_equal then1 then2 && ts_statement_equal else1 else2
    | `throw_statement expr1, `throw_statement expr2 -> expr1 = expr2
    | `block ast1, `block ast2 -> ts_ast_equal ast1 ast2
    | _ -> false
  and ts_type_desc_equal : ts_type_desc -> ts_type_desc -> bool = fun type1 type2 -> match type1, type2 with
    | `type_reference x1, `type_reference x2 -> x1 = x2
    | `type_literal members1, `type_literal members2  ->
      List.for_all2 (fun member1 member2 ->
          let { tsps_modifiers = mods1; tsps_name = name1; tsps_type_desc = type1; } = member1 in
          let { tsps_modifiers = mods2; tsps_name = name2; tsps_type_desc = type2; } = member2 in
          same_contents mods1 mods2 && name1 = name2 && ts_type_desc_equal type1 type2)
        members1 members2
    | `literal_type x1, `literal_type x2 -> x1 = x2
    | `tuple types1, `tuple types2 -> List.for_all2 ts_type_desc_equal types1 types2
    | `union types1, `union types2 -> same_contents types1 types2
    | `func_type f1, `func_type f2 -> f1 = f2
    | _ -> false
  and ts_parameter_equal : ts_parameter -> ts_parameter -> bool =
    fun { tsp_name = name1; tsp_type_desc = type1; } { tsp_name = name2; tsp_type_desc = type2; } ->
      name1 = name2 && ts_type_desc_equal type1 type2 in
  Alcotest.testable pp_ts_ast ts_ast_equal

let create_cases doc (module Ex : Typedesc_examples.T) =
  let open Alcotest in
  let create_test () =
    Alcotest.check testable_ts_ast doc
      (ts_ast_of_fwrt_decl (annotate_fwrt_decl true false Ex.fwrt)) (Option.get Ex.ts_ast) in
  (doc, [test_case "ts_ast_of_fwrt_decl and annotate_fwrt_decl work" `Quick create_test])

let () =
  let open Alcotest in
  let open Kxclib in
  Typedesc_examples.all
  |&> (fun (name, m) -> create_cases name m)
  |> run "lib_gen_ts: ts_ast"
