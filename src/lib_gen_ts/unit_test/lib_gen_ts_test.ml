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
open Bindoj_gen_ts.Typescript_datatype
open Bindoj_gen_ts.Typescript_datatype.Internals
open Bindoj_test_common.Typedesc_examples
open Bindoj_gen_foreign

module Ast = struct
  let testable_ts_ast =
    Alcotest.testable pp_ts_ast equal_ts_ast

  let annotate_fwrt_decl : bool -> bool -> (unit, unit, unit) ts_fwrt_decl -> fwrt_decl_of_ts =
    fun export readonly (name, env) ->
      (name,
      Foreign_datatype.FwrtTypeEnv.annotate
        name
        (if export then ([`export], []) else ([], []))
        (if readonly then ([`readonly], []) else ([], []))
        (if readonly then ([`readonly], []) else ([], []))
        env)

  let create_cases doc (module Ex : Ex_desc) =
    let open Alcotest in
    let ast_test_cases =
      let check a b () = Alcotest.check testable_ts_ast doc a b in
      match Ex.ts_ast with
      | Some ts_ast ->
        let create_test =
          check (ts_ast_of_fwrt_decl (annotate_fwrt_decl true false Ex.fwrt)) ts_ast in
        [test_case "ts_ast_of_fwrt_decl and annotate_fwrt_decl work" `Quick create_test]
      | None ->
        [test_case "(skipped)" `Quick (fun () -> ())]
    in
    (doc, ast_test_cases)
end

module Code = struct
  module CharStream : sig
    type t
    val peek : t -> char option
    val junk : t -> unit
    val of_string : string -> t
  end = struct
    type t = { mutable count : int; len : int; data : string; }
    let peek stream =
      if stream.count < stream.len then Some (String.get stream.data stream.count) else None
    let junk stream =
      if stream.count < stream.len then stream.count <- stream.count + 1 else ()
    let of_string s = { count = 0; len = (String.length s); data = s; }
  end

  let tokenize : string -> string list = fun code ->
    let whitespace : char list =
      [' '; '\n'; '\t'] in
    let delimiters : char list =
      whitespace
      @ ['('; ')'; '{'; '}'; '['; ']'; '<'; '>'; '|'; ','; '.'; ':'; ';';] in
    let is_whitespace : char -> bool = fun c ->
      List.mem c whitespace in
    let is_delimiter : char -> bool = fun c ->
      List.mem c delimiters in
    let rec aux : char list -> string list -> CharStream.t -> string list = fun looking acc src ->
      let string_of_chars chars =
        let buf = Buffer.create 16 in
        List.iter (Buffer.add_char buf) (List.rev chars);
        Buffer.contents buf in
      match CharStream.peek src with
      | None ->
        List.rev_append acc [string_of_chars (List.rev looking)]
      | Some head ->
        CharStream.junk src;
        if is_whitespace head then
          if looking = [] then
            aux [] acc src
          else
            aux [] (string_of_chars looking :: acc) src
        else if is_delimiter head then
          if looking = [] then
            aux [] (String.make 1 head :: acc) src
          else
            aux [] (String.make 1 head :: string_of_chars looking :: acc) src
        else
          aux (head :: looking) acc src in
    aux [] [] (CharStream.of_string code)

  let testable_code =
    let pp : string list Fmt.t = fun ppf strs ->
      match strs with
      | [] -> Format.fprintf ppf "[]"
      | _ ->
        Format.fprintf ppf "[";
        Format.fprintf ppf "\"%s\"" (List.hd strs);
        List.iter (Format.fprintf ppf "; \"%s\"") (List.tl strs);
        Format.fprintf ppf "]" in
    Alcotest.testable pp ( = )

  let cases =
    let open Alcotest in
    let check doc a b () =
      Alcotest.check testable_code doc a b in
    let test_case' doc a b =
      test_case
        (doc ^ " works")
        `Quick
        (check doc (tokenize a) (tokenize b)) in
    let var_x = "x" in
    let var_y = "y" in
    let var_z = "z" in
    let var_a = "a" in
    let var_b = "b" in
    let var_f = "f" in
    let var_i = "i" in
    let plus = "+" in
    let star = "*" in
    let number = "number" in
    let string = "string" in
    let type_A = "A" in
    let type_B = "B" in
    let type_C = "C" in
    let mod_A = "A" in
    let libs = "libs" in

    let expression_cases = [
      test_case'
        "rope_of_ts_expression - identifier"
        var_x
        (`identifier var_x
         |> rope_of_ts_expression
         |> Rope.to_string);
      test_case'
        "rope_of_ts_expression - numeric_literal"
        "42."
        (`literal_expression (`numeric_literal 42.)
         |> rope_of_ts_expression
         |> Rope.to_string);
      test_case'
        "rope_of_ts_expression - string_literal"
        "\"strlit\""
        (`literal_expression (`string_literal "strlit")
         |> rope_of_ts_expression
         |> Rope.to_string);
      test_case'
        "rope_of_ts_expression - template_literal"
        "`${number}-${number}-${number}`"
        (`literal_expression (`template_literal "${number}-${number}-${number}")
         |> rope_of_ts_expression
         |> Rope.to_string);
      test_case'
        "rope_of_ts_expression - call_expression"
        (var_f ^ "(" ^ var_x ^ ")")
        (`call_expression {
            tsce_expression = `identifier var_f;
            tsce_arguments = [`identifier var_x];
          }
         |> rope_of_ts_expression
         |> Rope.to_string);
      test_case'
        "rope_of_ts_expression - element_access_expression"
        (var_x ^ "[" ^ var_i ^ "]")
        (`element_access_expression {
            tsea_expression = `identifier var_x;
            tsea_argument = `identifier var_i;
          }
         |> rope_of_ts_expression
         |> Rope.to_string);
      test_case'
        "rope_of_ts_expression - property_access_expression"
        (var_x ^ "." ^ var_y)
        (`property_access_expression {
            tspa_expression = `identifier var_x;
            tspa_name = var_y;
          }
         |> rope_of_ts_expression
         |> Rope.to_string);
      test_case'
        "rope_of_ts_expression - binary_expression"
        (var_x ^ " " ^ plus ^ " " ^ var_y)
        (`binary_expression {
            tsbe_left = `identifier var_x;
            tsbe_operator_token = plus;
            tsbe_right = `identifier var_y;
          }
         |> rope_of_ts_expression
         |> Rope.to_string);
      test_case'
        "rope_of_ts_expression - binary_expression nested right"
        (var_x ^ " " ^ plus ^ " (" ^ var_y ^ " " ^ star ^ " " ^ var_z ^ ")")
        (`binary_expression {
            tsbe_left = `identifier var_x;
            tsbe_operator_token = plus;
            tsbe_right = `binary_expression {
                tsbe_left = `identifier var_y;
                tsbe_operator_token = star;
                tsbe_right = `identifier var_z;
              };
          }
         |> rope_of_ts_expression
         |> Rope.to_string);
      test_case'
        "rope_of_ts_expression - binary_expression nested left"
        ("(" ^ var_x ^ " " ^ plus ^ " " ^ var_y ^ ") " ^ star ^ " " ^ var_z)
        (`binary_expression {
            tsbe_left = `binary_expression {
                tsbe_left = `identifier var_x;
                tsbe_operator_token = plus;
                tsbe_right = `identifier var_y
              };
            tsbe_operator_token = star;
            tsbe_right = `identifier var_z;
          }
         |> rope_of_ts_expression
         |> Rope.to_string);
      test_case'
        "rope_of_ts_expression - arrow_function"
        ("(" ^ var_x ^ " : " ^ number ^ ", " ^ var_y ^ "? : " ^ number ^ ") => { " ^
         "return " ^ var_x ^ " " ^ plus ^ " " ^ var_y
         ^ " }")
        (`arrow_function {
            tsaf_parameters = [
              { tsp_name = var_x;
                tsp_optional = false;
                tsp_type_desc = `type_reference number; };
              { tsp_name = var_y;
                tsp_optional = true;
                tsp_type_desc = `type_reference number; };
            ];
            tsaf_body = [
              `return_statement
                (`binary_expression {
                    tsbe_left = `identifier var_x;
                    tsbe_operator_token = plus;
                    tsbe_right = `identifier var_y;
                  })
            ];
          }
         |> rope_of_ts_expression
         |> Rope.to_string);
      test_case'
        "rope_of_ts_expression - new_expression"
        ("new " ^ type_A ^ "(" ^ var_x ^ ")")
        (`new_expression {
            tsne_expression = `identifier type_A;
            tsne_arguments = [`identifier var_x];
          }
         |> rope_of_ts_expression
         |> Rope.to_string);
      test_case'
        "rope_of_ts_expression - literal_expression"
        ({|{"foo": (12.5), "bar": (null as (undefined))}|})
        (`literal_expression (
             `object_literal [
                 "foo", `literal_expression (`numeric_literal 12.5);
                 "bar", `casted_expression (
                            `identifier "null",
                            `special `undefined
                          )
           ])
         |> rope_of_ts_expression
         |> Rope.to_string);
      test_case'
        "rope_of_ts_expression - await_expression"
        ("await " ^ var_x)
        (`await_expression (`identifier var_x)
          |> rope_of_ts_expression
          |> Rope.to_string);
      test_case'
        "rope_of_ts_expression - const_assertion"
        "( \"abc\" ) as const"
        (`const_assertion (`literal_expression (`string_literal "abc"))
          |> rope_of_ts_expression
          |> Rope.to_string);
      test_case'
        "rope_of_ts_expression - call_expression"
        ("((" ^ var_x ^ " : " ^ number ^ ", " ^ var_y ^ "? : " ^ number ^ ") => {" ^
          "return " ^ var_x ^ " " ^ plus ^ " " ^ var_y
          ^ "})" ^ "(" ^ var_a ^ ", " ^ var_b ^ ")")
        (`call_expression {
            tsce_expression = `arrow_function {
                tsaf_parameters = [
                  { tsp_name = var_x;
                    tsp_optional = false;
                    tsp_type_desc = `type_reference number; };
                  { tsp_name = var_y;
                    tsp_optional = true;
                    tsp_type_desc = `type_reference number; };
                ];
                tsaf_body = [
                  `return_statement
                    (`binary_expression {
                        tsbe_left = `identifier var_x;
                        tsbe_operator_token = plus;
                        tsbe_right = `identifier var_y;
                      })
                ];
              };
            tsce_arguments = [
              `identifier var_a;
              `identifier var_b;
            ];
          }
          |> rope_of_ts_expression
          |> Rope.to_string);
    ] in
    let type_desc_cases = [
      test_case'
        "rope_of_ts_type_desc - type_reference"
        number
        (`type_reference number
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - void"
        "void"
        (`special `void
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - type_assertion"
        "( void ) as ( null )"
        (`type_assertion (`special `void, `special `null)
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - type_construct"
        "foo<(null), (void)>"
        (`type_construct ("foo", [`special `null; `special `void])
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - typeof"
        "typeof foo"
        (`typeof (`identifier "foo")
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - keyof"
        "keyof bar"
        (`keyof (`type_reference "bar")
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - type_literal"
        ("{ " ^ var_x ^ " : " ^ number ^ " , " ^ var_y ^ " : " ^ number ^ " , " ^ var_z ^ "? :" ^ number ^ " }" )
        (`type_literal [
            { tsps_modifiers = [];
              tsps_name = var_x;
              tsps_optional = false;
              tsps_type_desc = `type_reference number; };
            { tsps_modifiers = [];
              tsps_name = var_y;
              tsps_optional = false;
              tsps_type_desc = `type_reference number; };
            { tsps_modifiers = [];
              tsps_name = var_z;
              tsps_optional = true;
              tsps_type_desc = `type_reference number; };
          ]
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - numeric_literal"
        "42."
        (`literal_type (`numeric_literal 42.)
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - string_literal"
        "\"foo\""
        (`literal_type (`string_literal "foo")
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - template_literal"
        "`${number}-${number}-${number}`"
        (`literal_type (`template_literal "${number}-${number}-${number}")
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - tuple"
        ("[" ^ string ^ ", " ^ number ^ "]")
        (`tuple [`type_reference string; `type_reference number;]
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - union 2"
        "\"foo\" | \"bar\""
        (`union [`literal_type (`string_literal "foo"); `literal_type (`string_literal "bar")]
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - union 3"
        "\"foo\" | \"bar\" | \"baz\""
        (`union [`literal_type (`string_literal "foo");
                 `literal_type (`string_literal "bar");
                 `literal_type (`string_literal "baz")]
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - union nested left"
        "(\"foo\" | \"bar\") | \"baz\""
        (`union [`union [`literal_type (`string_literal "foo");
                         `literal_type (`string_literal "bar")];
                 `literal_type (`string_literal "baz")]
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - union nested right"
        "\"foo\" | (\"bar\" | \"baz\")"
        (`union [`literal_type (`string_literal "foo");
                 `union [`literal_type (`string_literal "bar");
                         `literal_type (`string_literal "baz")]]
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - intersection 2"
        (type_A ^ " & " ^ type_B)
        (`intersection [`type_reference type_A; `type_reference type_B]
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - intersection 3"
        (type_A ^ " & " ^ type_B ^ " & " ^ type_C)
        (`intersection [`type_reference type_A;
                        `type_reference type_B;
                        `type_reference type_C]
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - intersection nested left"
        ("(" ^ type_A ^ " & " ^ type_B ^ ")" ^ " & " ^ type_C)
        (`intersection [`intersection [`type_reference type_A; `type_reference type_B;];
                        `type_reference type_C]
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - intersection nested right"
        (type_A ^ " & " ^ "(" ^ type_B ^ " & " ^ type_C ^ ")")
        (`intersection [`type_reference type_A;
                        `intersection [`type_reference type_B;
                                       `type_reference type_C]]
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - array"
        (number ^ "[]")
        (`array (`type_reference number)
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - func_type"
        ("(" ^ var_x ^ ": " ^ number ^ ", " ^ var_y ^ "?: " ^ number ^ ") => " ^ number)
        (`func_type {
            tsft_parameters = [
              { tsp_name = var_x;
                tsp_optional = false;
                tsp_type_desc = `type_reference number; };
              { tsp_name = var_y;
                tsp_optional = true;
                tsp_type_desc = `type_reference number; }
            ];
            tsft_type_desc = `type_reference number;
          }
         |> rope_of_ts_type_desc
         |> Rope.to_string);
      test_case'
        "rope_of_ts_type_desc - record"
        ("Record<" ^ string ^ ", " ^ number ^ ">")
        (`record (`type_reference string, `type_reference number)
         |> rope_of_ts_type_desc
         |> Rope.to_string);
    ] in
    let statement_case = [
      test_case'
        "rope_of_ts_statement - import"
        ("import { "^var_x^", "^var_y^" as "^var_a^" } from \""^libs^"\";")
        (`import {
            tsi_from = libs;
            tsi_import_items = [
              { tsii_name = var_x;
                tsii_alias = None };
              { tsii_name = var_y;
                tsii_alias = Some var_a };
            ];
          }
          |> rope_of_ts_statement
          |> Rope.to_string);
      test_case'
        "rope_of_ts_statement - type_alias_declaration"
        ("type " ^ type_A ^ " = { " ^ var_x ^ ": " ^ number ^ ", " ^ var_y ^ ": " ^ string ^ ", " ^ var_z ^ "?: " ^ string ^ " }")
        (`type_alias_declaration {
            tsa_modifiers = [];
            tsa_name = type_A;
            tsa_type_parameters = [];
            tsa_type_desc = `type_literal [
                { tsps_modifiers = [];
                  tsps_name = var_x;
                  tsps_optional = false;
                  tsps_type_desc = `type_reference number; };
                { tsps_modifiers = [];
                  tsps_name = var_y;
                  tsps_optional = false;
                  tsps_type_desc = `type_reference string; };
                { tsps_modifiers = [];
                  tsps_name = var_z;
                  tsps_optional = true;
                  tsps_type_desc = `type_reference string; };
              ];
          }
         |> rope_of_ts_statement
         |> Rope.to_string);
      test_case'
        "rope_of_ts_statement - value_declaration"
        ("export const foo = 12.5")
        (`value_declaration {
             tsv_modifiers = [ `export ];
             tsv_kind = `const;
             tsv_name = "foo";
             tsv_type_desc = None;
             tsv_value = `literal_expression (`numeric_literal 12.5)
          }
         |> rope_of_ts_statement
         |> Rope.to_string);
      test_case'
        "rope_of_ts_statement - value_declaration"
        ("export const foo: number = 12.5")
        (`value_declaration {
             tsv_modifiers = [ `export ];
             tsv_kind = `const;
             tsv_name = "foo";
             tsv_type_desc = Some (`type_reference "number");
             tsv_value = `literal_expression (`numeric_literal 12.5)
          }
         |> rope_of_ts_statement
         |> Rope.to_string);
      test_case'
        "rope_of_ts_statement - function_declaration"
        ("function " ^ var_f ^ "(" ^ var_x ^ ": " ^ number ^ ", " ^ var_y ^ "?: " ^ number ^  "): " ^ number ^ "{ " ^
         "return " ^ var_x ^ " " ^ plus ^ " " ^ var_y
         ^ " }")
        (`function_declaration {
            tsf_modifiers = [];
            tsf_name = var_f;
            tsf_type_parameters = [];
            tsf_parameters = [
              { tsp_name = var_x;
                tsp_optional = false;
                tsp_type_desc = `type_reference number; };
              { tsp_name = var_y;
                tsp_optional = true;
                tsp_type_desc = `type_reference number; };
            ];
            tsf_type_desc = `type_reference number;
            tsf_body = [
              `return_statement
                (`binary_expression {
                    tsbe_left = `identifier var_x;
                    tsbe_operator_token = plus;
                    tsbe_right = `identifier var_y;
                  })
            ];
          }
         |> rope_of_ts_statement
         |> Rope.to_string);
      test_case'
        "rope_of_ts_statement - function_declaration async"
        ("async function " ^ var_f ^ "<" ^ type_A ^ "> (" ^ var_x ^ ": " ^ type_A ^ "): " ^ type_A ^
         " { " ^ "return await " ^ var_x ^ " }")
        (`function_declaration {
            tsf_modifiers = [`async];
            tsf_name = var_f;
            tsf_type_parameters = [type_A];
            tsf_parameters = [
              { tsp_name = var_x;
                tsp_optional = false;
                tsp_type_desc = `type_reference type_A; }
            ];
            tsf_type_desc = `type_reference type_A;
            tsf_body = [
              `return_statement (`await_expression (`identifier var_x))
            ]
          }
         |> rope_of_ts_statement
         |> Rope.to_string);
      test_case'
        "rope_of_ts_statement - module_declaration with type_alias_declaration"
        ("namespace " ^ mod_A ^ "{ " ^ "type " ^ type_A ^ " = " ^ type_B ^ " }")
        (`module_declaration {
            tsm_modifiers = [];
            tsm_name = mod_A;
            tsm_body = [
              `type_alias_declaration {
                tsa_modifiers = [];
                tsa_name = type_A;
                tsa_type_parameters = [];
                tsa_type_desc = `type_reference type_B;
              }
            ];
          }
         |> rope_of_ts_statement
         |> Rope.to_string);
      test_case'
        "rope_of_ts_statement - return_statement"
        ("return " ^ var_x)
        (`return_statement (`identifier var_x)
         |> rope_of_ts_statement
         |> Rope.to_string);
      test_case'
        "rope_of_ts_statement - if_statement"
        ("if (" ^ var_x ^ " == " ^ var_a ^ ") { return " ^ var_x ^ " } else { return " ^  var_y ^ " }")
        (`if_statement
           (`binary_expression {
               tsbe_left = `identifier var_x;
               tsbe_operator_token = "==";
               tsbe_right = `identifier var_a;
             },
            `return_statement (`identifier var_x),
            `return_statement (`identifier var_y))
         |> rope_of_ts_statement
         |> Rope.to_string);
      test_case'
        "rope_of_ts_statement - throw_statement"
        "throw new Error(\"something wrong\")"
        (`throw_statement
           (`new_expression {
               tsne_expression = `identifier "Error";
               tsne_arguments = [`literal_expression (`string_literal "something wrong")];
             })
         |> rope_of_ts_statement
         |> Rope.to_string);
    ] in
    [("ex_expression", expression_cases);
     ("ex_type_desc", type_desc_cases);
     ("ex_statement", statement_case)]
end

let () =
  let open Alcotest in
  let open Kxclib in
  (all
   |&>> (fun (name, (module E : Ex)) ->
    E.example_descs |&> (fun ((module D : Ex_desc) as d) ->
      let name = sprintf "%s.%s" name D.decl.td_name in
      Ast.create_cases name d)))
  @ Code.cases
  |> run "lib_gen_ts"
