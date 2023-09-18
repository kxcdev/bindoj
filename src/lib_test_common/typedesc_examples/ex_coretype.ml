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
open Bindoj_base.Type_desc
open Bindoj_gen_ts.Typescript_datatype
open Bindoj_openapi.V3

module Various_prim_types : Util.Ex_desc = struct
  let prims =
    [`unit; `bool; `int; `float; `string; `uchar; `byte; `bytes]
    |&> (fun p -> Coretype.string_of_prim p, Coretype.mk_prim p)

  let module_name = "Various_prim_types"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      record_decl "ex_coretype_various_prim_types" (prims |&> (!! record_field))
        ~doc:(doc "various primitive types")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    "ex_coretype_various_prim_types", Util.FwrtTypeEnv.(
      init
      |> bind_object "ex_coretype_various_prim_types" (prims |&> (!! field))
    )

  let json_name = "ExCoretypeVariousPrimTypes"

  let ts_ast : ts_ast option =
    let lit =
      `type_literal Util.Ts_ast.[
          property "unit" (`literal_type (`numeric_literal 1.));
          property "bool" (`type_reference "boolean");
          property "int" (`type_reference "number");
          property "float" (`type_reference "number");
          property "string" (`type_reference "string");
          property "uchar" (`type_reference "string");
          property "byte" (`type_reference "number");
          property "bytes" (`type_reference "string");
        ] in
    Some [
      `type_alias_declaration {
        tsa_modifiers = [`export];
        tsa_name = json_name;
        tsa_type_parameters = [];
        tsa_type_desc = lit
      } ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
            (json_name,
              (`object_of
                  [`mandatory_field
                    ("unit", (`special ("unit", (`exactly `null))));
                  `mandatory_field ("bool", `boolean);
                  `mandatory_field ("int", `integral);
                  `mandatory_field ("float", `proper_float);
                  `mandatory_field ("string", `string);
                  `mandatory_field ("uchar", (`special ("uchar", `string)));
                  `mandatory_field ("byte", (`special ("byte", `string)));
                  `mandatory_field ("bytes", `base64str)]))))
    )

  let schema_object : Schema_object.t option = None
end

module With_int53p : Util.Ex_desc = struct
  let cty_int53p = Coretype.mk_prim `int53p

  let module_name = "With_int53p"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      record_decl "ex_coretype_with_int53p" [
        record_field "value" cty_int53p ~doc:(doc "an int53p value");
      ] ~doc:(doc "record of an int53p value")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    "ex_coretype_with_int53p", Util.FwrtTypeEnv.(
        init
        |> bind_object "ex_coretype_with_int53p"
          [ field "value" cty_int53p; ]
      )

  let json_name = "ExCoretypeWithInt53p"

  let ts_ast : ts_ast option =
    Some [
      `type_alias_declaration {
          tsa_modifiers = [`export];
          tsa_name = json_name;
          tsa_type_parameters = [];
          tsa_type_desc =
            `type_literal [
                Util.Ts_ast.property "value" (`type_reference "number")
              ];};
    ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
            (json_name,
              (`object_of [`mandatory_field ("value", `proper_int53p)]))))
    )

  let schema_object : Schema_object.t option = None
end

open struct
  let tuple_configs : [`coretype] configs =
    let open Bindoj_codec.Json in
    [ Json_config.tuple_style (`obj `default) ]

  let cty_json = Bindoj_std.Coretypes.json
  let cty_int_opt = Coretype.(mk_option % prim) `int
  let cty_int_lst = Coretype.(mk_list % prim) `int
  let cty_int_tpl = Coretype.(mk_tuple % List.map prim) [ `int; `int ]
  let cty_int_obt = Coretype.(mk_tuple ~configs:tuple_configs % List.map prim) [ `int; `int ]
  let cty_int_map = Coretype.(mk_map `string % prim) `int

  let cty_nested = Coretype.mk_tuple [
    cty_int_opt.ct_desc;
    cty_int_lst.ct_desc;
    cty_int_tpl.ct_desc;
  ]
end

module Various_complex_types : Util.Ex_desc = struct
  let module_name = "Various_complex_types"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      record_decl "ex_coretype_various_complex_types" [
        record_field "option" cty_int_opt
          ~doc:(doc "int option");
        record_field "list" cty_int_lst
          ~doc:(doc "int list");
        record_field "map" cty_int_map
          ~doc:(doc "map<string, int>");
      ] ~doc:(doc "various complex types")

  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    "ex_coretype_various_complex_types", Util.FwrtTypeEnv.(
      init
      |> bind_object "ex_coretype_various_complex_types" [
        field "option" cty_int_opt;
        field "list" cty_int_lst;
        field "map" cty_int_map;
      ]
    )

  let json_name = "ExCoretypeVariousComplexTypes"

  let ts_ast : ts_ast option =
    Some [
      `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = json_name;
          tsa_type_parameters = [];
          tsa_type_desc =
            let ts_number = `type_reference "number" in
            `type_literal
              Util.Ts_ast.[
                property ~optional:true "option" ts_number;
                property "list" (`array ts_number);
                property "map" (`record (`type_reference "string", ts_number));
              ]
          }
    ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
              (json_name,
                (`object_of
                  [`optional_field ("option", `integral);
                  `mandatory_field ("list", (`array_of `integral));
                  `mandatory_field ("map", (`record_of `integral))]))))
    )

  let schema_object : Schema_object.t option = None
end

module Various_tuple_types : Util.Ex_desc = struct
  let module_name = "Various_tuple_types"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      record_decl "ex_coretype_various_tuple_types" [
        record_field "tuple" cty_int_tpl
          ~doc:(doc "(int * int)");

        record_field "objtuple" cty_int_obt
          ~doc:(doc "(int * int) (as object)");

        record_field "nested" cty_nested
          ~doc:(doc "(int option * int list * (int * int))");
      ] ~doc:(doc "collection of tuple types")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    "ex_coretype_various_tuple_types", Util.FwrtTypeEnv.(
      init
      |> bind_object "ex_coretype_various_tuple_types" [
        field "tuple" cty_int_tpl;
        field "objtuple" cty_int_obt;
        field "nested" cty_nested;
      ])

  let json_name = "ExCoretypeVariousTupleTypes"

  let ts_ast : ts_ast option =
    Some [
      `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = json_name;
          tsa_type_parameters = [];
          tsa_type_desc =
            let ts_number = `type_reference "number" in
            `type_literal
              Util.Ts_ast.[
                property "tuple" (`tuple [ ts_number; ts_number ]);
                property "objtuple" (`type_literal [
                  property "_0" ts_number;
                  property "_1" ts_number;
                ]);
                property "nested" (`tuple
                  [ `union [ts_number; `type_reference "null"; `type_reference "undefined" ];
                    `array ts_number;
                    `tuple [ ts_number; ts_number ];
                  ]);
              ]
          }
      ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
              (json_name,
                (`object_of
                  [`mandatory_field ("tuple", (`tuple_of [`integral; `integral]));
                  `mandatory_field
                    ("objtuple",
                      (`object_of
                          [`mandatory_field ("_0", `integral);
                          `mandatory_field ("_1", `integral)]));
                  `mandatory_field
                    ("nested",
                      (`tuple_of
                          [`nullable `integral;
                          `array_of `integral;
                          `tuple_of [`integral; `integral]]))]))))
    )

  let schema_object : Schema_object.t option = None
end

module Named_json : Util.Ex_desc = struct
  let module_name = "Named_json"

  let cty_string = Coretype.mk_prim `string

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      record_decl "ex_coretype_named_json" [
        record_field "name" cty_string ~doc:(doc "a name of datum");
        record_field "json" cty_json ~doc:(doc "a json datum");
      ] ~doc:(doc "record of name and json datum")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    "ex_coretype_named_json", Util.FwrtTypeEnv.(
        init
        |> bind_object "ex_coretype_named_json"
          [ field "name" cty_string;
            field "json" cty_json; ]
      )

  let json_name = "ExCoretypeNamedJson"

  let ts_ast : ts_ast option = Some [
      `type_alias_declaration {
          tsa_modifiers = [`export];
          tsa_name = json_name;
          tsa_type_parameters = [];
          tsa_type_desc =
            `type_literal Util.Ts_ast.[
                property "json" (`type_reference "json_value");
                property "name" (`type_reference "string");
              ];};
    ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
            (json_name,
              (`object_of
                [`mandatory_field ("name", `string);
                `mandatory_field
                  ("json",
                    (`named ("json_value", `any_json_value)))]))))
    )

  open Bindoj_openapi.V3

  let schema_object : Schema_object.t option = None
end

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex_coretype"

let example_descs : (module Util.Ex_desc) list = [
  (module Various_prim_types);
  (module With_int53p);
  (module Various_complex_types);
  (module Various_tuple_types);
  (module Named_json);
]
