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
open Bindoj_codec.Json

module Record : Util.Ex_desc = struct
  let module_name = "Record"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      record_decl "ex_nested_multiply_record" [
        record_field_nested "nested_record" (decl (module Ex_nested.Record))
          ~codec:(open_ "Ex_nested")
          ~doc:(doc "nested_record field");
        record_field_nested "nested_record_spread" (decl (module Ex_nested.Record))
          ~codec:(open_ "Ex_nested")
          ~configs:[
            Json_config.nested_field_style `spreading;
          ]
          ~doc:(doc "spread nested_record field");
      ]
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    let nested1_record, nested1_record_env = Ex_nested.Record.fwrt in
    "ex_nested_multiply_record", Util.FwrtTypeEnv.(
      init
      |> union nested1_record_env
      |> bind_object "ex_nested_multiply_record" [
        field_nested ~codec:(`open_ "Ex_nested_gen") "nested_record" nested1_record;
        field_nested ~codec:(`open_ "Ex_nested_gen") "nested_record_spread" nested1_record
          ~configs:[
            Json_config.nested_field_style `spreading;
          ];
      ]
    )

  let json_name = "ExNestedMultiplyRecord"

  let ts_ast : ts_ast option =
    let ref_nested_record = `type_reference "ExNestedRecord" in
    Some [
      `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = json_name;
          tsa_type_parameters = [];
          tsa_type_desc = `intersection [
            `type_literal Util.Ts_ast.[
              property "nestedRecord" ref_nested_record
            ];
            ref_nested_record
          ] }
    ]

  let expected_json_shape_explanation = None
  let schema_object = None
end

module Variant : Util.Ex_desc = struct
  let module_name = "Variant"

  let discriminator = "label"

  let variant_configs : [`type_decl] configs = [
    Json_config.variant_discriminator discriminator;
  ]

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      variant_decl "ex_nested_multiply_variant" [
        variant_constructor "Nested_record" (`inline_record [
          record_field_nested "nested_record" (decl (module Ex_nested.Record))
            ~codec:(open_ "Ex_nested")
            ~doc:(doc "nested_record field");
          record_field_nested "nested_record_spread" (decl (module Ex_nested.Record))
            ~codec:(open_ "Ex_nested")
            ~configs:[
              Json_config.nested_field_style `spreading;
            ]
            ~doc:(doc "spread nested_record field");
        ]);

        variant_constructor "Nested_variant" (`inline_record [
          record_field_nested "nested_variant" (decl (module Ex_nested.Variant))
            ~codec:(open_ "Ex_nested")
            ~doc:(doc "nested_variant field");
          record_field_nested "nested_variant_spread" (decl (module Ex_nested.Variant))
            ~codec:(open_ "Ex_nested")
            ~configs:[
              Json_config.nested_field_style `spreading;
              Json_config.no_mangling;
            ]
            ~doc:(doc "spread nested_variant field");
        ]);
      ] ~configs:variant_configs
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    let nested1_record, nested1_record_env = Ex_nested.Record.fwrt in
    let nested1_variant, nested1_variant_env = Ex_nested.Variant.fwrt in
    let parent = "ex_nested_multiply_variant" in
    parent, Util.FwrtTypeEnv.(
      init
      |> union nested1_record_env
      |> union nested1_variant_env
      |> bind_object ~configs:variant_configs parent [ ]

      |> bind_constructor ~parent "Nested_record" ~fields:[
        field_nested ~codec:(`open_ "Ex_nested_gen") "nested_record" nested1_record;
        field_nested ~codec:(`open_ "Ex_nested_gen") "nested_record_spread" nested1_record
          ~configs:[
            Json_config.nested_field_style `spreading;
          ];
      ]
      |> bind_constructor ~parent "Nested_variant" ~fields:[
        field_nested ~codec:(`open_ "Ex_nested_gen") "nested_variant" nested1_variant;
        field_nested ~codec:(`open_ "Ex_nested_gen") "nested_variant_spread" nested1_variant
          ~configs:[
            Json_config.nested_field_style `spreading;
          ];
      ]
    )

  let json_name = "ExNestedMultiplyVariant"

  let ts_ast : ts_ast option =
    let discriminator_value kind =
      Util.Ts_ast.property discriminator (`literal_type (`string_literal kind))
    in
    let ref_nested_record = `type_reference "ExNestedRecord" in
    let ref_nested_variant = `type_reference "ExNestedVariant" in
    let nested_record =
      `intersection [
        `type_literal Util.Ts_ast.[
          discriminator_value "nested-record";
          property "nestedRecord" ref_nested_record
        ];
        ref_nested_record
      ]
    in
    let nested_variant =
      `intersection [
        `type_literal Util.Ts_ast.[
          discriminator_value "nested-variant";
          property "nestedVariant" ref_nested_variant
        ];
        ref_nested_variant
      ]
    in
    let ctors = [
      "Nested_record", nested_record;
      "Nested_variant", nested_variant;
    ] in
    let options : Util.Ts_ast.options =
      { discriminator;
        var_v = "__bindoj_v";
        var_x = "__bindoj_x";
        var_fns = "__bindoj_fns";
        ret = "__bindoj_ret" } in
    Some
      [ `type_alias_declaration
          { tsa_modifiers = [`export];
            tsa_name = json_name;
            tsa_type_parameters = [];
            tsa_type_desc = `union (List.map snd ctors); };
        Util.Ts_ast.case_analyzer json_name ("analyze"^json_name) options ctors; ]

  let expected_json_shape_explanation = None
  let schema_object = None
end

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex_nested_multiply"

let example_descs : (module Util.Ex_desc) list = [
  (module Record);
  (module Variant);
]
