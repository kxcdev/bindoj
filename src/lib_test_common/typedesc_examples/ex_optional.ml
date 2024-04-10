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
open Bindoj_codec_config
open Bindoj_gen_ts.Typescript_datatype
open Bindoj_openapi.V3

open struct
  let cty_int_opt = Coretype.(mk_option % prim) `int

  let cty_tuple = Coretype.(mk_tuple [
      option @@ prim `int;
      option @@ prim `int ]
    ~configs:[ Json_config.tuple_style (`obj `default) ])
end

module Xy_opt : Util.Ex_desc = struct
  let module_name = "Xy_opt"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      record_decl "ex_optional_xy_opt" [
        record_field "x_opt" cty_int_opt ~doc:(doc "an optional int x");
        record_field "y_opt" cty_int_opt ~doc:(doc "an optional int y");
      ] ~doc:(doc "optional fields of record")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    "ex_optional_xy_opt", Util.FwrtTypeEnv.(
      init
      |> bind_object "ex_optional_xy_opt"
        [ field "x_opt" cty_int_opt;
          field "y_opt" cty_int_opt; ]
    )

  let json_name = "ExOptionalXyOpt"

  let ts_ast : ts_ast option =
    Some
      [ `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = json_name;
          tsa_type_parameters = [];
          tsa_type_desc =
            `type_literal
              Util.Ts_ast.[
                property ~optional:true "xOpt" (`type_reference "number");
                property ~optional:true "yOpt" (`type_reference "number");
              ]}]

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
            (json_name,
              (`object_of
                  [`optional_field ("xOpt", `integral);
                  `optional_field ("yOpt", `integral)]))))
    )

  let schema_object : Schema_object.t option = None
end

module Variant : Util.Ex_desc = struct
  let discriminator = "tag"
  let arg_fname = "arg"

  let variant_configs : [`type_decl] configs = [
    Json_config.variant_discriminator discriminator;
  ]

  let ctor_configs: [`variant_constructor] configs = [
    Json_config.name_of_variant_arg `arg
  ]

  let module_name = "Variant"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      variant_decl "ex_optional_variant" [
      variant_constructor "Tuple_like" (`tuple_like [
        variant_argument cty_int_opt
          ~doc:(doc "arguemnt in Tuple_like constructor");
      ]) ~configs:ctor_configs
        ~doc:(doc "Tuple_like constructor");
      variant_constructor "Tuple_like_alias" (`tuple_like [
        variant_argument_nested ~codec:(open_ "Ex_alias") (decl (module Ex_alias.Int_opt))
          ~doc:(doc "arguemnt in Tuple_like_alias constructor");
      ]) ~configs:ctor_configs
        ~doc:(doc "Tuple_like_alias constructor");
      variant_constructor "Tuple_like_obj" (`tuple_like [
        variant_argument cty_int_opt
          ~doc:(doc "arguemnt at 0 in Tuple_like_obj constructor");
        variant_argument_nested ~codec:(open_ "Ex_alias") (decl (module Ex_alias.Int_opt))
          ~doc:(doc "arguemnt at 1 in Tuple_like_obj constructor");
      ]) ~configs:(Json_config.tuple_style (`obj `default) :: ctor_configs)
        ~doc:(doc "Tuple_like_obj constructor");
      variant_constructor "Tuple_like_spreading" (`tuple_like [
        variant_argument_nested (decl (module Xy_opt))
          ~configs:[ Json_config.nested_field_style `spreading ];
      ]) ~configs:ctor_configs ~doc:(doc "tuple_like_spreading constructor");
      variant_constructor "Inline_record" (`inline_record [
        record_field_nested "int_opt" ~codec:(open_ "Ex_alias") (decl (module Ex_alias.Int_opt))
          ~doc:(doc "int_opt field in Inline_record constructor");
        record_field "x_opt" cty_int_opt
          ~doc:(doc "x_opt field in Inline_record constructor");
        record_field "y_opt" cty_int_opt
          ~doc:(doc "y_opt field in Inline_record constructor");
        record_field "objtuple" cty_tuple
          ~doc:(doc "objtuple field in Inline_record constructor");
      ]) ~configs:ctor_configs
        ~doc:(doc "Inline_record constructor");
      variant_constructor "Inline_record_spreading" (`inline_record [
        record_field_nested "int_opt" ~codec:(open_ "Ex_alias") (decl (module Ex_alias.Int_opt))
          ~doc:(doc "int_opt field in Inline_record_spreading constructor");
        record_field_nested "xy_opt" (decl (module Xy_opt))
          ~configs:[ Json_config.nested_field_style `spreading ]
          ~doc:(doc "xy_opt field in inline_record constructor");
      ]) ~configs:ctor_configs
        ~doc:(doc "Inline_record_spreading constructor");
      variant_constructor "Reused_inline_record" (`reused_inline_record (decl (module Xy_opt)))
        ~configs:(
          Ts_config.reused_variant_inline_record_style `intersection_type
          :: ctor_configs)
        ~doc:(doc "Reused_inline_record constructor");
    ] ~configs:variant_configs ~doc:(doc "definition of ex_optional_variant type")

  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    let int_opt, int_opt_env = Ex_alias.Int_opt.fwrt in
    let xy_opt, xy_opt_env = Xy_opt.fwrt in
    let ex_optional_variant = "ex_optional_variant" in
    ex_optional_variant, Util.FwrtTypeEnv.(
      init
      |> union int_opt_env
      |> union xy_opt_env

      |> bind_object ex_optional_variant ~configs:variant_configs []

      |> bind_constructor ~parent:ex_optional_variant "Tuple_like" ~args:[
          variant_argument cty_int_opt
        ] ~configs:ctor_configs
      |> bind_constructor ~parent:ex_optional_variant "Tuple_like_alias" ~args:[
          variant_argument_nested ~codec:(`open_ "Ex_alias_gen") int_opt
        ] ~configs:ctor_configs
      |> bind_constructor ~parent:ex_optional_variant "Tuple_like_obj" ~args:[
        variant_argument cty_int_opt;
        variant_argument_nested ~codec:(`open_ "Ex_alias_gen") int_opt;
      ] ~configs:(Json_config.tuple_style (`obj `default) :: ctor_configs)
      |> bind_constructor ~parent:ex_optional_variant "Tuple_like_spreading" ~args:[
          variant_argument_nested xy_opt
            ~configs:[ Json_config.nested_field_style `spreading ]
        ] ~configs:ctor_configs
      |> bind_constructor ~parent:ex_optional_variant "Inline_record" ~fields:[
          field_nested ~codec:(`open_ "Ex_alias_gen") "int_opt" int_opt;
          field "x_opt" cty_int_opt;
          field "y_opt" cty_int_opt;
          field "objtuple" cty_tuple;
        ] ~configs:ctor_configs
      |> bind_constructor ~parent:ex_optional_variant "Inline_record_spreading" ~fields:[
          field_nested ~codec:(`open_ "Ex_alias_gen") "int_opt" int_opt;
          field_nested "xy_opt" xy_opt
            ~configs:[ Json_config.nested_field_style `spreading ];
        ] ~configs:ctor_configs
      |> bind_constructor ~parent:ex_optional_variant "Reused_inline_record"
          ~annot_kc:(Some (Tfcki_reused_variant_inline_record Xy_opt.decl))
          ~fields:[
            field "x_opt" cty_int_opt;
            field "y_opt" cty_int_opt ]
          ~configs:(
            Ts_config.reused_variant_inline_record_style `intersection_type
            :: ctor_configs)
    )

  let json_name = "ExOptionalVariant"
  let ts_ast : ts_ast option =
    let record_json_name = "ExOptionalXyOpt" in
    let discriminator_value kind =
      Util.Ts_ast.property discriminator (`literal_type (`string_literal kind))
    in
    let property_int_opt name =
      Util.Ts_ast.property ~optional:true name (`type_reference "number")
    in
    let tuple_like =
      `type_literal [
        discriminator_value "tuple-like";
        property_int_opt arg_fname;
      ]
    in
    let tuple_like_alias =
      `type_literal [
        discriminator_value "tuple-like-alias";
        property_int_opt arg_fname;
      ]
    in
    let tuple_like_obj =
      `type_literal [
        discriminator_value "tuple-like-obj";
        property_int_opt "_0";
        property_int_opt "_1";
      ]
    in
    let tuple_like_spreading =
      `intersection [
        `type_literal [ discriminator_value "tuple-like-spreading" ];
        `type_reference record_json_name;
      ]
    in
    let inline_record =
      `type_literal [
        discriminator_value "inline-record";
        property_int_opt "intOpt";
        Util.Ts_ast.property "objtuple" (`type_literal [
          property_int_opt "_0";
          property_int_opt "_1";
        ]);
        property_int_opt "xOpt";
        property_int_opt "yOpt";
      ]
    in
    let inline_record_spreading =
      `intersection [
        `type_literal [
          discriminator_value "inline-record-spreading";
          property_int_opt "intOpt"; ];
        `type_reference record_json_name;
      ] in
    let reused_inline_record =
      `intersection [
        `type_literal [ discriminator_value "reused-inline-record" ];
        `type_reference record_json_name;
      ]
    in
    let optional_variant = [
      "Inline_record", inline_record;
      "Inline_record_spreading", inline_record_spreading;
      "Reused_inline_record", reused_inline_record;
      "Tuple_like", tuple_like;
      "Tuple_like_alias", tuple_like_alias;
      "Tuple_like_obj", tuple_like_obj;
      "Tuple_like_spreading", tuple_like_spreading;
      ] in
      let options : Util.Ts_ast.options =
      { discriminator;
        var_v = "__bindoj_v";
        var_x = "__bindoj_x";
        var_fns = "__bindoj_fns";
        ret = "__bindoj_ret" } in
    Some [
      `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = json_name;
          tsa_type_parameters = [];
          tsa_type_desc = `union (List.map snd optional_variant); };
      Util.Ts_ast.case_analyzer json_name ("analyze"^json_name) options optional_variant;
    ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
      ("not considering any config if exists",
        (`named
            (json_name,
              (`anyone_of
                [`object_of
                    [`mandatory_field ("tag", (`exactly (`str "tuple-like")));
                    `optional_field ("arg", `integral)];
                `object_of
                  [`mandatory_field
                      ("tag", (`exactly (`str "tuple-like-alias")));
                  `optional_field ("arg", `integral)];
                `object_of
                  [`mandatory_field
                      ("tag", (`exactly (`str "tuple-like-obj")));
                  `optional_field ("_0", `integral);
                  `optional_field ("_1", `integral)];
                `object_of
                  [`mandatory_field
                      ("tag", (`exactly (`str "tuple-like-spreading")));
                  `optional_field ("xOpt", `integral);
                  `optional_field ("yOpt", `integral)];
                `object_of
                  [`mandatory_field ("tag", (`exactly (`str "inline-record")));
                  `optional_field ("intOpt", `integral);
                  `optional_field ("xOpt", `integral);
                  `optional_field ("yOpt", `integral);
                  `mandatory_field
                    ("objtuple",
                      (`object_of
                          [`optional_field ("_0", `integral);
                          `optional_field ("_1", `integral)]))];
                `object_of
                  [`mandatory_field
                      ("tag", (`exactly (`str "inline-record-spreading")));
                  `optional_field ("intOpt", `integral);
                  `optional_field ("xOpt", `integral);
                  `optional_field ("yOpt", `integral)];
                `object_of
                  [`mandatory_field
                      ("tag", (`exactly (`str "reused-inline-record")));
                  `optional_field ("xOpt", `integral);
                  `optional_field ("yOpt", `integral)]]))))
    )

  open Bindoj_openapi.V3

  let schema_object : Schema_object.t option = 
    let open Schema_object in
    Util.Schema_object.variant json_name
      ~discriminator_fname:"tag"
      [ "tuple-like", [
          "arg", integer () ~nullable:true;
        ];
        "tuple-like-alias", [
          "arg", integer () ~nullable:true;
        ];
        "tuple-like-obj", [
          "_0", integer () ~nullable:true;
          "_1", integer () ~nullable:true;
        ];
        "tuple-like-spreading", [
          "xOpt", integer () ~nullable:true;
          "yOpt", integer () ~nullable:true;
        ];
        "inline-record", [
          "intOpt", integer () ~nullable:true;
          "xOpt", integer () ~nullable:true;
          "yOpt", integer () ~nullable:true;
          "objtuple", record [
            "_0", integer () ~nullable:true;
            "_1", integer () ~nullable:true;
          ];
        ];
        "inline-record-spreading", [
          "intOpt", integer () ~nullable:true;
          "xOpt", integer () ~nullable:true;
          "yOpt", integer () ~nullable:true;
        ];
        "reused-inline-record", [
          "xOpt", integer () ~nullable:true;
          "yOpt", integer () ~nullable:true;
        ]
    ]
    |> Option.some
end

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex_optional"

let example_descs : (module Util.Ex_desc) list = [
  (module Xy_opt);
  (module Variant);
]
