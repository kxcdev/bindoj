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
  let cty_unit = Coretype.mk_prim `unit
  let cty_int_opt = Coretype.(mk_option % prim) `int

  let cty_obj_tpl = Coretype.mk_tuple [
    Coretype.prim `float;
    Coretype.prim `string;
  ] ~configs: [
    Json_config.tuple_style (`obj `default)
  ]
end

module Unit : Util.Ex_desc = struct
  let module_name = "Unit"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      alias_decl "ex_alias_unit" cty_unit
        ~doc:(doc "alias of unit type")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    "ex_alias_unit", Util.FwrtTypeEnv.(
      init
      |> bind_alias "ex_alias_unit" cty_unit
    )

  let ts_ast : ts_ast option = Some [
    `type_alias_declaration {
      tsa_modifiers = [`export];
      tsa_name = "ExAliasUnit";
      tsa_type_parameters = [];
      tsa_type_desc = `literal_type (`numeric_literal 1.);
    }
  ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named ("ExAliasUnit", (`special ("unit", (`exactly `null))))))
    )

  let schema_object : Schema_object.t option =
    Schema_object.(
      integer () ~schema
        ~title:"ExAliasUnit"
        ~id:"#ExAliasUnit"
        ~minimum:1 ~maximum:1)
    |> Option.some
end

module Int_opt : Util.Ex_desc = struct
  let module_name = "Int_opt"

  include Util.Make_ex_decls(struct
    let make_decl (module D : Util.With_docstr) =
      let open D in
      alias_decl "ex_alias_int_opt" cty_int_opt
        ~doc:(doc "alias of int option type")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    "ex_alias_int_opt", Util.FwrtTypeEnv.(
      init
      |> bind_alias "ex_alias_int_opt" cty_int_opt
    )

  let ts_ast : ts_ast option =
    Some [
      `type_alias_declaration {
        tsa_modifiers = [`export];
        tsa_name = "ExAliasIntOpt";
        tsa_type_parameters = [];
        tsa_type_desc = `union [ `type_reference "number"; `type_reference "null"; `type_reference "undefined"];
      }
    ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named ("ExAliasIntOpt", `nullable `integral)))
    )

  let schema_object : Schema_object.t option =
    Schema_object.(
      option (
        integer () ~schema
        ~title:"ExAliasIntOpt"
        ~id:"#ExAliasIntOpt"
      ))
    |> Option.some
end

module Objtuple : Util.Ex_desc = struct
  let module_name = "Objtuple"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      alias_decl "ex_alias_objtuple" cty_obj_tpl
        ~doc:(doc "alias of objtuple type")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    "ex_alias_objtuple", Util.FwrtTypeEnv.(
      init
      |> bind_alias "ex_alias_objtuple" cty_obj_tpl
    )

  let json_name = "ExAliasObjtuple"

  let ts_ast : ts_ast option = Some [
    `type_alias_declaration {
      tsa_modifiers = [`export];
      tsa_name = json_name;
      tsa_type_parameters = [];
      tsa_type_desc =
        `type_literal Util.Ts_ast.[
          property "_0" (`type_reference "number");
          property "_1" (`type_reference "string"); ]
    }
  ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
      ("not considering any config if exists",
        (`named
            (json_name,
              (`object_of
                [`mandatory_field ("_0", `proper_float);
                `mandatory_field ("_1", `string)]))))
    )

  let schema_object : Schema_object.t option =
    Schema_object.(record ~schema
      ~title:json_name
      ~id:("#"^json_name)
      [
        "_0", number ();
        "_1", string ();
      ])
    |> Option.some
end

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex_alias"

let example_descs : (module Util.Ex_desc) list = [
  (module Unit);
  (module Int_opt);
  (module Objtuple);
]
