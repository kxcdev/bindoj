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

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex19"

let cty_int = Coretype.mk_prim `int

let cty_enum = Coretype.(
  mk_string_enum [
    string_enum_case "Case_version_v1";
    string_enum_case "Case_v2_0_version";
    string_enum_case "v3_0_1_case_version";
  ])

open struct
  let make_decl with_doc : type_decl =
    let doc =
      if with_doc then (fun s -> `docstr s)
      else constant `nodoc
    in
    record_decl "v3_2_1_preserving_version_info" [
      record_field "v5_3_version_info" cty_enum
        ~doc:(doc "v5_3_version_info field");

      record_field "version_info_v2" cty_int
        ~doc:(doc "version_info_v2 field");
      record_field "version_info_v2_0" cty_int
        ~doc:(doc "version_info_v2_0 field");
      record_field "version_info_v2_0_1" cty_int
        ~doc:(doc "version_info_v2_0_1 field");

      record_field "version_v3_info" cty_int
        ~doc:(doc "version_v3_info field");
      record_field "version_v3_0_info" cty_int
        ~doc:(doc "version_v3_0_info field");
      record_field "version_v3_0_1_info" cty_int
        ~doc:(doc "version_v3_0_1_info field");

      record_field "v4_version_info" cty_int
        ~doc:(doc "v4_version_info field");
      record_field "v4_0_version_info" cty_int
        ~doc:(doc "v4_0_version_info field");
      record_field "v4_0_1_version_info" cty_int
        ~doc:(doc "v4_0_1_version_info field");

      record_field "no_preserving_v1_2_version" cty_int
        ~configs:[ Json_config.default_mangling_no_preserving_version_substring ];
    ]
end

let decl : type_decl = make_decl false
let decl_with_docstr : type_decl = make_decl true

let fwrt : (unit, unit, unit) ts_fwrt_decl =
  let name = "v3_2_1_preserving_version_info" in
  name, Util.FwrtTypeEnv.(
    init
    |> bind_object name [
      field "v5_3_version_info" cty_enum;

      field "version_info_v2" cty_int;
      field "version_info_v2_0" cty_int;
      field "version_info_v2_0_1" cty_int;

      field "version_v3_info" cty_int;
      field "version_v3_0_info" cty_int;
      field "version_v3_0_1_info" cty_int;

      field "v4_version_info" cty_int;
      field "v4_0_version_info" cty_int;
      field "v4_0_1_version_info" cty_int;

      field "no_preserving_v1_2_version" cty_int
        ~configs:[ Json_config.default_mangling_no_preserving_version_substring ];
    ]
  )

let ts_ast : ts_ast option =
  let int_field name = Util.Ts_ast.property name (`type_reference "number") in
  Some
    [ `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = "V3_2_1PreservingVersionInfo";
          tsa_type_parameters = [];
          tsa_type_desc =
            `type_literal
              Util.Ts_ast.[
                property "v5_3VersionInfo" (`union [
                  `literal_type (`string_literal "Case-version-v1");
                  `literal_type (`string_literal "Case-v2_0-version");
                  `literal_type (`string_literal "v3_0_1-case-version");
                ]);

                int_field "versionInfoV2";
                int_field "versionInfoV2_0";
                int_field "versionInfoV2_0_1";

                int_field "versionV3Info";
                int_field "versionV3_0Info";
                int_field "versionV3_0_1Info";

                int_field "v4VersionInfo";
                int_field "v4_0VersionInfo";
                int_field "v4_0_1VersionInfo";

                int_field "noPreservingV12Version";
              ] }]

let expected_json_shape_explanation =
  Some (
    `with_warning
     ("not considering any config if exists",
       (`named
          ("V3_2_1PreservingVersionInfo",
            (`object_of [
               `mandatory_field
                 ("v5_3VersionInfo",
                   (`string_enum
                     ["Case-version-v1";
                     "Case-v2_0-version";
                     "v3_0_1-case-version"]));
               `mandatory_field ("versionInfoV2", `integral);
               `mandatory_field ("versionInfoV2_0", `integral);
               `mandatory_field ("versionInfoV2_0_1", `integral);
               `mandatory_field ("versionV3Info", `integral);
               `mandatory_field ("versionV3_0Info", `integral);
               `mandatory_field ("versionV3_0_1Info", `integral);
               `mandatory_field ("v4VersionInfo", `integral);
               `mandatory_field ("v4_0VersionInfo", `integral);
               `mandatory_field ("v4_0_1VersionInfo", `integral);
               `mandatory_field ("noPreservingV12Version", `integral)]))))
  )

open Bindoj_openapi.V3

let schema_object : Schema_object.t option =
  Some Schema_object.(
    record ~schema
      ~title:"V3_2_1PreservingVersionInfo"
      ~id:"#V3_2_1PreservingVersionInfo"
      [ "v5_3VersionInfo", string () ~enum:[
          `str "Case-version-v1";
          `str "Case-v2_0-version";
          `str "v3_0_1-case-version";
        ];

        "versionInfoV2", integer ();
        "versionInfoV2_0", integer ();
        "versionInfoV2_0_1", integer ();

        "versionV3Info", integer ();
        "versionV3_0Info", integer ();
        "versionV3_0_1Info", integer ();

        "v4VersionInfo", integer ();
        "v4_0VersionInfo", integer ();
        "v4_0_1VersionInfo", integer ();

        "noPreservingV12Version", integer ();
      ]
  )
