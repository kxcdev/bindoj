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

open struct
  let make_decl with_doc : type_decl =
    let doc =
      if with_doc then
        (fun s -> `docstr s)
      else
        constant `nodoc
    in
    variant_decl "preserving_version_info_v1_0" [
      variant_constructor "Version_info_v1_0" (`inline_record [
        record_field "version_info_v1" cty_int
          ~doc:(doc "version_info_v1 field in Version_info_v1_0 constructor");
        record_field "version_info_v1_0" cty_int
          ~doc:(doc "version_info_v1_0 field in Version_info_v1_0 constructor");
        record_field "version_info_v1_0_1" cty_int
          ~doc:(doc "version_info_v1_0_1 field in Version_info_v1_0 constructor");
      ]) ~doc:(doc "Version_info_v1_0 constructor");
      variant_constructor "Version_v1_0_info" (`inline_record [
        record_field "version_v1_info" cty_int
          ~doc:(doc "version_v1_info field in Version_v1_0_info constructor");
        record_field "version_v1_0_info" cty_int
          ~doc:(doc "version_v1_0_info field in Version_v1_0_info constructor");
        record_field "version_v1_0_1_info" cty_int
          ~doc:(doc "version_v1_0_1_info field in Version_v1_0_info constructor");
      ]) ~doc:(doc "Version_v1_0_info constructor");
      variant_constructor "V1_0_version_info" (`inline_record [
        record_field "v1_version_info" cty_int
          ~doc:(doc "v1_version_info field in V1_0_version_info constructor");
        record_field "v1_0_version_info" cty_int
          ~doc:(doc "v1_0_version_info field in V1_0_version_info constructor");
        record_field "v1_0_1_version_info" cty_int
          ~doc:(doc "v1_0_1_version_info field in V1_0_version_info constructor");
      ]) ~doc:(doc "Version_v1_0_info constructor");
      variant_constructor "No_preserving_version_substring_v1_0" (`inline_record [
        record_field "version_info_v1" cty_int
          ~doc:(doc "version_info_v1 field in No_preserving_version_substring_constructor_v1_0");
        record_field "version_info_v1_0" cty_int
          ~doc:(doc "version_info_v1_0 field in No_preserving_version_substring_constructor_v1_0");
        record_field "version_info_v1_0_1" cty_int
          ~doc:(doc "version_info_v1_0_1 field in No_preserving_version_substring_constructor_v1_0");
      ]) ~configs:[ Json_config.default_mangling_no_preserving_version_substring ]
         ~doc:(doc "No_preserving_version_substring constructor");
    ] ~doc:(doc "definition of type version_info_v1_0")
end

let decl : type_decl = make_decl false
let decl_with_docstr : type_decl = make_decl true

let fwrt : (unit, unit, unit) ts_fwrt_decl =
  let parent = "preserving_version_info_v1_0" in
  parent, Util.FwrtTypeEnv.(
    init
    |> bind_object parent []
    |> bind_constructor ~parent "Version_info_v1_0" ~fields:[
      field "version_info_v1" cty_int;
      field "version_info_v1_0" cty_int;
      field "version_info_v1_0_1" cty_int;
    ]
    |> bind_constructor ~parent "Version_v1_0_info" ~fields:[
      field "version_v1_info" cty_int;
      field "version_v1_0_info" cty_int;
      field "version_v1_0_1_info" cty_int;
    ]
    |> bind_constructor ~parent "V1_0_version_info" ~fields:[
      field "v1_version_info" cty_int;
      field "v1_0_version_info" cty_int;
      field "v1_0_1_version_info" cty_int;
    ]
    |> bind_constructor ~parent "No_preserving_version_substring_v1_0" ~fields:[
      field "version_info_v1" cty_int;
      field "version_info_v1_0" cty_int;
      field "version_info_v1_0_1" cty_int;
    ] ~configs:[ Json_config.default_mangling_no_preserving_version_substring ]
  )

let ts_ast : ts_ast option =
  let discriminator = "kind" in
  let discriminator_value kind = Util.Ts_ast.property discriminator (`literal_type (`string_literal kind)) in
  let int_field name = Util.Ts_ast.property name (`type_reference "number") in
  let version_info_v1_0 =
    `type_literal [
      discriminator_value "version-info-v1_0";
      int_field "versionInfoV1";
      int_field "versionInfoV1_0";
      int_field "versionInfoV1_0_1";
    ]
  in
  let version_v1_0_info =
    `type_literal [
      discriminator_value "version-v1_0-info";
      int_field "versionV1_0_1Info";
      int_field "versionV1_0Info";
      int_field "versionV1Info";
    ]
  in
  let v1_0_version_info =
    `type_literal [
      discriminator_value "v1_0-version-info";
      int_field "v1_0_1VersionInfo";
      int_field "v1_0VersionInfo";
      int_field "v1VersionInfo";
    ]
  in
  let no_preserving_version_substring_v1_0 =
    `type_literal [
      discriminator_value "no-preserving-version-substring-v1-0";
      int_field "versionInfoV1";
      int_field "versionInfoV10";
      int_field "versionInfoV101";
    ]
  in
  let preserving_version_info_v1_0 = [
    "Version_v1_0_info", version_v1_0_info;
    "Version_info_v1_0", version_info_v1_0;
    "V1_0_version_info", v1_0_version_info;
    "No_preserving_version_substring_v1_0", no_preserving_version_substring_v1_0;
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
          tsa_name = "PreservingVersionInfoV1_0";
          tsa_type_parameters = [];
          tsa_type_desc = `union (List.map snd preserving_version_info_v1_0); };
      Util.Ts_ast.case_analyzer "PreservingVersionInfoV1_0" "analyzePreservingVersionInfoV1_0" options preserving_version_info_v1_0; ]

let expected_json_shape_explanation =
  Some (
    `with_warning
     ("not considering any config if exists",
       (`named
          ("PreservingVersionInfoV1_0",
            (`anyone_of
               [`object_of
                  [`mandatory_field
                     ("kind", (`exactly (`str "version-info-v1_0")));
                  `mandatory_field ("versionInfoV1", `integral);
                  `mandatory_field ("versionInfoV1_0", `integral);
                  `mandatory_field ("versionInfoV1_0_1", `integral)];
                `object_of
                  [`mandatory_field
                     ("kind", (`exactly (`str "version-v1_0-info")));
                  `mandatory_field ("versionV1Info", `integral);
                  `mandatory_field ("versionV1_0Info", `integral);
                  `mandatory_field ("versionV1_0_1Info", `integral)];
                `object_of
                  [`mandatory_field
                     ("kind", (`exactly (`str "v1_0-version-info")));
                  `mandatory_field ("v1VersionInfo", `integral);
                  `mandatory_field ("v1_0VersionInfo", `integral);
                  `mandatory_field ("v1_0_1VersionInfo", `integral)];
               `object_of
                 [`mandatory_field
                    ("kind",
                      (`exactly (`str "no-preserving-version-substring-v1-0")));
                 `mandatory_field ("versionInfoV1", `integral);
                 `mandatory_field ("versionInfoV10", `integral);
                 `mandatory_field ("versionInfoV101", `integral)]]))))
  )

open Bindoj_openapi.V3

let schema_object : Schema_object.t option =
  Util.Schema_object.variant "PreservingVersionInfoV1_0"
    Schema_object.[
      "version-info-v1_0", [
        "versionInfoV1", integer ();
        "versionInfoV1_0", integer ();
        "versionInfoV1_0_1", integer ();
      ];
      "version-v1_0-info", [
        "versionV1Info", integer ();
        "versionV1_0Info", integer ();
        "versionV1_0_1Info", integer ();
      ];
      "v1_0-version-info", [
        "v1VersionInfo", integer ();
        "v1_0VersionInfo", integer ();
        "v1_0_1VersionInfo", integer ();
      ];
      "no-preserving-version-substring-v1-0", [
        "versionInfoV1", integer ();
        "versionInfoV10", integer ();
        "versionInfoV101", integer ();
      ];
    ]
  |> Option.some
