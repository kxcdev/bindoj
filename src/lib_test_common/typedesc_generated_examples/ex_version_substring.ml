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
open struct
  module Td_ex_version_substring = Bindoj_test_common_typedesc_examples.Ex_version_substring
end

include Bindoj_gen_test_gen_output.Ex_version_substring_gen

module Record = struct
  type t = ex_version_substring_record_v3_2_1 =
    { v5_3_version_info: [ `Case_version_v1  | `Case_v2_0_version  | `v3_0_1_case_version ];
      version_info_v2: int;
      version_info_v2_0: int;
      version_info_v2_0_1: int;

      version_v3_info: int;
      version_v3_0_info: int;
      version_v3_0_1_info: int;

      v4_version_info: int;
      v4_0_version_info: int;
      v4_0_1_version_info: int;

      no_preserving_v1_2_version: int;
  } [@@deriving show]


  let decl = Td_ex_version_substring.Record.decl
  let reflect = ex_version_substring_record_v3_2_1_reflect

  let json_shape_explanation = ex_version_substring_record_v3_2_1_json_shape_explanation
  let to_json = ex_version_substring_record_v3_2_1_to_json
  let of_json' = ex_version_substring_record_v3_2_1_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  open Util.Sample_value

  let sample_values =
    [ `Case_version_v1, "Case-version-v1";
      `Case_v2_0_version, "Case-v2_0-version";
      `v3_0_1_case_version, "v3_0_1-case-version" ]
    |> List.map @@ fun (v5_3_version_info, case_json) ->
      { orig = {
          v5_3_version_info;

          version_info_v2 = -1;
          version_info_v2_0 = -2;
          version_info_v2_0_1 = -3;

          version_v3_info = 1;
          version_v3_0_info = 2;
          version_v3_0_1_info = 3;

          v4_version_info = 4;
          v4_0_version_info = 5;
          v4_0_1_version_info = 6;

          no_preserving_v1_2_version = 0;
        };
      jv = `obj [
        ("v5_3VersionInfo", `str case_json);

        ("versionInfoV2", `num (-1.));
        ("versionInfoV2_0", `num (-2.));
        ("versionInfoV2_0_1", `num (-3.));

        ("versionV3Info", `num 1.);
        ("versionV3_0Info", `num 2.);
        ("versionV3_0_1Info", `num 3.);

        ("v4VersionInfo", `num 4.);
        ("v4_0VersionInfo", `num 5.);
        ("v4_0_1VersionInfo", `num 6.);

        ("noPreservingV12Version", `num 0.);
      ] }
end

module Variant = struct
  type t = ex_version_substring_variant_v1_0 =
    | Version_info_v1_0 of {
      version_info_v1: int;
      version_info_v1_0: int;
      version_info_v1_0_1: int }
    | Version_v1_0_info of {
      version_v1_info: int;
      version_v1_0_info: int;
      version_v1_0_1_info: int }
    | V1_0_version_info of {
      v1_version_info: int;
      v1_0_version_info: int;
      v1_0_1_version_info: int }
    | No_preserving_version_substring_v1_0 of {
      version_info_v1: int;
      version_info_v1_0: int;
      version_info_v1_0_1: int }
  [@@deriving show]

  let decl = Td_ex_version_substring.Variant.decl
  let reflect = ex_version_substring_variant_v1_0_reflect

  let json_discriminator_value = ex_version_substring_variant_v1_0_json_discriminator_value
  let json_shape_explanation = ex_version_substring_variant_v1_0_json_shape_explanation
  let to_json = ex_version_substring_variant_v1_0_to_json
  let of_json' = ex_version_substring_variant_v1_0_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  open Util.Sample_value
  open Util.Sample_value.JvHelper

  let sample_value01 = {
    orig = Version_info_v1_0 {
      version_info_v1 = 1;
      version_info_v1_0 = 2;
      version_info_v1_0_1 = 3;
    };
    jv = ctor_record "version-info-v1_0" [
      "versionInfoV1", `num 1.;
      "versionInfoV1_0", `num 2.;
      "versionInfoV1_0_1", `num 3.;
    ]
  }

  let sample_value02 = {
    orig = Version_v1_0_info {
      version_v1_info = 1;
      version_v1_0_info = 2;
      version_v1_0_1_info = 3;
    };
    jv = ctor_record "version-v1_0-info" [
      "versionV1Info", `num 1.;
      "versionV1_0Info", `num 2.;
      "versionV1_0_1Info", `num 3.;
    ]
  }

  let sample_value03 = {
    orig = V1_0_version_info {
      v1_version_info = 1;
      v1_0_version_info = 2;
      v1_0_1_version_info = 3;
    };
    jv = ctor_record "v1_0-version-info" [
      "v1VersionInfo", `num 1.;
      "v1_0VersionInfo", `num 2.;
      "v1_0_1VersionInfo", `num 3.;
    ]
  }

  let sample_value04 = {
    orig = No_preserving_version_substring_v1_0 {
      version_info_v1 = 1;
      version_info_v1_0 = 2;
      version_info_v1_0_1 = 3;
    };
    jv = ctor_record "no-preserving-version-substring-v1-0" [
      "versionInfoV1", `num 1.;
      "versionInfoV10", `num 2.;
      "versionInfoV101", `num 3.;
    ]
  }

  let sample_values = [
    sample_value01;
    sample_value02;
    sample_value03;
    sample_value04;
  ]

end

let env = empty_tdenv

let example_generated_descs : (module Util.Ex_generated_desc) list = [
  (module Record);
  (module Variant);
]
