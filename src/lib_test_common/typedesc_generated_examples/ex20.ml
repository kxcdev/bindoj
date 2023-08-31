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
include Bindoj_gen_test_gen_output.Ex20_gen
open Bindoj_base

type t = v3_2_1_preserving_version_info =
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


let decl = Bindoj_test_common_typedesc_examples.Ex20.decl
let reflect = v3_2_1_preserving_version_info_reflect

let json_shape_explanation = v3_2_1_preserving_version_info_json_shape_explanation
let to_json = v3_2_1_preserving_version_info_to_json
let of_json' = v3_2_1_preserving_version_info_of_json'
let env = empty_tdenv
let t : t Alcotest.testable = Alcotest.of_pp pp

open Sample_value

let sample_values =
  [ `Case_version_v1, "Case-version-v1";
    `Case_v2_0_version, "Case-v2_0-version";
    `v3_0_1_case_version, "v3_0_1-case-version" ]
  |> List.map @@ fun (v5_3_version_info, case_json) ->
    { orig = {
        version_info_v2 = -1;
        version_info_v2_0 = -2;
        version_info_v2_0_1 = -3;

        version_v3_info = 1;
        version_v3_0_info = 2;
        version_v3_0_1_info = 3;

        v4_version_info = 4;
        v4_0_version_info = 5;
        v4_0_1_version_info = 6;

        v5_3_version_info;
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
