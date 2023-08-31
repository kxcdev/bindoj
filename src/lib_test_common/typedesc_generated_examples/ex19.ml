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
include Bindoj_gen_test_gen_output.Ex19_gen
open Bindoj_base

type t = preserving_version_info_v1_0 =
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

let decl = Bindoj_test_common_typedesc_examples.Ex19.decl
let reflect = preserving_version_info_v1_0_reflect

let json_discriminator_value = preserving_version_info_v1_0_json_discriminator_value
let json_shape_explanation = preserving_version_info_v1_0_json_shape_explanation
let to_json = preserving_version_info_v1_0_to_json
let of_json' = preserving_version_info_v1_0_of_json'
let env = empty_tdenv
let t : t Alcotest.testable = Alcotest.of_pp pp

open Sample_value
open Sample_value.JvHelper

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
