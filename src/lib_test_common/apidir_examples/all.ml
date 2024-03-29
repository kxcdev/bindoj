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
module type MockServerBuilder = Utils.MockServerBuilder

module type T = sig
  include Bindoj_apidir_shared.RegistryInfo

  val tests : unit Alcotest.test_case list

  val build_mock_server : (module MockServerBuilder) -> unit
end

let all : (string * (module T)) list = [
  "sample_apidir_01", (module Sample_apidir_01);
  "sample_apidir_01_no_mangling", (module Sample_apidir_01_no_mangling);
  "sample_apidir_02", (module Sample_apidir_02);
  "sample_apidir_03", (module Sample_apidir_03);
  "sample_apidir_04", (module Sample_apidir_04);
  "sample_apidir_05", (module Sample_apidir_05);
  "sample_apidir_06", (module Sample_apidir_06);
]
