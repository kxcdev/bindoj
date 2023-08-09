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
open Bindoj_typedesc
open Bindoj_apidir_shared
open Alcotest

let () =
  run "duplicated_registration_test.ml" [
    "duplicated_registration_test", [
      test_case "duplicated_registration_test" `Quick (fun () ->
        check_raises
          "duplicated registration exception is thrown"
          (Invalid_argument "duplicated registration of same type_decl named 'typ'")
          (fun () ->
            let typ1 = Coretypes.(Prims.int |> to_typed_type_decl "typ") in
            let typ2 = Coretypes.(Prims.float |> to_typed_type_decl "typ") in
            let module R = MakeRegistry() in
            R.register_type_decl_info typ1;
            R.register_type_decl_info typ2)
      )
    ]
  ]
