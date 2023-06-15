(* Copyright 2022 Kotoi-Xie Consultancy, Inc. This file is a part of the

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
open Alcotest
open Bindoj_test_common
open Bindoj_gen
open Bindoj_openapi

module Testables : sig
  val schema_object : V3.Schema_object.t testable
end = struct
  let schema_object = testable V3.Schema_object.pp (=)
end

let create_test_cases (module Ex : Typedesc_examples.T) =
  match Ex.schema_object with
  | Some expected ->
    [ test_case "json schema" `Quick (fun () ->
        let actual = Json_codec.gen_json_schema Ex.decl in
        check' Testables.schema_object
          ~msg:"the generated schema is expected"
          ~expected
          ~actual
      )]
  | None ->
    [ test_case "(skipped)" `Quick ignore ]

let () =
  Typedesc_examples.all
  |&> ((?>) create_test_cases)
  |> Alcotest.run "lib_gen.json_schema"
