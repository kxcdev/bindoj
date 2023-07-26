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
open Kxclib
open Bindoj_test_common.Of_json_error_examples
open Bindoj_runtime

module Testables : sig
  val json_shape_explanation : json_shape_explanation testable
  val jvpath : Json.jvpath testable
end = struct
  let json_shape_explanation = testable Json_shape.pp_shape_explanation ( = )

  let jvpath = testable Json.pp_jvpath ( = )
end

let create_test_cases (module S : SampleGenerated) =
  S.name, (S.samples |&> (fun (name, jv, (msg, path)) ->
    let msg =
      if List.empty path then sprintf "%s at root" msg
      else sprintf "%s at path %s" msg (path |> List.rev |> Json.unparse_jvpath)
    in
    test_case name `Quick(fun () ->
      let res_msg, res_path =
        S.of_json' jv
        |> function
        | Error err ->
          OfJsonResult.Err.(Some (to_string err), Some(path err))
        | _ -> None, None
      in
      check' (option string) ~msg:"error message" ~expected:(Some msg) ~actual:res_msg;
      check' (option Testables.jvpath) ~msg:"error jvpath" ~expected:(Some path) ~actual:res_path
    )) |> fun tests ->
      tests @ begin match S.expected_json_shape_explanation with
      | None -> [ test_case "json_shape_explanation(skipped)" `Quick ignore]
      | Some expected -> [
        test_case "json_shape_explanation" `Quick(fun () ->
          check' Testables.json_shape_explanation
            ~msg:"json_shape_explanation"
            ~expected
            ~actual:S.json_shape_explanation
        )]
      end)

let () =
  all_generated
  |&> create_test_cases
  |> Alcotest.run "lib_gen.of_json'"
