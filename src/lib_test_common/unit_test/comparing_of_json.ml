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
open Kxclib_priv_test_lib.Json
open Bindoj_test_common.Of_json_error_examples
open Bindoj_runtime
open Bindoj_typedesc
open Bindoj_codec

let () =
  let that ?(count=200) name =
    QCheck2.Test.make ~name ~count
  in
  all_generated
  |&> (fun (module S : SampleGenerated) ->
    that S.name gen_jv ~print:Kxclib.Json.unparse
      (fun jv ->
        let interpreted : S.t OfJsonResult.t =
          let typed_decl = Typed_type_desc.Typed.mk S.decl S.reflect in
          Json.of_json' ~env:S.env typed_decl jv
        in
        let compiled : S.t OfJsonResult.t = S.of_json' jv in
        let result = interpreted = compiled in
        if not result then
          let print_result label = function
            | Ok x -> eprintf "%s [%s]: Ok %a\n" S.name label S.pp x
            | Error ((_, _, shape) as e) ->
              eprintf "%s [%s]: Error \"%s\" %a\n"
                S.name label (OfJsonResult.Err.to_string e)
                Json_shape.pp_shape_explanation shape
          in
          print_result "interpreted" interpreted;
          print_result "compiled   " compiled;
        ;
        result
      )
  )
  |> QCheck_base_runner.run_tests_main |> exit
