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
open Bindoj_gen_foreign.Foreign_datatype
open Bindoj_test_common

let testable_fwrt =
  let pp : (unit, unit, unit, unit*unit*unit) fwrt_decl Fmt.t =
    let pp_unit fmt () = Format.pp_print_string fmt "()" in
    pp_fwrt_decl pp_unit pp_unit pp_unit pp_unit pp_unit pp_unit
  in
  let equal : (unit, unit, unit, unit*unit*unit) fwrt_decl -> (unit, unit, unit, unit*unit*unit) fwrt_decl -> bool =
    let equal_unit () () = true in
    equal_fwrt_decl equal_unit equal_unit equal_unit equal_unit equal_unit equal_unit
  in
  Alcotest.testable pp equal

let fc_annot_to_unit desc : (unit, unit, unit, unit*unit*unit) fwrt_desc =
  { desc with
    fd_kind = match desc.fd_kind with
      | Fwrt_constructor c ->
        Fwrt_constructor { c with fc_annot = () }
      | Fwrt_alias a -> Fwrt_alias a
      | Fwrt_object o -> Fwrt_object o }

let create_cases doc (module Ex : Typedesc_examples.T) =
  let open Alcotest in
  let open Kxclib in
  let create_test () =
    Alcotest.check testable_fwrt doc
      (fwrt_decl_of_type_decl Ex.decl) (Ex.fwrt /> FwrtTypeEnv.map fc_annot_to_unit) in
  (doc, [test_case "fwrt_decl_of_type_decl works" `Quick create_test])

let () =
  let open Alcotest in
  Typedesc_examples.all
  |> List.map (fun (name, m) -> create_cases name m)
  |> run "lib_gen_foreign"
