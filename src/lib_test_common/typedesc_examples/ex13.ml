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
open Bindoj_base.Type_desc
open Bindoj_gen_ts.Typescript_datatype

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex13"

let cty_student =
  Coretype.mk_ident
    "student"
    ~codec:(`open_ "Ex01_gen")

let decl : type_decl =
  record_decl "student_pair" [
    record_field "student1" cty_student;
    record_field "student2" cty_student;
  ]

let decl_with_docstr : type_decl =
  record_decl "student_pair" [
    record_field "student1" cty_student ~doc:(`docstr "student1 field");
    record_field "student2" cty_student ~doc:(`docstr "student2 field");
  ] ~doc:(`docstr "definition of student pair")

let fwrt : (unit, unit) ts_fwrt_decl =
  "student_pair", Util.FwrtTypeEnv.(
    snd Ex01.fwrt
    |> bind_object "student_pair"
      [ field "student1" cty_student;
        field "student2" cty_student; ]
  )

let ts_ast : ts_ast option =
  Some
    [ `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = "StudentPair";
          tsa_type_parameters = [];
          tsa_type_desc =
            `type_literal
              [ { tsps_modifiers = [];
                  tsps_name = "student1";
                  tsps_type_desc = `type_reference "Student"; };
                { tsps_modifiers = [];
                  tsps_name = "student2";
                  tsps_type_desc = `type_reference "Student"; }; ]; } ]

open Bindoj_openapi.V3

let schema_object : Schema_object.t option =
  Some Schema_object.(
    record ~schema
      ~title:"StudentPair"
      ~id:"#StudentPair"
      [ "student1", ref "#Student";
        "student2", ref "#Student"
      ]
  )
