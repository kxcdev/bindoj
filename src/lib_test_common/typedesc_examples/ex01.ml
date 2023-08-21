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
open Bindoj_base.Type_desc
open Bindoj_gen_ts.Typescript_datatype

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex01"

let cty_int = Coretype.mk_prim `int
let cty_string = Coretype.mk_prim `string

let decl : type_decl =
  record_decl "student" [
    record_field "admission_year" cty_int;
    record_field "name" cty_string;
  ]

let decl_with_docstr : type_decl =
  record_decl "student" [
    record_field "admission_year" cty_int ~doc:(`docstr "addmission_year field");
    record_field "name" cty_string ~doc:( `docstr "name field");
  ] ~doc:(`docstr "definition of student type")

let fwrt : (unit, unit, unit) ts_fwrt_decl =
  "student", Util.FwrtTypeEnv.(
    init
    |> bind_object "student"
      [ field "admission_year" cty_int;
        field "name" cty_string; ]
  )

let ts_ast : ts_ast option =
  Some
    [ `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = "Student";
          tsa_type_parameters = [];
          tsa_type_desc =
            `type_literal
              Util.Ts_ast.[
                property "admissionYear" (`type_reference "number");
                property "name" (`type_reference "string") ]; } ]

let expected_json_shape_explanation =
  Some (
    `with_warning
     ("not considering any config if exists",
       (`named
          ("Student",
            (`object_of
               [`mandatory_field ("admissionYear", `integral);
               `mandatory_field ("name", `string)]))))
  )

open Bindoj_openapi.V3

let schema_object : Schema_object.t option =
  Some Schema_object.(
    record ~schema
      ~title:"Student"
      ~id:"#Student"
      [ "admissionYear", integer ();
        "name", string () ]
  )
