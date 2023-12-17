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
open Bindoj_gen_config

open struct
  let cty_int = Coretype.mk_prim `int
  let cty_string = Coretype.mk_prim `string
end

let my_int_decl = alias_decl "my_int" cty_int
  ~doc:(`docstr "definition of my_int type")

let my_tuple_decl = alias_decl "my_tuple" (
  Coretype.mk_tuple [
    Coretype.prim `float;
    Coretype.prim `string;
  ] ~configs:[
    Json_config.tuple_style (`obj `default)
  ]) ~doc:(`docstr "definition of my_tuple type")

let student_decl = record_decl "student" [
  record_field "admission_year" cty_int
    ~doc:(`docstr "📅 addmission_year field");
  record_field "name" cty_string
    ~doc:( `docstr "📛 name field");
] ~doc:(`docstr "📝 definition of student type")

let person_decl = variant_decl "person" [
  variant_constructor "Anonymous" `no_param
    ~doc:(`docstr "Anonymous constructor");

  variant_constructor "With_id" (`tuple_like [variant_argument cty_int])
    ~doc:(`docstr "With_id constructor");

  variant_constructor "Student" (`inline_record [
    record_field "student_id" cty_int
      ~doc:(`docstr "student_id field in Student constructor");
    record_field "name" cty_string
      ~doc:(`docstr "name field in Student constructor");
  ]) ~doc:(`docstr "Student constructor");

  variant_constructor "Teacher" (`inline_record [
    record_field "faculty_id" cty_int
      ~doc:(`docstr "faculty_id field in Teacher constructor");
    record_field "name" cty_string
      ~doc:(`docstr "name field in Teacher constructor");
    record_field "department" cty_string
      ~doc:(`docstr "dapartment field in Teacher constructor");
  ]) ~doc:(`docstr "Teacher constructor");
] ~doc:(`docstr "definition of person type")

let decls : (string * type_decl) list = [
  "my_int_decl", my_int_decl;
  "my_tuple_decl", my_tuple_decl;
  "student_decl", student_decl;
  "person_decl", person_decl;
]
