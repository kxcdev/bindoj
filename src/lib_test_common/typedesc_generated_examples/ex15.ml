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
include Bindoj_gen_test_gen_output.Ex15_gen
open Bindoj_base

type t = nested_variant =
  | Student1 of { student: Ex01.t }
  | Student2 of { student: Ex01.t }
  | Student3 of Ex01.t
  | Student4 of Ex01.t
  | Int_list1 of Ex03.t
  | Int_list2 of Ex03.t
[@@deriving show]

let decl = Bindoj_test_common_typedesc_examples.Ex15.decl
let reflect = nested_variant_reflect

let json_shape_explanation = nested_variant_json_shape_explanation
let to_json = nested_variant_to_json
let of_json' = nested_variant_of_json'

let env =
  let open Bindoj_typedesc.Typed_type_desc in
  { Type_decl_environment.empty with
    alias_ident_typemap =
      StringMap.of_list [
        "student",
        (Boxed (Typed.mk Ex01.decl Ex01.reflect));
        "int_list",
        (Boxed (Typed.mk Ex03.decl Ex03.reflect)); ] }

let t : t Alcotest.testable = Alcotest.of_pp pp

open struct
  let sample_student: Ex01.t =
    { admission_year = 1984;
      name = "William Gibson";
    }

  let sample_student_jv_fields =
    [ ("admissionYear", `num 1984.);
      ("name", `str "William Gibson")
    ]

  open Sample_value.JvHelper
  let intCons a  b = ctor2 "intcons" (`num (float_of_int a)) b
  let intNil = ctor0 "intnil"
end

let sample_value01 : t Sample_value.t = {
  orig = Student1 { student = sample_student};
  jv = `obj [ ("tag", `str "student1"); ("student", `obj sample_student_jv_fields); ]
}

let sample_value02 : t Sample_value.t = {
  orig = Student2 { student = sample_student};
  jv = `obj (("tag", `str "student2") :: sample_student_jv_fields)
}

let sample_value03 : t Sample_value.t = {
  orig = Student3 sample_student;
  jv = `obj [ ("tag", `str "student3"); ("value", `obj sample_student_jv_fields); ]
}

let sample_value04 : t Sample_value.t = {
  orig = Student4 sample_student;
  jv = `obj (("tag", `str "student4") :: sample_student_jv_fields)
}

let sample_value05 : t Sample_value.t = {
  orig = Int_list1 (IntCons (1, IntCons (2, IntNil)));
  jv = `obj [ ("tag", `str "int-list1"); ("value", intCons 1 (intCons 2 intNil))]
}

let sample_value06 : t Sample_value.t = {
  orig = Int_list2 (IntCons (1, IntCons (2, IntNil)));
  jv = `obj [ ("tag", `str "int-list2"); ("kind", `str "intcons"); ("arg", `arr [ `num 1.; intCons 2 intNil ]); ]
}

let sample_values = [
  sample_value01;
  sample_value02;
  sample_value03;
  sample_value04;
  sample_value05;
  sample_value06;
]
