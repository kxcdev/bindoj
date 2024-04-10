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
open Kxclib
open Bindoj_runtime
open struct
  module Td_ex_nested = Bindoj_test_common_typedesc_examples.Ex_nested
end

include Bindoj_gen_test_gen_output.Ex_nested_gen

module Point2 = struct
  type t = ex_nested_point2 = { x: float; y: float } [@@deriving show]
  let decl = Td_ex_nested.Point2.decl
  let reflect = ex_nested_point2_reflect

  let json_shape_explanation = ex_nested_point2_json_shape_explanation
  let to_json = ex_nested_point2_to_json
  let of_json' = ex_nested_point2_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  let sample_value01 : t Util.Sample_value.t = {
    orig = { x = 1.; y = 2. };
    jv = `obj [ ("x", `num 1.); ("y", `num 2.) ]
  }

  let sample_values = [
    sample_value01;
  ]
end

module Record = struct
  type t = ex_nested_record = {
    unit: Ex_alias.Unit.t;
    point2: Point2.t;
    point2_spread: Point2.t;
    person: Ex_mangling.Person_inherited.t;
    optional_variant: Ex_optional.Variant.t;
    person_spread: Ex_mangling.Person_inherited.t }
  [@@deriving show]

  let decl = Td_ex_nested.Record.decl
  let reflect = ex_nested_record_reflect

  let json_shape_explanation = ex_nested_record_json_shape_explanation
  let to_json = ex_nested_record_to_json
  let of_json' = ex_nested_record_of_json'

  let t : t Alcotest.testable = Alcotest.of_pp pp

  open struct
    let person_samples = Ex_mangling.Person_inherited.[
      Anonymous, [
        "kind", `str "Anonymous"
      ];
      With_id 1619, [
        ("kind", `str "With_id");
        ("value", `num 1619.);
      ];
      Student {
        student_id = 451;
        name = "Ray Bradbury";
        case_value = `Case_at0
      }, [
          ("kind", `str "student");
          ("student_id", `num 451.);
          ("name", `str "Ray Bradbury");
          ("caseValue", `str "Case_at0");
        ];
      Teacher {
        faculty_id = 2001;
        name = "Arthur C. Clark";
        department = "Space";
      }, [
        ("kind", `str "Teacher");
        ("facultyId", `num 2001.);
        ("name", `str "Arthur C. Clark");
        ("department", `str "Space");
      ]
    ]
  end

  let sample_values : t Util.Sample_value.t list =
    let point2 = { x = 1.; y = 2. } in
    let point2_fields= [ ("x", `num 1.); ("y", `num 2.) ] in
    person_samples |&>> (fun (person, person_fields) ->
      Ex_optional.Variant.sample_values
      |&> (fun optional_variant ->
        { Util.Sample_value.orig =
            { unit = ();
              point2;
              point2_spread = point2;
              person;
              optional_variant = optional_variant.orig;
              person_spread = person;
            };
          jv = `obj (
            [ ("unit", `num 1.); ("point2", `obj point2_fields) ]
            @ point2_fields
            @ [ ("person", `obj person_fields);
                ("optionalVariant", optional_variant.jv)
              ]
            @ person_fields)
        }))
end

module Variant = struct
  type t = ex_nested_variant =
    | Student1 of { student: Ex_record.Student.t }
    | Student2 of { student: Ex_record.Student.t }
    | Student3 of Ex_record.Student.t
    | Student4 of Ex_record.Student.t
    | Int_list1 of Ex_variant.Int_list.t
    | Int_list2 of Ex_variant.Int_list.t
  [@@deriving show]

  let decl = Td_ex_nested.Variant.decl
  let reflect = ex_nested_variant_reflect

  let json_shape_explanation = ex_nested_variant_json_shape_explanation
  let to_json = ex_nested_variant_to_json
  let of_json' = ex_nested_variant_of_json'

  let t : t Alcotest.testable = Alcotest.of_pp pp

  open Util.Sample_value
  open Util.Sample_value.JvHelper
  open struct
    let sample_student: Ex_record.Student.t =
      { admission_year = 1984;
        name = "William Gibson";
      }

    let sample_student_jv_fields =
      [ ("admissionYear", `num 1984.);
        ("name", `str "William Gibson")
      ]


    let intCons a  b = ctor2 "intcons" (`num (float_of_int a)) b
    let intNil = ctor0 "intnil"
  end

  let sample_value01 = {
    orig = Student1 { student = sample_student};
    jv = `obj [ ("tag", `str "student1"); ("student", `obj sample_student_jv_fields); ]
  }

  let sample_value02 = {
    orig = Student2 { student = sample_student};
    jv = `obj (("tag", `str "student2") :: sample_student_jv_fields)
  }

  let sample_value03 = {
    orig = Student3 sample_student;
    jv = `obj [ ("tag", `str "student3"); ("arg", `obj sample_student_jv_fields); ]
  }

  let sample_value04 = {
    orig = Student4 sample_student;
    jv = `obj (("tag", `str "student4") :: sample_student_jv_fields)
  }

  let sample_value05 = {
    orig = Int_list1 (IntCons (1, IntCons (2, IntNil)));
    jv = `obj [ ("tag", `str "int-list1"); ("arg", intCons 1 (intCons 2 intNil))]
  }

  let sample_value06 = {
    orig = Int_list2 (IntCons (1, IntCons (2, IntNil)));
    jv = `obj [ ("tag", `str "int-list2"); ("kind", `str "intcons"); ("value", `arr [ `num 1.; intCons 2 intNil ]); ]
  }

  let sample_values = [
    sample_value01;
    sample_value02;
    sample_value03;
    sample_value04;
    sample_value05;
    sample_value06;
  ]
end

let env =
  let open Bindoj_typedesc.Typed_type_desc in
  { Type_decl_environment.empty with
    alias_ident_typemap =
      Util.Env.to_alias_ident_typelist [
        (module Ex_alias.Unit);
        (module Ex_mangling.Person_inherited);
        (module Ex_record.Student);
        (module Ex_variant.Int_list);
        (module Point2);
        (module Ex_optional.Variant);
      ]
      @ (Ex_optional.env.alias_ident_typemap
         |> StringMap.to_list)
      |> StringMap.of_list}

let example_generated_descs : (module Util.Ex_generated_desc) list = [
  (module Point2);
  (module Record);
  (module Variant);
]
