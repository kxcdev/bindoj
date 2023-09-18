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
open Bindoj_base
open struct
  module Td_ex_mangling = Bindoj_test_common_typedesc_examples.Ex_mangling
end

include Bindoj_gen_test_gen_output.Ex_mangling_gen

module Student_inherited = struct
  type t = ex_mangling_student_inherited = {
    admission_year: int;
    name: string;
    case_value: [ `Case_at0 | `case_at1 ]
  } [@@deriving show]
  let decl = Td_ex_mangling.Student_inherited.decl
  let reflect = ex_mangling_student_inherited_reflect

  let json_shape_explanation = ex_mangling_student_inherited_json_shape_explanation
  let to_json = ex_mangling_student_inherited_to_json
  let of_json' = ex_mangling_student_inherited_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  let sample_value01 : t Util.Sample_value.t = {
    orig = {
      admission_year = 1984;
      name = "William Gibson";
      case_value = `Case_at0
    };
    jv = `obj [
      ("admission_year", `num 1984.);
      ("name", `str "William Gibson");
      ("caseValue", `str "Case-at0");
    ];
  }

  let sample_value02 : t Util.Sample_value.t = {
    orig = {
      admission_year = 2001;
      name = "Arthur C. Clark";
      case_value = `case_at1
    };
    jv = `obj [
      ("admission_year", `num 2001.);
      ("name", `str "Arthur C. Clark");
      ("caseValue", `str "case_at1");
    ];
  }

  let sample_values = [
    sample_value01;
    sample_value02;
  ]
end

module Person_no_mangling = struct
  type t = ex_mangling_person_no_mangling =
    | Anonymous
    | With_id of int
    | Student of { student_id: int; name: string }
    | Teacher of { faculty_id: int; name: string; department: string }
    [@@deriving show]
  let decl = Td_ex_mangling.Person_no_mangling.decl
  let reflect = ex_mangling_person_no_mangling_reflect

  let json_discriminator_value = ex_mangling_person_no_mangling_json_discriminator_value
  let json_shape_explanation = ex_mangling_person_no_mangling_json_shape_explanation
  let to_json = ex_mangling_person_no_mangling_to_json
  let of_json' = ex_mangling_person_no_mangling_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  open Util.Sample_value
  open Util.Sample_value.JvHelper

  let sample_value01 = { orig = Anonymous; jv = ctor0 "Anonymous" }

  let sample_value02 = {
    orig = With_id 1619;
    jv = ctor1 "With_id" (`num 1619.);
  }

  let sample_value03 = {
    orig =
      Student {
        student_id = 451;
        name = "Ray Bradbury";
      };
    jv =
      ctor_record "Student" [
        ("student_id", `num 451.);
        ("name", `str "Ray Bradbury");
      ];
  }

  let sample_value04 = {
    orig =
      Teacher {
        faculty_id = 2001;
        name = "Arthur C. Clark";
        department = "Space";
      };
    jv =
      ctor_record "Teacher" [
        ("faculty_id", `num 2001.);
        ("name", `str "Arthur C. Clark");
        ("department", `str "Space");
      ];
  }

  let sample_values = [
    sample_value01;
    sample_value02;
    sample_value03;
    sample_value04;
  ]
end

module Person_inherited = struct
  type t = ex_mangling_person_inherited =
    | Anonymous
    | With_id of int
    | Student of { student_id: int; name: string; case_value: [ `Case_at0 | `case_at1 ] }
    | Teacher of { faculty_id: int; name: string; department: string }
    [@@deriving show]
  let decl = Td_ex_mangling.Person_inherited.decl
  let reflect = ex_mangling_person_inherited_reflect

  let json_discriminator_value = ex_mangling_person_inherited_json_discriminator_value
  let json_shape_explanation = ex_mangling_person_inherited_json_shape_explanation
  let to_json = ex_mangling_person_inherited_to_json
  let of_json' = ex_mangling_person_inherited_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  open Util.Sample_value
  open Util.Sample_value.JvHelper

  let sample_value01 = { orig = Anonymous; jv = ctor0 "Anonymous" }

  let sample_value02 = {
    orig = With_id 1619;
    jv = ctor1 "With_id" (`num 1619.);
  }

  let sample_value03 = {
    orig =
      Student {
        student_id = 451;
        name = "Ray Bradbury";
        case_value = `Case_at0
      };
    jv =
      ctor_record "student" [
        ("student_id", `num 451.);
        ("name", `str "Ray Bradbury");
        ("caseValue", `str "Case_at0")
      ];
  }

  let sample_value04 = {
    orig =
      Student {
        student_id = 451;
        name = "Ray Bradbury";
        case_value = `case_at1
      };
    jv =
      ctor_record "student" [
        ("student_id", `num 451.);
        ("name", `str "Ray Bradbury");
        ("caseValue", `str "case-at1")
      ];
  }

  let sample_value05 = {
    orig =
      Teacher {
        faculty_id = 2001;
        name = "Arthur C. Clark";
        department = "Space";
      };
    jv =
      ctor_record "Teacher" [
        ("facultyId", `num 2001.);
        ("name", `str "Arthur C. Clark");
        ("department", `str "Space");
      ];
  }

  let sample_values = [
    sample_value01;
    sample_value02;
    sample_value03;
    sample_value04;
    sample_value05;
  ]

end

module Enum = struct
  type t = [ `Case_at0 | `case_at1 | `Case_at2 | `Case_at3 ] [@@deriving show]

  let decl = Td_ex_mangling.Enum.decl
  let reflect = ex_mangling_enum_reflect

  let json_shape_explanation = ex_mangling_enum_json_shape_explanation
  let to_json = ex_mangling_enum_to_json
  let of_json' = ex_mangling_enum_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  let sample_value00 : t Util.Sample_value.t = {
    orig = `Case_at0; jv = `str "Case_at0"
  }

  let sample_value01 : t Util.Sample_value.t = {
    orig = `case_at1; jv = `str "case-at1"
  }

  let sample_value02 : t Util.Sample_value.t = {
    orig = `Case_at2; jv = `str "Case-at2"
  }

  let sample_value03 : t Util.Sample_value.t = {
    orig = `Case_at3; jv = `str "Case-third"
  }

  let sample_values = [
    sample_value00;
    sample_value01;
    sample_value02;
    sample_value03;
  ]

end

let env = empty_tdenv

let example_generated_descs : (module Util.Ex_generated_desc) list = [
  (module Student_inherited);
  (module Person_no_mangling);
  (module Person_inherited);
  (module Enum);
]
