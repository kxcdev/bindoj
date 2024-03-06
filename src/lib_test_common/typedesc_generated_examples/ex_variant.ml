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
  module Td_ex_variant = Bindoj_test_common_typedesc_examples.Ex_variant
end

include Bindoj_gen_test_gen_output.Ex_variant_gen

module Person = struct
  type t = ex_variant_person =
    | Anonymous
    | With_id of int
    | Student of { student_id: int; name: string }
    | Teacher of { faculty_id: int; name: string; department: string }
    [@@deriving show]
  let decl = Td_ex_variant.Person.decl
  let reflect = ex_variant_person_reflect

  let json_discriminator_value = ex_variant_person_json_discriminator_value
  let json_shape_explanation = ex_variant_person_json_shape_explanation
  let to_json = ex_variant_person_to_json
  let of_json' = ex_variant_person_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  open Util.Sample_value
  open Util.Sample_value.JvHelper

  let sample_value01 = { orig = Anonymous; jv = ctor0 "anonymous" }

  let sample_value02 = {
    orig = With_id 1619;
    jv = ctor1 "with-id" (`num 1619.);
  }

  let sample_value03 = {
    orig =
      Student {
        student_id = 451;
        name = "Ray Bradbury";
      };
    jv =
      ctor_record "student" [
        ("studentId", `num 451.);
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
      ctor_record "teacher" [
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
  ]
end

module Person_reused = struct
  type t = ex_variant_person_reused =
    | Anonymous
    | With_id of int
    | Student of { student_id: int; name: string }
    | Teacher of { faculty_id: int; name: string; department: string }
    [@@deriving show]
  let decl = Td_ex_variant.Person_reused.decl
  let reflect = ex_variant_person_reused_reflect

  let json_discriminator_value = ex_variant_person_reused_json_discriminator_value
  let json_shape_explanation = ex_variant_person_reused_json_shape_explanation
  let to_json = ex_variant_person_reused_to_json
  let of_json' = ex_variant_person_reused_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  open Util.Sample_value
  open Util.Sample_value.JvHelper

  let sample_value01 = { orig = Anonymous; jv = ctor0 "anonymous" }

  let sample_value02 = {
    orig = With_id 1619;
    jv = ctor1 "with-id" (`num 1619.);
  }

  let sample_value03 = {
    orig =
      Student {
        student_id = 451;
        name = "Ray Bradbury";
      };
    jv =
      ctor_record "student" [
        ("studentId", `num 451.);
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
      ctor_record "teacher" [
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
  ]
end

module Int_list = struct
  type t = ex_variant_int_list =
    | IntNil
    | IntCons of int * t
    [@@deriving show]
  let decl = Td_ex_variant.Int_list.decl
  let reflect = ex_variant_int_list_reflect

  let json_discriminator_value = ex_variant_int_list_json_discriminator_value
  let json_shape_explanation = ex_variant_int_list_json_shape_explanation
  let to_json = ex_variant_int_list_to_json
  let of_json' = ex_variant_int_list_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  open Util.Sample_value
  open Util.Sample_value.JvHelper

  let intCons a b = ctor2 "intcons" (`num (float_of_int a)) b
  let intNil = ctor0 "intnil"

  let sample_value01 = { orig = IntNil; jv = intNil }

  let sample_value02 = {
    orig = IntCons (1, IntCons (2, IntNil));
    jv = intCons 1 (intCons 2 intNil);
  }

  let sample_value03 = {
    orig = IntCons (1, IntCons (2, IntCons (3, IntCons (4, IntNil))));
    jv = intCons 1 (intCons 2 (intCons 3 (intCons 4 intNil)));
  }

  let sample_value04 = {
      orig = IntCons (26335605, IntCons (35460072, IntNil));
      jv = intCons 26335605 (intCons 35460072 intNil)
    }

  let sample_values = [
    sample_value01;
    sample_value02;
    sample_value03;
    sample_value04;
  ]

end

module Int_list_objtuple = struct
  type t = ex_variant_int_list_objtuple =
    | IntNil
    | IntCons of int * t
    [@@deriving show]
  let decl = Td_ex_variant.Int_list_objtuple.decl
  let reflect = ex_variant_int_list_objtuple_reflect

  let json_discriminator_value = ex_variant_int_list_objtuple_json_discriminator_value
  let json_shape_explanation = ex_variant_int_list_objtuple_json_shape_explanation
  let to_json = ex_variant_int_list_objtuple_to_json
  let of_json' = ex_variant_int_list_objtuple_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  open Util.Sample_value
  open Util.Sample_value.JvHelper

  let intCons a b =
    ctor_record "intcons" [
      "_0", `num (float_of_int a);
      "_1", b
    ]
  let intNil = ctor0 "intnil"

  let sample_value01 = { orig = IntNil; jv = intNil }

  let sample_value02 = {
    orig = IntCons (1, IntCons (2, IntNil));
    jv = intCons 1 (intCons 2 intNil);
  }

  let sample_value03 = {
    orig = IntCons (1, IntCons (2, IntCons (3, IntCons (4, IntNil))));
    jv = intCons 1 (intCons 2 (intCons 3 (intCons 4 intNil)));
  }

  let sample_value04 = {
      orig = IntCons (26335605, IntCons (35460072, IntNil));
      jv = intCons 26335605 (intCons 35460072 intNil)
    }

  let sample_values = [
    sample_value01;
    sample_value02;
    sample_value03;
    sample_value04;
  ]

end

module Polymorphic = struct
  type t = [ `Foo0  | `Foo1 of int  | `Foo2 of (int * int) | `Foo3 of (int * int) ] [@@deriving show]
  let decl = Td_ex_variant.Polymorphic.decl
  let reflect = ex_variant_foo_reflect

  let json_discriminator_value = ex_variant_foo_json_discriminator_value
  let json_shape_explanation = ex_variant_foo_json_shape_explanation
  let to_json = ex_variant_foo_to_json
  let of_json' = ex_variant_foo_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  open Util.Sample_value
  open Util.Sample_value.JvHelper

  let sample_value01 = {
    orig = `Foo0;
    jv = ctor0 "foo0";
  }

  let sample_value02 = {
    orig = `Foo1 1;
    jv = ctor1 "foo1" (`num 1.);
  }

  let sample_value03 = {
    orig = `Foo2 (1, 2);
    jv = ctor2 "foo2" (`num 1.) (`num 2.);
  }

  let sample_value04 = {
    orig = `Foo3 (1, 2);
    jv = ctor_record "foo3" [
      "field1", `num 1.;
      "field2", `num 2.;
    ];
  }

  let sample_values = [
    sample_value01;
    sample_value02;
    sample_value03;
    sample_value04;
  ]
end

module Customized_union = struct
  type t = ex_variant_customized_union =
    | Case_tuple_like_arg of int
    | Case_tuple_like_exactly of int
    | Case_tuple_like_kind_name of int
    | Case_tuple_like_kind_name_no_mangling of int
    | Case_tuple_like_kind_name_no_mangling_with_ctor_name of int
    | Case_inline_record of { x : int; y : int }
  [@@deriving show]
  let decl = Td_ex_variant.Customized_union.decl
  let reflect = ex_variant_customized_union_reflect

  let json_discriminator_value = ex_variant_customized_union_json_discriminator_value
  let json_shape_explanation = ex_variant_customized_union_json_shape_explanation
  let to_json = ex_variant_customized_union_to_json
  let of_json' = ex_variant_customized_union_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  open Util.Sample_value
  open Util.Sample_value.JvHelper

  let discriminator = "tag"

  let sample_value01 = {
    orig = Case_tuple_like_arg 42;
    jv = ctor1 ~discriminator ~arg:"arg" "case-tuple-like-arg'" (`num 42.);
  }
  let sample_value02 = {
    orig = Case_tuple_like_exactly 1024;
    jv = ctor1 ~discriminator ~arg:"Argument" "case-tuple-like-exactly'" (`num 1024.);
  }
  let sample_value03 = {
    orig = Case_tuple_like_kind_name 12345;
    jv = ctor1 ~discriminator ~arg:"case-tuple-like-kind-name'" "case-tuple-like-kind-name'" (`num 12345.);
  }
  let sample_value04 = {
    orig = Case_tuple_like_kind_name_no_mangling 654321;
    jv = ctor1 ~discriminator ~arg:"Case_tuple_like_kind_name_no_mangling" "case-tuple-like-kind-name-no-mangling" (`num 654321.);
  }
  let sample_value05 = {
    orig = Case_tuple_like_kind_name_no_mangling_with_ctor_name 256;
    jv = ctor1 ~discriminator ~arg:"case-tuple-like-kind-name-no-mangling-with-ctor-name" "case-tuple-like-kind-name-no-mangling-with-ctor-name" (`num 256.);
  }
  let sample_value06 = {
    orig = Case_inline_record { x = 4; y = 2 };
    jv = ctor_record ~discriminator "case-inline-record'" [
      "x'", `num 4.;
      "y'", `num 2.;
    ]
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

let env = empty_tdenv

let example_generated_descs : (module Util.Ex_generated_desc) list = [
  (module Person);
  (module Person_reused);
  (module Int_list);
  (module Int_list_objtuple);
  (module Polymorphic);
  (module Customized_union);
]
