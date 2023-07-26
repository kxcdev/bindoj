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
open Kxclib
open Bindoj_base
open Bindoj_runtime
open Bindoj_typedesc
open Typedesc_generated_examples
open Typedesc_generated_examples.Sample_value.JvHelper

module type Sample = sig
  type t
  val decl : Type_desc.type_decl
  val reflect : t Runtime.Refl.t
  val env : Typed_type_desc.Type_decl_environment.env
  val name : string
  val samples : (string * Json.jv * (string * Json.jvpath)) list
  val expected_json_shape_explanation : json_shape_explanation option
end

module type SampleGenerated = sig
  include Sample
  val of_json' : ?path:Json.jvpath -> Json.jv -> t OfJsonResult.t
  val pp : ppf -> t -> unit
  val json_shape_explanation : json_shape_explanation
end

open struct
  let not_obj label = sprintf "an object is expected for a %s value, but the given is of type '%s'" label
  let record_not_obj = not_obj "record"
  let tuple_not_obj = not_obj "tuple"
  let discriminator_not_found = sprintf "discriminator field '%s' does not exist"
  let discriminator_not_string = sprintf "a string is expected for a variant discriminator, but the given is of type '%s'"
  let field_not_found = sprintf "mandatory field '%s' does not exist"
  let not_integer = sprintf "expecting an integer but the given is '%f'"
  let type_mismatch = sprintf "expecting type '%s' but the given is of type '%s'"
  let incorrect_list_length = sprintf "expecting an array of length %d, but the given has a length of %d"
  let incorrect_tuple_length = sprintf "expecting a tuple of length %d, but the given has a length of %d"
end

module SampleEx01 : SampleGenerated = struct
  include Ex01
  let name = "SampleEx01"
  let samples = [
    "not obj", `null, (record_not_obj "null", []);

    "missing field", `obj [ ("admissionYear", `num 1984.) ]
    , (field_not_found "name", []);

    "type mismatch", `obj [ ("admissionYear", `null); ("name", `str "William Gibson") ]
    , (type_mismatch "int" "null", [ `f "admissionYear" ]);

    "not integer", `obj [ ("admissionYear", `num 1984.5); ("name", `str "William Gibson") ]
    , (not_integer 1984.5, [ `f "admissionYear" ]);
  ]
  let expected_json_shape_explanation = Typedesc_examples.Ex01.expected_json_shape_explanation
end

module SampleEx01_inherited_mangling : SampleGenerated = struct
  include Ex01_inherited_mangling
  let name = "SampleEx01_inherited_mangling"
  let expected_json_shape_explanation = Typedesc_examples.Ex01_inherited_mangling.expected_json_shape_explanation

  let samples = [
    "not obj", `null, (record_not_obj "null", []);

    "missing field", `obj [ ("admission_year", `num 1984.) ]
    , (field_not_found "name", []);

    "type mismatch", `obj [ ("admission_year", `null); ("name", `str "William Gibson") ]
    , (type_mismatch "int" "null", [ `f "admission_year" ]);

    "not integer", `obj [ ("admission_year", `num 1984.5); ("name", `str "William Gibson") ]
    , (not_integer 1984.5, [ `f "admission_year" ]);

    "student: missing field", ctor_record "student" [ ("admission_year", `num 1984.); ("name", `str "William Gibson")],
    (field_not_found "caseValue", [ ]);

    "not one of", ctor_record "student" [ ("admission_year", `num 1984.); ("name", `str "William Gibson"); ("caseValue", `str "Case_at0")],
    ("given string 'Case_at0' is not one of [ 'Case-at0', 'case_at1' ]", [ `f "caseValue" ]);
  ]
end

module SampleEx02 : SampleGenerated = struct
  include Ex02
  let name = "SampleEx02"
  let expected_json_shape_explanation = Typedesc_examples.Ex02.expected_json_shape_explanation

  let samples = [
    "missing discriminator", `obj [ ],
    (discriminator_not_found "kind", []);

    "discriminator not string", `obj [ ("kind", `null)],
    (discriminator_not_string "null", [ `f "kind" ]);

    "invalid constructor", ctor0 "FooBar",
    ("given discriminator field value 'FooBar' is not one of [ 'anonymous', 'with-id', 'student', 'teacher' ]", [ `f "kind" ]);

    "with-id: missing field", ctor0 "with-id",
    (field_not_found "arg", []);

    "with-id: not integer", ctor_record "with-id" [ ("arg", `num 1.2) ],
    (not_integer 1.2, [ `f "arg" ]);

    "student: inline record", ctor0 "student",
    (field_not_found "studentId", []);

    "teacher: missing field", ctor_record "teacher" [ ("facultyId", `num 2001.) ],
    (field_not_found "name", []);

    "teacher: type mismatch", ctor_record "teacher" [ ("facultyId", `num 2001.); ("name", `str "Arthur C. Clark"); ("department", `arr []); ],
    (type_mismatch "string" "array", [ `f "department" ]);
  ]
end

module SampleEx02_no_mangling : SampleGenerated = struct
  include Ex02_no_mangling
  let name = "SampleEx02_no_mangling"
  let expected_json_shape_explanation = Typedesc_examples.Ex02_no_mangling.expected_json_shape_explanation
  let samples = [
    "missing discriminator", `obj [ ],
    (discriminator_not_found "kind", []);

    "discriminator not string", `obj [ ("kind", `null)],
    (discriminator_not_string "null", [ `f "kind" ]);

    "invalid constructor", ctor0 "FooBar",
    ("given discriminator field value 'FooBar' is not one of [ 'Anonymous', 'With_id', 'Student', 'Teacher' ]", [ `f "kind" ]);

    "With_id: missing field", ctor0 "With_id",
    (field_not_found "arg", []);

    "With_id: not integer", ctor_record "With_id" [ ("arg", `num 1.2) ],
    (not_integer 1.2, [ `f "arg" ]);

    "Student: inline record", ctor0 "Student",
    (field_not_found "student_id", []);

    "Teacher: missing field", ctor_record "Teacher" [ ("faculty_id", `num 2001.) ],
    (field_not_found "name", []);

    "Teacher: type mismatch", ctor_record "Teacher" [ ("faculty_id", `num 2001.); ("name", `str "Arthur C. Clark"); ("department", `arr []); ],
    (type_mismatch "string" "array", [ `f "department" ]);
  ]
end

module SampleEx02_inherited_mangling : SampleGenerated = struct
  include Ex02_inherited_mangling
  let name = "SampleEx02_inherited_mangling"
  let expected_json_shape_explanation = Typedesc_examples.Ex02_inherited_mangling.expected_json_shape_explanation

  let samples = [
    "missing discriminator", `obj [ ],
    (discriminator_not_found "kind", []);

    "discriminator not string", `obj [ ("kind", `null)],
    (discriminator_not_string "null", [ `f "kind" ]);

    "invalid constructor", ctor0 "FooBar",
    ("given discriminator field value 'FooBar' is not one of [ 'Anonymous', 'With_id', 'student', 'Teacher' ]", [ `f "kind" ]);

    "With_id: missing field", ctor0 "With_id",
    (field_not_found "arg", []);

    "With_id: not integer", ctor_record "With_id" [ ("arg", `num 1.2) ],
    (not_integer 1.2, [ `f "arg" ]);

    "student: inline record", ctor0 "student",
    (field_not_found "student_id", []);

    "student: missing field", ctor_record "student" [ ("student_id", `num 2.)],
    (field_not_found "name", [ ]);

    "student: missing field", ctor_record "student" [ ("student_id", `num 2.); ("name", `str "foobar")],
    (field_not_found "caseValue", [ ]);

    "student: not one of", ctor_record "student" [ ("student_id", `num 2.); ("name", `str "foobar"); ("caseValue", `str "Case-at0")],
    ("given string 'Case-at0' is not one of [ 'Case_at0', 'case-at1' ]", [ `f "caseValue" ]);

    "Teacher: missing field", ctor_record "Teacher" [ ("faculty_id", `num 2001.) ],
    (field_not_found "facultyId", []);
  ]
end

module SampleEx03 : SampleGenerated = struct
  include Ex03
  let name = "SampleEx03"
  let expected_json_shape_explanation = Typedesc_examples.Ex03.expected_json_shape_explanation

  let intCons_null = ctor2 "intcons" `null
  let samples = [
    "missing discriminator", `obj [ ],
    (discriminator_not_found "kind", []);

    "discriminator not string", `obj [ ("kind", `null)],
    (discriminator_not_string "null", [ `f "kind" ]);

    "missing discriminator (nested 1)", `obj [ ] |> intCons 1 ,
    (discriminator_not_found "kind", [ `i 1; `f "arg" ]);

    "missing discriminator (nested 2)", `obj [ ] |> intCons 1 |> intCons 2,
    (discriminator_not_found "kind", [ `i 1; `f "arg"; `i 1; `f "arg" ]);

    "missing field", ctor0 "intcons",
    (field_not_found "arg", []);

    "missing field (nested 1)", ctor0 "intcons" |> intCons 1 ,
    (field_not_found "arg", [ `i 1; `f "arg" ]);

    "missing field (nested 2)", ctor0 "intcons" |> intCons 1 |> intCons 2,
    (field_not_found "arg", [ `i 1; `f "arg"; `i 1; `f "arg" ]);

    "type mismatch", intCons_null intNil,
    (type_mismatch "int" "null" , [ `i 0; `f "arg" ]);

    "type mismatch (nested 1)", intCons_null intNil |> intCons 1 ,
    (type_mismatch "int" "null", [ `i 0; `f "arg"; `i 1; `f "arg" ]);

    "type mismatch (nested 2)", intCons_null intNil |> intCons 1 |> intCons 2,
    (type_mismatch "int" "null", [ `i 0; `f "arg"; `i 1; `f "arg"; `i 1; `f "arg" ]);
  ]
end

module SampleEx04 : SampleGenerated = struct
  include Ex04
  let name = "SampleEx04"
  let expected_json_shape_explanation = Typedesc_examples.Ex04.expected_json_shape_explanation

  let samples = [
    "foo2: incorrect list length", ctorN "foo2" [ `num 1. ],
    (incorrect_list_length 2 1, [ `f "arg" ])
  ]
end

module SampleEx05 : SampleGenerated = struct
  include Ex05
  let name = "SampleEx05"
  let expected_json_shape_explanation = Typedesc_examples.Ex05.expected_json_shape_explanation

  let samples = [
    "invalid tuple",
    `obj [
      "option", `num 42.;
      "list", `arr [ ];
      "tuple", `null;
    ],
    ("an array is expected for a tuple value, but the given is of type 'null'", [ `f "tuple" ]);

    "invalid type for tuple (nested)",
    `obj [
      "option", `num 42.;
      "list", `arr [ ];
      "tuple", `arr [ `num 4.; `num 2. ];
      "objtuple", `arr [ `num 4.; `num 2. ]
    ],
    (tuple_not_obj "array", [ `f "objtuple" ]);

    "incorrect list length in nested field",
    `obj [
      "option", `num 42.;
      "list", `arr [ ];
      "tuple", `arr [ `num 4.; `num 2. ];
      "objtuple", `obj ["_0", `num 4.; "_1", `num 2.];
      "nested", `arr [ `num 42.; `arr [ ]; `arr [`num 4.; `num 2.; `null]; ];
      "map", `obj [ ];
    ],
    (incorrect_tuple_length 2 3, [ `i 2; `f "nested" ]);
  ]
end

module SampleEx06 : SampleGenerated = struct
  include Ex06
  let name = "SampleEx06"
  let expected_json_shape_explanation = Typedesc_examples.Ex06.expected_json_shape_explanation

  let samples = [
    "invalid uchar",
    `obj [
      "unit", `num 1.;
      "bool", `bool true;
      "int", `num 42.;
      "float", `num 4.2;
      "string", `str "foo";
      "uchar", `str "Hello!";
    ],
    ("string 'Hello!' is not a valid uchar value", [ `f "uchar" ]);

    "invalid byte",
    `obj [
      "unit", `num 1.;
      "bool", `bool true;
      "int", `num 42.;
      "float", `num 4.2;
      "string", `str "foo";
      "uchar", `str "s";
      "byte", `num 512.;
    ],
    ("number '512' is not a valid byte value", [ `f "byte" ]);
  ]
end

module SampleEx07 : SampleGenerated = struct
  include Ex07
  let name = "SampleEx07"
  let expected_json_shape_explanation = Typedesc_examples.Ex07.expected_json_shape_explanation

  let samples = [
    "missing discriminator", `obj [ ],
    (discriminator_not_found "tag", []);

    "discriminator not string", `obj [ ("tag", `null)],
    (discriminator_not_string "null", [ `f "tag" ]);

    "missing field", ctor0 ~discriminator:"tag" "case1'",
    (field_not_found "value", []);

    "Case1: not integer", ctor_record ~discriminator:"tag" "case1'" [ ("value", `num 1.2) ],
    (not_integer 1.2, [ `f "value" ]);

    "missing field", ctor0 ~discriminator:"tag" "case2'",
    (field_not_found "x'", []);

    "missing field", ctor_record ~discriminator:"tag" "case2'" [ "x'", `num 4. ],
    (field_not_found "y'", [ ]);

    "Case2: not integer", ctor_record ~discriminator:"tag" "case2'" [ "x'", `str "fooBar"; "y'", `num 2. ],
    (type_mismatch "int" "string", [ `f "x'" ]);

    "Case2: not integer", ctor_record ~discriminator:"tag" "case2'" [ "x'", `num 4.; "y'", `num 2.1 ],
    (not_integer 2.1, [ `f "y'" ]);
  ]
end

module SampleEx08 : SampleGenerated = struct
  include Ex08
  let name = "SampleEx08"
  let expected_json_shape_explanation = Typedesc_examples.Ex08.expected_json_shape_explanation
  let samples = []
end

module SampleEx09 : SampleGenerated = struct
  include Ex09
  let name = "SampleEx09"
  let expected_json_shape_explanation = Typedesc_examples.Ex09.expected_json_shape_explanation

  let samples = [
    "missing field", `obj [],
    (field_not_found "value", []);

    "missing field", `obj [ "value", `null ],
    (type_mismatch "int53p" "null", [ `f "value" ]);
  ]
end

module SampleEx10 : SampleGenerated = struct
  include Ex10
  let name = "SampleEx10"
  let expected_json_shape_explanation = Typedesc_examples.Ex10.expected_json_shape_explanation

  let samples = [
    "not obj", `null, (record_not_obj "null", []);

    "type mismatch", `obj [ "yOpt", `str "fooBar"],
    (type_mismatch "int" "string", [ `f "yOpt" ])
  ]
end

module SampleEx11 : SampleGenerated = struct
  include Ex11
  let name = "SampleEx11"
  let expected_json_shape_explanation = Typedesc_examples.Ex11.expected_json_shape_explanation

  let samples = [
    "null", `null, ("expecting type 'unit' but the given is of type 'null'", []);
  ]
end

module SampleEx12 : SampleGenerated = struct
  include Ex12
  let name = "SampleEx12"
  let expected_json_shape_explanation = Typedesc_examples.Ex12.expected_json_shape_explanation
  let samples = [
    "null", `null, ("expecting type 'string' but the given is of type 'null'", []);
    "not one of", `str "case-at4", ("given string 'case-at4' is not one of [ 'Case_at0', 'case-at1', 'Case-at2', 'Case-third' ]", []);
  ]
end

module SampleEx13 : SampleGenerated = struct
  include Ex13
  let name = "SampleEx13"
  let expected_json_shape_explanation = Typedesc_examples.Ex13.expected_json_shape_explanation

  let samples =
    SampleEx01.samples
    |&> (fun (name, jv, (msg, path)) ->
        (name, `obj [ ("student1", jv) ], (msg, path @ [ `f "student1" ]))
      )
end

module SampleEx14 : SampleGenerated = struct
  include Ex14
  let name = "SampleEx14"
  let expected_json_shape_explanation = Typedesc_examples.Ex14.expected_json_shape_explanation

  let samples = [
    "null", `null, (tuple_not_obj "null", []);

    "missing field _0", `obj [],
    (field_not_found "_0", []);

    "missing field _1", `obj [ "_0", `num 12.3 ],
    (field_not_found "_1", []);

    "type mismatch _0", `obj [ ("_0", `null); ("_1", `str "test") ],
    (type_mismatch "float" "null", [ `f "_0" ]);

    "type mismatch _1", `obj [ ("_0", `num 12.3); ("_1", `bool false) ],
    (type_mismatch "string" "bool", [ `f "_1" ]);
  ]
end

module SampleEx15 : SampleGenerated = struct
  include Ex15
  let name = "SampleEx15"
  let expected_json_shape_explanation = Typedesc_examples.Ex15.expected_json_shape_explanation
  let samples =
    let discriminator = "tag" in
    let arg = "value" in
    [
      "null", `null, (not_obj "variant" "null", []);

      "discriminator not string", `obj [ (discriminator, `null)],
      (discriminator_not_string "null", [ `f discriminator ]);

      "invalid constructor", ctor0 ~discriminator "FooBar",
      ("given discriminator field value 'FooBar' is not one of [ 'student1', 'student2', 'student3', 'student4', 'int-list1', 'int-list2' ]", [ `f discriminator ]);

      "student1: missing field", ctor0 ~discriminator "student1",
      (field_not_found "student", []);

      "student1: missing field", ctor_record ~discriminator "student1" [
        "student", `obj []
      ],
      (field_not_found "admissionYear", [ `f "student" ]);

      "student2: missing field", ctor0 ~discriminator "student2",
      (field_not_found "admissionYear", []);

      "student3: missing field", ctor0 ~discriminator "student3",
      (field_not_found arg, []);

      "student3: missing field", ctor1 ~discriminator "student3" ~arg (`obj []),
      (field_not_found "admissionYear", [ `f arg ]);

      "student4: missing field", ctor0 ~discriminator "student4",
      (field_not_found "admissionYear", []);
    ]
end

module SampleEx16 : SampleGenerated = struct
  include Ex16
  let name = "SampleEx16"
  let expected_json_shape_explanation = Typedesc_examples.Ex16.expected_json_shape_explanation
  let samples = [
    "null", `null, (record_not_obj "null", []);

    "missing field", `obj [
      "student", `obj [
        ("admissionYear", `num 1984.);
        ("name", `str "William Gibson")
      ];
      "person1", ctor0 "anonymous";
      "kind", `str "anonymous";
    ],
    (field_not_found "value", []);

    "missing discriminator (spreading)", `obj [
      "student", `obj [
        ("admissionYear", `num 1984.);
        ("name", `str "William Gibson")
      ];
      "value", `num 12.;
      "person1", ctor0 "anonymous";
    ],
    (discriminator_not_found "kind", []);

    "missing discriminator (nested)", `obj [
      "student", `obj [
        ("admissionYear", `num 1984.);
        ("name", `str "William Gibson")
      ];
      "value", `num 12.;
      "person1", `obj [];
      "kind", `str "with-id";
      "arg", `num 123.
    ],
    (discriminator_not_found "kind", [ `f "person1" ]);

    "not integer", `obj [
      "student", `obj [
        ("admissionYear", `num 1984.);
        ("name", `str "William Gibson")
      ];
      "value", `num 12.;
      "person1", `obj [
        "kind", `str "anonymous";
      ];
      "kind", `str "with-id";
      "arg", `num 123.5
    ],
    (not_integer 123.5, [ `f "arg" ]);
  ]
end

let ttd_name (type t) ((module Td) : t Typed_type_desc.typed_type_decl) =
  Td.decl.td_name

module SampleIdentInt_base = struct
  open Bindoj_typedesc.Typed_type_desc
  let child = Coretypes.(Prims.int |> to_typed_type_decl "child")

  let env : tdenv =
    { prim_ident_typemap = StringMap.empty;
      alias_ident_typemap =
        StringMap.of_list [ ttd_name child, Boxed child ];
    }

  let samples = [
    "not integer", `num 42.1,
    (not_integer 42.1, [ ])
  ]

  let child_json_shape_explanation : json_shape_explanation =
    `named ("Child", `integral)
end

module SampleIdentInt_Refl : Sample = struct
  open Bindoj_typedesc.Typed_type_desc
  include SampleIdentInt_base

  let decl = Coretype.(ident (ttd_name child) |> mk) |> alias_decl "parent"
  let reflect =
    let open Runtime in
    let child_refl = Typed.to_refl child in
    Expr.(of_refl child_refl, to_refl child_refl)
    |> !! Reflects.reflect_of_alias

  type t = int
  let name = "SampleIdentInt_Refl"

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
            ("Parent", child_json_shape_explanation)))
    )
end

module SampleIdentInt_Coretypes : Sample = struct
  open Bindoj_typedesc.Typed_type_desc
  include SampleIdentInt_base

  let parent = Coretypes.(ident' child |> to_typed_type_decl ~env "parent")

  let decl = Typed.decl parent
  let reflect = Typed.to_refl parent

  type t = int
  let name = "SampleIdentInt_Coretypes"

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
            ("Parent", child_json_shape_explanation)))
    )
end

module SampleIdentIntOption_Coretypes : Sample = struct
  open Bindoj_typedesc.Typed_type_desc
  include SampleIdentInt_base

  let parent = Coretypes.(ident' child |> option |> to_typed_type_decl ~env "parent")

  let decl = Typed.decl parent
  let reflect = Typed.to_refl parent

  type t = int option
  let name = "SampleIdentIntOption_Coretypes"

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
            ("Parent", `nullable child_json_shape_explanation)))
    )
end

module SampleIdentStudent_base = struct
  open Bindoj_typedesc.Typed_type_desc
  let child = Ex01.(Typed.mk decl reflect)

  let env : tdenv =
    { prim_ident_typemap = StringMap.empty;
      alias_ident_typemap =
        StringMap.of_list [ ttd_name child, Boxed child ];
    }

  let samples = [
    "missing field", `obj [ ("admissionYear", `num 1984.) ]
    , (field_not_found "name", []);
  ]

  let child_json_shape_explanation =
    `named
      ("Student",
        (`object_of
            [`mandatory_field ("admissionYear", `integral);
            `mandatory_field ("name", `string)]))
end

module SampleIdentStudent_Refl : Sample = struct
  open Bindoj_typedesc.Typed_type_desc
  include SampleIdentStudent_base

  let decl = Coretype.(ident (ttd_name child) |> mk) |> alias_decl "parent"
  let reflect =
    let open Runtime in
    let child_refl = Typed.to_refl child in
    Expr.(of_refl child_refl, to_refl child_refl)
    |> !! Reflects.reflect_of_alias

  type t = Ex01.t
  let name = "SampleIdentStudent_Refl"

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
            ("Parent", child_json_shape_explanation)))
    )
end

module SampleIdentStudent_Coretypes : Sample = struct
  open Bindoj_typedesc.Typed_type_desc
  include SampleIdentStudent_base

  let parent = Coretypes.(ident' child |> to_typed_type_decl ~env "parent")

  let decl = Typed.decl parent
  let reflect = Typed.to_refl parent

  type t = Ex01.t
  let name = "SampleIdentStudent_Coretypes"

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
            ("Parent", child_json_shape_explanation)))
    )
end

module SampleIdentStudentOption_Coretypes : Sample = struct
  open Bindoj_typedesc.Typed_type_desc
  include SampleIdentStudent_base

  let parent = Coretypes.(ident' child |> option |> to_typed_type_decl ~env "parent")

  let decl = Typed.decl parent
  let reflect = Typed.to_refl parent

  type t = Ex01.t option
  let name = "SampleIdentStudentOption_Coretypes"

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
            ("Parent", `nullable child_json_shape_explanation)))
    )
end

let all_generated : (module SampleGenerated) list = [
  (module SampleEx01);
  (module SampleEx01_inherited_mangling);
  (module SampleEx02);
  (module SampleEx02_no_mangling);
  (module SampleEx02_inherited_mangling);
  (module SampleEx03);
  (module SampleEx04);
  (module SampleEx05);
  (module SampleEx06);
  (module SampleEx07);
  (module SampleEx08);
  (module SampleEx09);
  (module SampleEx10);
  (module SampleEx11);
  (module SampleEx12);
  (module SampleEx13);
  (module SampleEx14);
  (module SampleEx15);
  (module SampleEx16);
]

let all : (module Sample) list = [
  (module SampleEx01);
  (module SampleEx01_inherited_mangling);
  (module SampleEx02);
  (module SampleEx02_no_mangling);
  (module SampleEx02_inherited_mangling);
  (module SampleEx03);
  (module SampleEx04);
  (module SampleEx05);
  (module SampleEx06);
  (module SampleEx07);
  (module SampleEx08);
  (module SampleEx09);
  (module SampleEx10);
  (module SampleEx11);
  (module SampleEx12);
  (module SampleEx13);
  (module SampleEx14);
  (module SampleEx15);
  (module SampleEx16);
  (module SampleIdentInt_Refl);
  (module SampleIdentInt_Coretypes);
  (module SampleIdentIntOption_Coretypes);
  (module SampleIdentStudent_Refl);
  (module SampleIdentStudent_Coretypes);
  (module SampleIdentStudentOption_Coretypes);
]
