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
open Bindoj_base
open Bindoj_runtime
open Bindoj_typedesc
open Typedesc_generated_examples
open Typedesc_generated_examples.Util.Sample_value.JvHelper

module type Sample_dynamic_desc = sig
  type t
  val decl : Type_desc.type_decl
  val reflect : t Runtime.Refl.t
  val samples : (string * Json.jv * (string * Json.jvpath)) list
  val expected_json_shape_explanation : json_shape_explanation option
end

module type Sample_generated_desc = sig
  include Sample_dynamic_desc
  val of_json' : ?path:Json.jvpath -> Json.jv -> t OfJsonResult.t
  val pp : ppf -> t -> unit
  val json_shape_explanation : json_shape_explanation
end

module type Sample_dynamic = sig
  val name : string
  val env : Typed_type_desc.Type_decl_environment.env
  val descs : (module Sample_dynamic_desc) list
end

module type Sample_generated = sig
  val name : string
  val env : Typed_type_desc.Type_decl_environment.env
  val descs : (module Sample_generated_desc) list
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

  let ttd_name (type t) ((module Td) : t Typed_type_desc.typed_type_decl) =
    Td.decl.td_name

  let with_warning_named name shape =
    `with_warning ("not considering any config if exists", (`named (name, shape)))
end

module Sample_ex_coretype = struct
  let name = "Sample_ex_coretype"
  module Sample_various_prim_types : Sample_generated_desc = struct
    include Ex_coretype.Various_prim_types

    let expected_json_shape_explanation =
      Typedesc_examples.Ex_coretype.Various_prim_types.expected_json_shape_explanation

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

  module Sample_various_tuple_types : Sample_generated_desc = struct
    include Ex_coretype.Various_tuple_types
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_coretype.Various_tuple_types.expected_json_shape_explanation

    let samples = [
      "invalid tuple",
      `obj [
        "tuple", `null;
      ],
      ("an array is expected for a tuple value, but the given is of type 'null'", [ `f "tuple" ]);

      "invalid type for tuple (nested)",
      `obj [
        "tuple", `arr [ `num 4.; `num 2. ];
        "objtuple", `arr [ `num 4.; `num 2. ]
      ],
      (tuple_not_obj "array", [ `f "objtuple" ]);

      "incorrect list length in nested field",
      `obj [
        "tuple", `arr [ `num 4.; `num 2. ];
        "objtuple", `obj ["_0", `num 4.; "_1", `num 2.];
        "nested", `arr [ `num 42.; `arr [ ]; `arr [`num 4.; `num 2.; `null]; ];
      ],
      (incorrect_tuple_length 2 3, [ `i 2; `f "nested" ]);
    ]
  end

  module Sample_named_json : Sample_generated_desc = struct
    include Ex_coretype.Named_json
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_coretype.Named_json.expected_json_shape_explanation
    let samples = []
  end

  module Sample_with_int53p : Sample_generated_desc = struct
    include Ex_coretype.With_int53p
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_coretype.With_int53p.expected_json_shape_explanation
    let samples = []
  end

  let env = Ex_coretype.env
  let descs : (module Sample_generated_desc) list = [
    (module Sample_various_prim_types);
    (module Sample_various_tuple_types);
    (module Sample_named_json);
    (module Sample_with_int53p);
  ]
end

module Sample_ex_alias = struct
  let name = "Sample_ex_alias"
  module Sample_unit : Sample_generated_desc = struct
    include Ex_alias.Unit
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_alias.Unit.expected_json_shape_explanation

    let samples = [
      "null", `null, ("expecting type 'unit' but the given is of type 'null'", []);
    ]
  end

  module Sample_objtuple : Sample_generated_desc = struct
    include Ex_alias.Objtuple
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_alias.Objtuple.expected_json_shape_explanation

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

  module Sample_int_opt : Sample_generated_desc = struct
    include Ex_alias.Int_opt
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_alias.Int_opt.expected_json_shape_explanation
    let samples = [
      "not integer", `num 12.3, (not_integer 12.3, []);
      "type mismatch", `str "test", (type_mismatch "int" "string", []);
    ]
  end

  let env = Ex_alias.env

  let descs : (module Sample_generated_desc) list = [
    (module Sample_unit);
    (module Sample_objtuple);
    (module Sample_int_opt);
  ]
end

module Sample_ex_record = struct
  let name = "Sample_ex_record"
  module Sample_student : Sample_generated_desc = struct
    include Ex_record.Student
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_record.Student.expected_json_shape_explanation
    let samples = [
      "not obj", `null, (record_not_obj "null", []);

      "missing field", `obj [ ("admissionYear", `num 1984.) ]
      , (field_not_found "name", []);

      "type mismatch", `obj [ ("admissionYear", `null); ("name", `str "William Gibson") ]
      , (type_mismatch "int" "null", [ `f "admissionYear" ]);

      "not integer", `obj [ ("admissionYear", `num 1984.5); ("name", `str "William Gibson") ]
      , (not_integer 1984.5, [ `f "admissionYear" ]);
    ]
  end

  let env = Ex_record.env
  let descs : (module Sample_generated_desc) list = [
    (module Sample_student);
  ]
end

module Sample_ex_variant = struct
  let name = "Sample_ex_variant"
  module Sample_person : Sample_generated_desc = struct
    include Ex_variant.Person
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_variant.Person.expected_json_shape_explanation
    let samples = [
      "missing discriminator", `obj [ ],
      (discriminator_not_found "kind", []);

      "discriminator not string", `obj [ ("kind", `null)],
      (discriminator_not_string "null", [ `f "kind" ]);

      "invalid constructor", ctor0 "FooBar",
      ("given discriminator field value 'FooBar' is not one of [ 'anonymous', 'with-id', 'student', 'teacher' ]", [ `f "kind" ]);

      "with-id: missing field", ctor0 "with-id",
      (field_not_found "value", []);

      "with-id: not integer", ctor_record "with-id" [ ("value", `num 1.2) ],
      (not_integer 1.2, [ `f "value" ]);

      "student: inline record", ctor0 "student",
      (field_not_found "studentId", []);

      "teacher: missing field", ctor_record "teacher" [ ("facultyId", `num 2001.) ],
      (field_not_found "name", []);

      "teacher: type mismatch", ctor_record "teacher" [ ("facultyId", `num 2001.); ("name", `str "Arthur C. Clark"); ("department", `arr []); ],
      (type_mismatch "string" "array", [ `f "department" ]);
    ]
  end

  module Sample_int_list : Sample_generated_desc = struct
    include Ex_variant.Int_list
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_variant.Int_list.expected_json_shape_explanation

    let samples =
      let intCons_null = ctor2 "intcons" `null in
      [ "missing discriminator", `obj [ ],
        (discriminator_not_found "kind", []);

        "discriminator not string", `obj [ ("kind", `null)],
        (discriminator_not_string "null", [ `f "kind" ]);

        "missing discriminator (nested 1)", `obj [ ] |> intCons 1 ,
        (discriminator_not_found "kind", [ `i 1; `f "value" ]);

        "missing discriminator (nested 2)", `obj [ ] |> intCons 1 |> intCons 2,
        (discriminator_not_found "kind", [ `i 1; `f "value"; `i 1; `f "value" ]);

        "missing field", ctor0 "intcons",
        (field_not_found "value", []);

        "missing field (nested 1)", ctor0 "intcons" |> intCons 1 ,
        (field_not_found "value", [ `i 1; `f "value" ]);

        "missing field (nested 2)", ctor0 "intcons" |> intCons 1 |> intCons 2,
        (field_not_found "value", [ `i 1; `f "value"; `i 1; `f "value" ]);

        "type mismatch", intCons_null intNil,
        (type_mismatch "int" "null" , [ `i 0; `f "value" ]);

        "type mismatch (nested 1)", intCons_null intNil |> intCons 1 ,
        (type_mismatch "int" "null", [ `i 0; `f "value"; `i 1; `f "value" ]);

        "type mismatch (nested 2)", intCons_null intNil |> intCons 1 |> intCons 2,
        (type_mismatch "int" "null", [ `i 0; `f "value"; `i 1; `f "value"; `i 1; `f "value" ]);

        "arg is not array", `obj [
          "kind", `str "intcons";
          "value", `str "invalid"
        ],
        ("an array is expected for a tuple value, but the given is of type 'string'", [ `f "value" ]);
      ]
  end

  module Sample_customized_union : Sample_generated_desc = struct
    include Ex_variant.Customized_union
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_variant.Customized_union.expected_json_shape_explanation

    let samples =
      ( [ "case-tuple-like-arg'", "arg";
          "case-tuple-like-exactly'", "Argument";
          "case-tuple-like-kind-name'", "case-tuple-like-kind-name'";
          "case-tuple-like-kind-name-no-mangling", "Case_tuple_like_kind_name_no_mangling";
          "case-tuple-like-kind-name-no-mangling-with-ctor-name", "case-tuple-like-kind-name-no-mangling-with-ctor-name";
        ] |&>> (fun (discriminator_value, arg_fname) ->
          [ sprintf "%s: missing field" discriminator_value, ctor1 ~discriminator:"tag" discriminator_value (`num 42.),
            (field_not_found arg_fname, []);
            sprintf "%s: not integer" discriminator_value, ctor1 ~discriminator:"tag" discriminator_value (`num 43.21),
            (not_integer 43.21, [ `f arg_fname ]); ])
      ) @ [
      "missing discriminator", `obj [ ],
      (discriminator_not_found "tag", []);

      "discriminator not string", `obj [ ("tag", `null)],
      (discriminator_not_string "null", [ `f "tag" ]);

      "Case_inline_record: missing field", ctor_record ~discriminator:"tag" "case-inline-record'" [ "x", `num 4.; "y", `num 2.1 ],
      (field_not_found "x", []);

      "Case_inline_record: missing field", ctor_record ~discriminator:"tag" "case-inline-record'" [ "x'", `num 4. ],
      (field_not_found "y'", [ ]);

      "Case_inline_record: not integer", ctor_record ~discriminator:"tag" "case-inline-record'" [ "x'", `str "fooBar"; "y'", `num 2. ],
      (type_mismatch "int" "string", [ `f "x'" ]);

      "Case_inline_record: not integer", ctor_record ~discriminator:"tag" "case-inline-record'" [ "x'", `num 4.; "y'", `num 2.1 ],
      (not_integer 2.1, [ `f "y'" ]);
    ]
  end

  module Sample_polymorphic : Sample_generated_desc = struct
    include Ex_variant.Polymorphic

    let expected_json_shape_explanation =
      Typedesc_examples.Ex_variant.Polymorphic.expected_json_shape_explanation

    let samples = [
      "foo2: incorrect list length", ctorN "foo2" [ `num 1. ],
      (incorrect_list_length 2 1, [ `f "value" ])
    ]
  end

  let env = Ex_variant.env
  let descs : (module Sample_generated_desc) list = [
    (module Sample_person);
    (module Sample_int_list);
    (module Sample_polymorphic);
  ]
end

module Sample_ex_mangling = struct
  let name = "Sample_ex_mangling"
  module Sample_student_inherited  :Sample_generated_desc = struct
    include Ex_mangling.Student_inherited
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_mangling.Student_inherited.expected_json_shape_explanation
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

  module Sample_person_no_mangling : Sample_generated_desc = struct
    include Ex_mangling.Person_no_mangling
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_mangling.Person_no_mangling.expected_json_shape_explanation
    let samples = [
      "missing discriminator", `obj [ ],
      (discriminator_not_found "kind", []);

      "discriminator not string", `obj [ ("kind", `null)],
      (discriminator_not_string "null", [ `f "kind" ]);

      "invalid constructor", ctor0 "FooBar",
      ("given discriminator field value 'FooBar' is not one of [ 'Anonymous', 'With_id', 'Student', 'Teacher' ]", [ `f "kind" ]);

      "With_id: missing field", ctor0 "With_id",
      (field_not_found "value", []);

      "With_id: not integer", ctor_record "With_id" [ ("value", `num 1.2) ],
      (not_integer 1.2, [ `f "value" ]);

      "Student: inline record", ctor0 "Student",
      (field_not_found "student_id", []);

      "Teacher: missing field", ctor_record "Teacher" [ ("faculty_id", `num 2001.) ],
      (field_not_found "name", []);

      "Teacher: type mismatch", ctor_record "Teacher" [ ("faculty_id", `num 2001.); ("name", `str "Arthur C. Clark"); ("department", `arr []); ],
      (type_mismatch "string" "array", [ `f "department" ]);
    ]
  end

  module Sample_person_inherited : Sample_generated_desc = struct
    include Ex_mangling.Person_inherited
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_mangling.Person_inherited.expected_json_shape_explanation
    let samples = [
      "missing discriminator", `obj [ ],
      (discriminator_not_found "kind", []);

      "discriminator not string", `obj [ ("kind", `null)],
      (discriminator_not_string "null", [ `f "kind" ]);

      "invalid constructor", ctor0 "FooBar",
      ("given discriminator field value 'FooBar' is not one of [ 'Anonymous', 'With_id', 'student', 'Teacher' ]", [ `f "kind" ]);

      "With_id: missing field", ctor0 "With_id",
      (field_not_found "value", []);

      "With_id: not integer", ctor_record "With_id" [ ("value", `num 1.2) ],
      (not_integer 1.2, [ `f "value" ]);

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

  module Sample_enum : Sample_generated_desc = struct
    include Ex_mangling.Enum
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_mangling.Enum.expected_json_shape_explanation
    let samples = [
      "null", `null, ("expecting type 'string' but the given is of type 'null'", []);
      "not one of", `str "case-at4", ("given string 'case-at4' is not one of [ 'Case_at0', 'case-at1', 'Case-at2', 'Case-third' ]", []);
    ]
  end

  let env = Ex_mangling.env
  let descs : (module Sample_generated_desc) list = [
    (module Sample_student_inherited);
    (module Sample_person_no_mangling);
    (module Sample_person_inherited);
    (module Sample_enum);
  ]
end

module Sample_ex_optional = struct
  let name = "Sample_ex_optional"
  module Sample_xy_opt : Sample_generated_desc = struct
    include Ex_optional.Xy_opt
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_optional.Xy_opt.expected_json_shape_explanation

    let samples = [
      "not obj", `null, (record_not_obj "null", []);

      "type mismatch", `obj [ "yOpt", `str "fooBar"],
      (type_mismatch "int" "string", [ `f "yOpt" ])
    ]
  end

  module Sample_variant : Sample_generated_desc = struct
    include Ex_optional.Variant
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_optional.Variant.expected_json_shape_explanation
    let samples = [
      "discriminator not found", `obj [], (discriminator_not_found "tag", []);
      "discriminator not string", `obj [ ("tag", `null)], (discriminator_not_string "null", [ `f "tag" ]);

      "invalid constructor", ctor0 ~discriminator:"tag" "Tuple_like",
      ("given discriminator field value 'Tuple_like' is not one of [ 'tuple-like', 'tuple-like-alias', 'tuple-like-obj', 'tuple-like-spreading', 'inline-record', 'inline-record-spreading', 'reused-inline-record' ]", [ `f "tag" ]);

      "type mismatch", `obj [ ("tag", `str "tuple-like"); ("arg", `obj []); ],
      (type_mismatch "int" "object", [ `f "arg" ]);

      "not integer", `obj [ ("tag", `str "tuple-like"); ("arg", `num 12.3); ],
      (not_integer 12.3, [ `f "arg" ]);

      "type mismatch", `obj [ ("tag", `str "tuple-like-alias"); ("arg", `arr [ `null; `num 1. ]); ],
      (type_mismatch "int" "array", [ `f "arg" ]);

      "not integer", `obj [ ("tag", `str "tuple-like-alias"); ("arg", `num 12.3); ],
      (not_integer 12.3, [ `f "arg" ]);

      "type mismatch", `obj [ ("tag", `str "tuple-like-spreading"); ("xOpt", `bool false) ],
      (type_mismatch "int" "bool", [ `f "xOpt" ]);

      "type mismatch", `obj [ ("tag", `str "tuple-like-spreading"); ("yOpt", `str "foobar") ],
      (type_mismatch "int" "string", [ `f "yOpt" ]);

      "type mismatch", `obj [ ("tag", `str "inline-record"); ("yOpt", `str "foobar") ],
      (type_mismatch "int" "string", [ `f "yOpt" ]);

      "type mismatch", `obj [ ("tag", `str "inline-record-spreading"); ("yOpt", `str "foobar") ],
      (type_mismatch "int" "string", [ `f "yOpt" ]);

      "type mismatch", `obj [ ("tag", `str "reused-inline-record"); ("yOpt", `str "foobar") ],
      (type_mismatch "int" "string", [ `f "yOpt" ]);
    ]
  end

  let env = Ex_optional.env
  let descs : (module Sample_generated_desc) list = [
    (module Sample_xy_opt);
    (module Sample_variant);
  ]
end

module Sample_ex_ident = struct
  let name = "Sample_ex_ident"
  module Sample_student_pair : Sample_generated_desc = struct
    include Ex_ident.Student_pair
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_ident.Student_pair.expected_json_shape_explanation

    let samples =
      Sample_ex_record.Sample_student.samples
      |&> (fun (name, jv, (msg, path)) ->
          (name, `obj [ ("student1", jv) ], (msg, path @ [ `f "student1" ]))
        )
  end

  let env = Ex_ident.env
  let descs : (module Sample_generated_desc) list = [
    (module Sample_student_pair);
  ]
end

module Sample_ex_nested = struct
  let name = "Sample_ex_nested"
  module Sample_point2 : Sample_generated_desc = struct
    include Ex_nested.Point2
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_nested.Point2.expected_json_shape_explanation
    let samples = []
  end

  module Sample_record : Sample_generated_desc = struct
    include Ex_nested.Record
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_nested.Record.expected_json_shape_explanation
    let samples = [
      "null", `null, (record_not_obj "null", []);

      "missing field", `obj [
        "unit", `num 1.;
        "point2", `obj [ "x", `num 1.; "y", `num 2. ];
        "y", `num 2.;
        "person", ctor0 "Anonymous";
        "kind", `str "Anonymous";
      ],
      (field_not_found "x", []);

      "missing discriminator (spreading)", `obj [
        "unit", `num 1.;
        "point2", `obj [ "x", `num 1.; "y", `num 2. ];
        "x", `num 1.;
        "y", `num 2.;
        "person", ctor0 "Anonymous";
      ],
      (discriminator_not_found "kind", []);

      "missing discriminator (nested)", `obj [
        "unit", `num 1.;
        "point2", `obj [ "x", `num 1.; "y", `num 2. ];
        "x", `num 1.;
        "y", `num 2.;
        "person", `obj [];
        "kind", `str "With_id";
        "value", `num 123.
      ],
      (discriminator_not_found "kind", [ `f "person" ]);

      "not integer", `obj [
        "unit", `num 1.;
        "point2", `obj [ "x", `num 1.; "y", `num 2. ];
        "x", `num 1.;
        "y", `num 2.;
        "person", ctor0 "Anonymous";
        "kind", `str "With_id";
        "value", `num 123.5
      ],
      (not_integer 123.5, [ `f "value" ]);
    ]
  end

  module Sample_variant : Sample_generated_desc = struct
    include Ex_nested.Variant
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_nested.Variant.expected_json_shape_explanation
    let samples =
      let discriminator = "tag" in
      let arg = "arg" in
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
  let env = Ex_nested.env
  let descs : (module Sample_generated_desc) list = [
    (module Sample_point2);
    (module Sample_record);
    (module Sample_variant);
  ]
end

module Sample_ex_nested_multiply = struct
  let name = "Sample_ex_nested_multiply"

  module Sample_record : Sample_generated_desc = struct
    include Ex_nested_multiply.Record
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_nested_multiply.Record.expected_json_shape_explanation
    let samples = [
      "null", `null, (not_obj "record" "null", []);

      "type mismatch (.nestedRecord.point2.x)",
      `obj [
        "nestedRecord", `obj [
          "unit", `num 1.;
          "point2", `obj [ "x", `str "1"; "y", `num 2. ];
          "x", `num 1.;
          "y", `num 2.;
          "person", ctor0 "Anonymous";
          "kind", `str "With_id";
          "value", `num 123.
        ]
      ],
      (type_mismatch "float" "string", [ `f "x"; `f "point2"; `f "nestedRecord" ]);

      "field not found (.nestedRecord.arg)",
      `obj [
        "nestedRecord", `obj [
          "unit", `num 1.;
          "point2", `obj [ "x", `num 1.; "y", `num 2. ];
          "x", `num 1.;
          "y", `num 2.;
          "person", ctor0 "Anonymous";
          "kind", `str "With_id";
        ]
      ],
      (field_not_found "value", [ `f "nestedRecord" ]);

      "field not found (.nestedRecord.value)",
      `obj [
        "nestedRecord", `obj [
          "unit", `num 1.;
          "point2", `obj [ "x", `num 1.; "y", `num 2. ];
          "x", `num 1.;
          "y", `num 2.;
          "person", ctor0 "Anonymous";
          "kind", `str "With_id";
          "value", `num 123.
        ];
        "unit", `num 1.;
          "point2", `obj [ "x", `num 1.; "y", `num 2. ];
          "x", `num 1.;
          "y", `num 2.;
          "person", ctor0 "With_id";
      ],
      (field_not_found "value", [ `f "person" ]);
    ]
  end

  module Sample_variant : Sample_generated_desc = struct
    include Ex_nested_multiply.Variant
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_nested_multiply.Variant.expected_json_shape_explanation
    let samples = [
      "null", `null, (not_obj "variant" "null", []);

      "type mismatch (.nestedRecord.point2.x)",
      `obj [
        "label", `str "nested-record";
        "nestedRecord", `obj [
          "unit", `num 1.;
          "point2", `obj [ "x", `str "1"; "y", `num 2. ];
          "x", `num 1.;
          "y", `num 2.;
          "person", ctor0 "Anonymous";
          "kind", `str "With_id";
          "arg", `num 123.
        ]
      ],
      (type_mismatch "float" "string", [ `f "x"; `f "point2"; `f "nestedRecord" ]);

      "missing discriminator (.nestedRecord.value)",
      `obj [
        "label", `str "nested-record";
        "nestedRecord", `obj [
          "unit", `num 1.;
          "point2", `obj [ "x", `num 1.; "y", `num 2. ];
          "x", `num 1.;
          "y", `num 2.;
          "person", ctor0 "Anonymous";
          "kind", `str "With_id";
        ]
      ],
      (field_not_found "value", [ `f "nestedRecord" ]);

      "missing discriminator (.nestedVariant.tag)",
      `obj [
        "label", `str "nested-variant";
        "nestedVariant", `obj [ ];
      ],
      (discriminator_not_found "tag", [ `f "nestedVariant" ]);

      "type mismatch (.nestedVariant.name)",
      `obj [
        "label", `str "nested-variant";
        "nestedVariant", `obj [
          "tag", `str "student2";
          "admissionYear", `num 1234.;
          "name", `num 1.;
        ]
      ],
      (type_mismatch "string" "number", [ `f "name"; `f "nestedVariant" ]);

      "not array (.value.[1].value)",
      `obj [
        "label", `str "nested-variant";
        "nestedVariant", `obj [
          "tag", `str "student2";
          "admissionYear", `num 1234.;
          "name", `str "foo bar";
        ];
        "tag", `str "int-list2";
        "kind", `str "intcons";
        "value", `arr [
          `num 1.; `obj [ "kind", `str "intcons"; "value", `str "invalid" ]
        ]
      ],
      ("an array is expected for a tuple value, but the given is of type 'string'", [ `f "value"; `i 1; `f "value" ]);
    ]
  end

  let env = Ex_nested_multiply.env
  let descs : (module Sample_generated_desc) list = [
    (module Sample_record);
    (module Sample_variant);
  ]
end

module Sample_ex_version_substring = struct
  let name = "Sample_ex_version_substring"
  module Sample_record : Sample_generated_desc = struct
    include Ex_version_substring.Record
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_version_substring.Record.expected_json_shape_explanation
    let samples =
      let fields = [
        (Some "v5-3VersionInfo", "v5_3VersionInfo", `str "Case-version-v1");
        (None, "versionInfoV2", `num (-1.));
        (Some "versionInfoV20", "versionInfoV2_0", `num (-2.));
        (Some "versionInfoV201", "versionInfoV2_0_1", `num (-3.));
        (None, "versionV3Info", `num 1.);
        (Some "versionV30Info", "versionV3_0Info", `num 2.);
        (Some "versionV301Info", "versionV3_0_1Info", `num 3.);
        (None, "v4VersionInfo", `num 4.);
        (Some "v40VersionInfo", "v4_0VersionInfo", `num 5.);
        (Some "v401VersionInfo", "v4_0_1VersionInfo", `num 6.);
      ] in
      (fields |&?> function
        | (None, _, _) -> None
        | (Some invalid_name, json_name, _) ->
        Some (sprintf "field '%s' not found" json_name, `obj (
          fields |&> fun (invalid_name', json_name, jv) ->
            ((match invalid_name' with
              | Some s' when invalid_name = s' -> invalid_name
              | _ -> json_name), jv)),
        (field_not_found json_name, [])))
      @ [
        "invalid enum case", `obj [
          ("v5_3VersionInfo", `str "Case-v2-0-version")
        ], ("given string 'Case-v2-0-version' is not one of [ 'Case-version-v1', 'Case-v2_0-version', 'v3_0_1-case-version' ]", [ `f "v5_3VersionInfo" ]);
        "invalid enum case", `obj [
          ("v5_3VersionInfo", `str "v3-0-1-case-version")
        ], ("given string 'v3-0-1-case-version' is not one of [ 'Case-version-v1', 'Case-v2_0-version', 'v3_0_1-case-version' ]", [ `f "v5_3VersionInfo" ]);
      ]
  end

  module Sample_variant : Sample_generated_desc = struct
    include Ex_version_substring.Variant
    let expected_json_shape_explanation =
      Typedesc_examples.Ex_version_substring.Variant.expected_json_shape_explanation
    let samples = [
      "discriminator not found", `obj [], (discriminator_not_found "kind", []);
      "discriminator not string", `obj [ ("kind", `null)], (discriminator_not_string "null", [ `f "kind" ]);

      "invalid constructor", ctor0 "version-info-v1-0",
      ("given discriminator field value 'version-info-v1-0' is not one of [ 'version-info-v1_0', 'version-v1_0-info', 'v1_0-version-info', 'no-preserving-version-substring-v1-0' ]", [ `f "kind" ]);
      "invalid constructor", ctor0 "version-v1-0-info",
      ("given discriminator field value 'version-v1-0-info' is not one of [ 'version-info-v1_0', 'version-v1_0-info', 'v1_0-version-info', 'no-preserving-version-substring-v1-0' ]", [ `f "kind" ]);
      "invalid constructor", ctor0 "v1-0-version-info",
      ("given discriminator field value 'v1-0-version-info' is not one of [ 'version-info-v1_0', 'version-v1_0-info', 'v1_0-version-info', 'no-preserving-version-substring-v1-0' ]", [ `f "kind" ]);
      "invalid constructor", ctor0 "no-preserving-version-substring-v1_0",
      ("given discriminator field value 'no-preserving-version-substring-v1_0' is not one of [ 'version-info-v1_0', 'version-v1_0-info', 'v1_0-version-info', 'no-preserving-version-substring-v1-0' ]", [ `f "kind" ]);

      "field not found", ctor_record "version-info-v1_0" [
        "versionInfoV1", `num 1.;
        "versionInfoV10", `num 2.;
      ],
      (field_not_found "versionInfoV1_0", []);

      "field not found", ctor_record "version-info-v1_0" [
        "versionInfoV1", `num 1.;
        "versionInfoV1_0", `num 2.;
        "versionInfoV101", `num 3.;
      ],
      (field_not_found "versionInfoV1_0_1", []);

      "field not found", ctor_record "version-v1_0-info" [
        "versionV1Info", `num 1.;
        "versionV10Info", `num 2.;
      ],
      (field_not_found "versionV1_0Info", []);

      "field not found", ctor_record "version-v1_0-info" [
        "versionV1Info", `num 1.;
        "versionV1_0Info", `num 2.;
        "versionV101Info", `num 3.;
      ],
      (field_not_found "versionV1_0_1Info", []);

      "field not found", ctor_record "v1_0-version-info" [
        "v1VersionInfo", `num 1.;
        "v10VersionInfo", `num 2.;
      ],
      (field_not_found "v1_0VersionInfo", []);

      "field not found", ctor_record "v1_0-version-info" [
        "v1VersionInfo", `num 1.;
        "v1_0VersionInfo", `num 2.;
        "v101VersionInfo", `num 3.;
      ],
      (field_not_found "v1_0_1VersionInfo", []);

      "field not found", ctor_record "no-preserving-version-substring-v1-0" [
        "versionInfoV1", `num 1.;
        "versionInfoV1_0", `num 2.;
      ],
      (field_not_found "versionInfoV10", []);
      "field not found", ctor_record "no-preserving-version-substring-v1-0" [
        "versionInfoV1", `num 1.;
        "versionInfoV10", `num 2.;
        "versionInfoV1_0_1", `num 3.;
      ],
      (field_not_found "versionInfoV101", []);
    ]
  end

  let env = Ex_version_substring.env
  let descs : (module Sample_generated_desc) list = [
    (module Sample_record);
    (module Sample_variant);
  ]
end

module Sample_dynamic_ident_int = struct
  let name = "Sample_dynamic_ident_int"
  open Bindoj_typedesc.Typed_type_desc

  let child = Coretypes.(Prims.int |> to_typed_type_decl "child")
  let child_json_shape_explanation : json_shape_explanation =
    `named ("Child", `integral)

  let samples = [
    "not integer", `num 42.1,
    (not_integer 42.1, [ ])
  ]

  let env : tdenv =
    { prim_ident_typemap = StringMap.empty;
      alias_ident_typemap =
        StringMap.of_list [ ttd_name child, Boxed child ];
    }

  module Sample_refl : Sample_dynamic_desc = struct
    let samples = samples
    let decl = Coretype.(ident (ttd_name child) |> mk) |> alias_decl "dynamic_ident_int_refl"
    let reflect =
      let open Runtime in
      let child_refl = Typed.to_refl child in
      Expr.(of_refl child_refl, to_refl child_refl)
      |> !! Reflects.reflect_of_alias

    type t = int

    let expected_json_shape_explanation =
      some @@ with_warning_named "DynamicIdentIntRefl" child_json_shape_explanation
  end

  module Sample_coretypes : Sample_dynamic_desc = struct
    let samples = samples
    let parent = Coretypes.(ident' child |> to_typed_type_decl ~env "dynamic_ident_int_coretypes")
    let decl = Typed.decl parent
    let reflect = Typed.to_refl parent

    type t = int

    let expected_json_shape_explanation =
      some @@ with_warning_named "DynamicIdentIntCoretypes" child_json_shape_explanation
  end

  module Sample_opt_coretypes : Sample_dynamic_desc = struct
    let samples = samples
    let parent = Coretypes.(ident' child |> option |> to_typed_type_decl ~env "dynamic_ident_int_opt_coretypes")
    let decl = Typed.decl parent
    let reflect = Typed.to_refl parent

    type t = int option

    let expected_json_shape_explanation =
      some @@ with_warning_named "DynamicIdentIntOptCoretypes" (`nullable child_json_shape_explanation)
  end

  let descs : (module Sample_dynamic_desc) list = [
    (module Sample_refl);
    (module Sample_coretypes);
    (module Sample_opt_coretypes);
  ]
end

module Sample_dynamic_ident_student = struct
  let name = "Sample_dynamic_ident_student"
  open Bindoj_typedesc.Typed_type_desc

  let child = Ex_record.Student.(Typed.mk decl reflect)

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
      ("ExRecordStudent",
        (`object_of
            [`mandatory_field ("admissionYear", `integral);
            `mandatory_field ("name", `string)]))

  module Sample_refl : Sample_dynamic_desc = struct
    let samples = samples
    let decl = Coretype.(ident (ttd_name child) |> mk) |> alias_decl "dynamic_ident_student_refl"
    let reflect =
      let open Runtime in
      let child_refl = Typed.to_refl child in
      Expr.(of_refl child_refl, to_refl child_refl)
      |> !! Reflects.reflect_of_alias

    type t = Ex_record.Student.t

    let expected_json_shape_explanation =
      some @@ with_warning_named "DynamicIdentStudentRefl" child_json_shape_explanation
  end

  module Sample_coretypes : Sample_dynamic_desc = struct
    let samples = samples
    let parent = Coretypes.(ident' child |> to_typed_type_decl ~env "dynamic_ident_student_coretypes")
    let decl = Typed.decl parent
    let reflect = Typed.to_refl parent
    type t = Ex_record.Student.t

    let expected_json_shape_explanation =
      some @@ with_warning_named "DynamicIdentStudentCoretypes" child_json_shape_explanation
  end

  module Sample_opt_coretypes : Sample_dynamic_desc = struct
    let samples = samples
    let parent = Coretypes.(ident' child |> option |> to_typed_type_decl ~env "dynamic_ident_student_opt_coretypes")
    let decl = Typed.decl parent
    let reflect = Typed.to_refl parent
    type t = Ex_record.Student.t option

    let expected_json_shape_explanation =
      some @@ with_warning_named "DynamicIdentStudentOptCoretypes" (`nullable child_json_shape_explanation)
  end

  let descs : (module Sample_dynamic_desc) list = [
    (module Sample_refl);
    (module Sample_coretypes);
    (module Sample_opt_coretypes);
  ]
end

let all_generated : (module Sample_generated) list = [
  (module Sample_ex_coretype);
  (module Sample_ex_alias);
  (module Sample_ex_record);
  (module Sample_ex_variant);
  (module Sample_ex_mangling);
  (module Sample_ex_optional);
  (module Sample_ex_ident);
  (module Sample_ex_nested);
  (module Sample_ex_nested_multiply);
  (module Sample_ex_version_substring);
]

let all : (module Sample_dynamic) list =
  (all_generated |&> (fun (module S : Sample_generated) ->
    (module struct
      include S
      let descs =
        S.descs |&> (fun (module D : Sample_generated_desc) ->
          (module D : Sample_dynamic_desc))
    end : Sample_dynamic))) @ [
    (module Sample_dynamic_ident_int);
    (module Sample_dynamic_ident_student);
  ]
