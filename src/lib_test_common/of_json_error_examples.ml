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
  val expected_json_shape_explanation : json_shape_explanation
end

module type SampleGenerated = sig
  include Sample
  val json_shape_explanation : json_shape_explanation
  val of_json' : Json.jv -> t OfJsonResult.t
  val pp : ppf -> t -> unit
end

open struct
  let record_not_obj = sprintf "an object is expected for a record value, but the given is of type '%s'"
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

    "missing field", `obj [ ("admission_year", `num 1984.) ]
    , (field_not_found "name", []);

    "type mismatch", `obj [ ("admission_year", `null); ("name", `str "William Gibson") ]
    , (type_mismatch "int" "null", [ `f "admission_year" ]);

    "not integer", `obj [ ("admission_year", `num 1984.5); ("name", `str "William Gibson") ]
    , (not_integer 1984.5, [ `f "admission_year" ]);
  ]

  let expected_json_shape_explanation =
    `with_warning
      ("not considering any config if exists",
        (`named
          ("student",
            (`object_of
                [`mandatory_field ("admission_year", `integral);
                `mandatory_field ("name", `string)]))))
end

module SampleEx02 : SampleGenerated = struct
  include Ex02
  let name = "SampleEx02"
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

  let expected_json_shape_explanation =
    `with_warning
      ("not considering any config if exists",
        (`named
          ("person",
            (`anyone_of
                [`object_of
                  [`mandatory_field ("kind", (`exactly (`str "Anonymous")))];
                `object_of
                  [`mandatory_field ("kind", (`exactly (`str "With_id")));
                  `mandatory_field ("arg", (`tuple_of [`integral]))];
                `object_of
                  [`mandatory_field ("kind", (`exactly (`str "Student")));
                  `mandatory_field ("student_id", `integral);
                  `mandatory_field ("name", `string)];
                `object_of
                  [`mandatory_field ("kind", (`exactly (`str "Teacher")));
                  `mandatory_field ("faculty_id", `integral);
                  `mandatory_field ("name", `string);
                  `mandatory_field ("department", `string)]]))))
end

module SampleEx03 : SampleGenerated = struct
  include Ex03
  let name = "SampleEx03"
  let intCons_null = ctor2 "IntCons" `null
  let samples = [
    "missing discriminator", `obj [ ],
    (discriminator_not_found "kind", []);

    "discriminator not string", `obj [ ("kind", `null)],
    (discriminator_not_string "null", [ `f "kind" ]);

    "missing discriminator (nested 1)", `obj [ ] |> intCons 1 ,
    (discriminator_not_found "kind", [ `i 1; `f "arg" ]);

    "missing discriminator (nested 2)", `obj [ ] |> intCons 1 |> intCons 2,
    (discriminator_not_found "kind", [ `i 1; `f "arg"; `i 1; `f "arg" ]);

    "missing field", ctor0 "IntCons",
    (field_not_found "arg", []);

    "missing field (nested 1)", ctor0 "IntCons" |> intCons 1 ,
    (field_not_found "arg", [ `i 1; `f "arg" ]);

    "missing field (nested 2)", ctor0 "IntCons" |> intCons 1 |> intCons 2,
    (field_not_found "arg", [ `i 1; `f "arg"; `i 1; `f "arg" ]);

    "type mismatch", intCons_null intNil,
    (type_mismatch "int""null" , [ `i 0; `f "arg" ]);

    "type mismatch (nested 1)", intCons_null intNil |> intCons 1 ,
    (type_mismatch "int" "null", [ `i 0; `f "arg"; `i 1; `f "arg" ]);

    "type mismatch (nested 2)", intCons_null intNil |> intCons 1 |> intCons 2,
    (type_mismatch "int" "null", [ `i 0; `f "arg"; `i 1; `f "arg"; `i 1; `f "arg" ]);
  ]

  let expected_json_shape_explanation =
    `with_warning
      ("not considering any config if exists",
        (`named
          ("int_list",
            (`anyone_of
                [`object_of
                  [`mandatory_field ("kind", (`exactly (`str "IntNil")))];
                `object_of
                  [`mandatory_field ("kind", (`exactly (`str "IntCons")));
                  `mandatory_field ("arg", (`tuple_of [`integral; `self]))]]))))
end

module SampleEx04 : SampleGenerated = struct
  include Ex04
  let name = "SampleEx04"
  let samples = [
    "Foo2: incorrect list length", ctorN "Foo2" [ `num 1. ],
    (incorrect_list_length 2 1, [ `f "arg" ])
  ]

  let expected_json_shape_explanation =
    `with_warning
      ("not considering any config if exists",
        (`named
          ("foo",
            (`anyone_of
                [`object_of
                  [`mandatory_field ("kind", (`exactly (`str "Foo0")))];
                `object_of
                  [`mandatory_field ("kind", (`exactly (`str "Foo1")));
                  `mandatory_field ("arg", (`tuple_of [`integral]))];
                `object_of
                  [`mandatory_field ("kind", (`exactly (`str "Foo2")));
                  `mandatory_field ("arg", (`tuple_of [`integral; `integral]))]]))))
end

module SampleEx05 : SampleGenerated = struct
  include Ex05
  let name = "SampleEx05"
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
    ("an object is expected for a tuple value, but the given is of type 'array'", [ `f "objtuple" ]);

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

  let expected_json_shape_explanation =
    `with_warning
      ("not considering any config if exists",
        (`named
          ("complex_types",
            (`object_of
                [`optional_field ("option", `integral);
                `mandatory_field ("list", (`array_of `integral));
                `mandatory_field ("tuple", (`tuple_of [`integral; `integral]));
                `mandatory_field
                  ("objtuple", (`tuple_of [`integral; `integral]));
                `mandatory_field
                  ("nested",
                    (`tuple_of
                      [`nullable `integral;
                      `array_of `integral;
                      `tuple_of [`integral; `integral]]));
                `mandatory_field ("map", (`record_of `integral))]))))
end

module SampleEx06 : SampleGenerated = struct
  include Ex06
  let name = "SampleEx06"
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

  let expected_json_shape_explanation =
    `with_warning
      ("not considering any config if exists",
        (`named
          ("various_prim_types",
            (`object_of
                [`mandatory_field
                  ("unit", (`special ("unit", (`exactly `null))));
                `mandatory_field ("bool", `boolean);
                `mandatory_field ("int", `integral);
                `mandatory_field ("float", `proper_float);
                `mandatory_field ("string", `string);
                `mandatory_field ("uchar", (`special ("uchar", `string)));
                `mandatory_field ("byte", (`special ("byte", `string)));
                `mandatory_field ("bytes", `base64str)]))))
end

module SampleEx07 : SampleGenerated = struct
  include Ex07
  let name = "SampleEx07"
  let samples = [
    "missing discriminator", `obj [ ],
    (discriminator_not_found "tag", []);

    "discriminator not string", `obj [ ("tag", `null)],
    (discriminator_not_string "null", [ `f "tag" ]);

    "missing field", ctor0 ~discriminator:"tag" "Case1_",
    (field_not_found "value", []);

    "Case1: not integer", ctor_record ~discriminator:"tag" "Case1_" [ ("value", `num 1.2) ],
    (not_integer 1.2, [ `f "value" ]);

    "missing field", ctor0 ~discriminator:"tag" "Case2_",
    (field_not_found "x_", []);

    "missing field", ctor_record ~discriminator:"tag" "Case2_" [ "x_", `num 4. ],
    (field_not_found "y_", [ ]);

    "Case2: not integer", ctor_record ~discriminator:"tag" "Case2_" [ "x_", `str "fooBar"; "y_", `num 2. ],
    (type_mismatch "int" "string", [ `f "x_" ]);

    "Case2: not integer", ctor_record ~discriminator:"tag" "Case2_" [ "x_", `num 4.; "y_", `num 2.1 ],
    (not_integer 2.1, [ `f "y_" ]);
  ]

  let expected_json_shape_explanation =
    `with_warning
      ("not considering any config if exists",
        (`named
          ("customized_union",
            (`anyone_of
                [`object_of
                  [`mandatory_field ("kind", (`exactly (`str "Case1")));
                  `mandatory_field ("arg", (`tuple_of [`integral]))];
                `object_of
                  [`mandatory_field ("kind", (`exactly (`str "Case2")));
                  `mandatory_field ("x", `integral);
                  `mandatory_field ("y", `integral)]]))))
end

module SampleEx08 : SampleGenerated = struct
  include Ex08
  let name = "SampleEx08"
  let samples = []

  let expected_json_shape_explanation =
    `with_warning
      ("not considering any config if exists",
        (`named
          ("named_json",
            (`object_of
              [`mandatory_field ("name", `string);
              `mandatory_field
                ("json",
                  (`named ("json_value", `any_json_value)))]))))
end

module SampleEx09 : SampleGenerated = struct
  include Ex09
  let name = "SampleEx09"
  let samples = [
    "missing field", `obj [],
    (field_not_found "value", []);

    "missing field", `obj [ "value", `null ],
    (type_mismatch "int53p" "null", [ `f "value" ]);
  ]

  let expected_json_shape_explanation =
    `with_warning
      ("not considering any config if exists",
        (`named
          ("with_int53p",
            (`object_of [`mandatory_field ("value", `proper_int53p)]))))
end

module SampleEx10 : SampleGenerated = struct
  include Ex10
  let name = "SampleEx10"
  let samples = [
    "not obj", `null, (record_not_obj "null", []);

    "type mismatch", `obj [ "y_opt", `str "fooBar"],
    (type_mismatch "int" "string", [ `f "y_opt" ])
  ]

  let expected_json_shape_explanation =
    `with_warning
      ("not considering any config if exists",
        (`named
          ("xy_opt",
            (`object_of
                [`optional_field ("x_opt", `integral);
                `optional_field ("y_opt", `integral)]))))
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
    `named ("child", `integral)
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
    `with_warning
      ("not considering any config if exists",
        (`named
          ("parent", child_json_shape_explanation)))
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
    `with_warning
      ("not considering any config if exists",
        (`named
          ("parent", child_json_shape_explanation)))
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
    `with_warning
      ("not considering any config if exists",
        (`named
          ("parent", `nullable child_json_shape_explanation)))
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
    "missing field", `obj [ ("admission_year", `num 1984.) ]
    , (field_not_found "name", []);
  ]

  let child_json_shape_explanation =
    `named
      ("student",
        (`object_of
            [`mandatory_field ("admission_year", `integral);
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
    `with_warning
      ("not considering any config if exists",
        (`named
          ("parent", child_json_shape_explanation)))
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
    `with_warning
      ("not considering any config if exists",
        (`named
          ("parent", child_json_shape_explanation)))
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
    `with_warning
      ("not considering any config if exists",
        (`named
          ("parent", `nullable child_json_shape_explanation)))
end

let all_generated : (module SampleGenerated) list = [
  (module SampleEx01);
  (module SampleEx02);
  (module SampleEx03);
  (module SampleEx04);
  (module SampleEx05);
  (module SampleEx06);
  (module SampleEx07);
  (module SampleEx08);
  (module SampleEx09);
  (module SampleEx10);
]

let all : (module Sample) list = [
  (module SampleEx01);
  (module SampleEx02);
  (module SampleEx03);
  (module SampleEx04);
  (module SampleEx05);
  (module SampleEx06);
  (module SampleEx07);
  (module SampleEx08);
  (module SampleEx09);
  (module SampleEx10);
  (module SampleIdentInt_Refl);
  (module SampleIdentInt_Coretypes);
  (module SampleIdentIntOption_Coretypes);
  (module SampleIdentStudent_Refl);
  (module SampleIdentStudent_Coretypes);
  (module SampleIdentStudentOption_Coretypes);
]
