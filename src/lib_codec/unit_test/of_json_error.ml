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
open Alcotest
open Kxclib
open Bindoj_base
open Bindoj_typedesc
open Bindoj_test_common.Typedesc_generated_examples
open Bindoj_test_common.Typedesc_generated_examples.Sample_value.JvHelper

module Testables : sig
  val jvpath : Json.jvpath testable
end = struct
  let jvpath = testable Json.pp_jvpath ( = )
end

module type Sample = sig
  type t
  val decl : Type_desc.type_decl
  val reflect : t Runtime.Refl.t
  val env : Typed_type_desc.Type_decl_environment.env
  val name : string
  val samples : (string * Json.jv * (string * Json.jvpath)) list
end

open struct
  let discriminator_not_found = sprintf "discriminator field '%s' does not exist"
  let field_not_found = sprintf "mandatory field '%s' does not exist"
  let not_integer = sprintf "expecting an integer but the given is '%f'"
  let type_mismatch = sprintf "expecting type '%s' but the given is of type '%s'"
  let incorrect_list_length = sprintf "expecting an array of length %d, but the given has a length of %d"
  let incorrect_tuple_length = sprintf "expecting a tuple of length %d, but the given has a length of %d"
end

module SampleEx01 : Sample = struct
  include Ex01
  let name = "SampleEx01"
  let samples = [
    "not obj", `null, ("an object is expected for a record value, but the given is of type 'null'", []);

    "missing field", `obj [ ("admission_year", `num 1984.) ]
    , (field_not_found "name", []);

    "type mismatch", `obj [ ("admission_year", `null); ("name", `str "William Gibson") ]
    , (type_mismatch "int" "null", [ `f "admission_year" ]);

    "not integer", `obj [ ("admission_year", `num 1984.5); ("name", `str "William Gibson") ]
    , (not_integer 1984.5, [ `f "admission_year" ]);
  ]
end

module SampleEx02 : Sample = struct
  include Ex02
  let name = "SampleEx02"
  let samples = [
    "missing discriminator", `obj [ ("kind", `null)],
    (discriminator_not_found "kind", []);

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

module SampleEx03 : Sample = struct
  include Ex03
  let name = "SampleEx03"
  let intCons_null = ctor2 "IntCons" `null
  let samples = [
    "missing discriminator", `obj [ ("kind", `null)],
    (discriminator_not_found "kind", []);

    "missing discriminator (nested 1)", `obj [ ("kind", `null)] |> intCons 1 ,
    (discriminator_not_found "kind", [ `i 1; `f "arg" ]);

    "missing discriminator (nested 2)", `obj [ ("kind", `null)] |> intCons 1 |> intCons 2,
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
end

module SampleEx04 : Sample = struct
  include Ex04
  let name = "SampleEx04"
  let samples = [
    "Foo2: incorrect list length", ctorN "Foo2" [ `num 1. ],
    (incorrect_list_length 2 1, [ `f "arg" ])
  ]
end

module SampleEx05 : Sample = struct
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
end

module SampleEx06 : Sample = struct
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
end

module SampleIdentInt_Coretypes : Sample = struct
  open Bindoj_typedesc.Typed_type_desc
  include SampleIdentInt_base

  let parent = Coretypes.(ident' child |> to_typed_type_decl ~env "parent")

  let decl = Typed.decl parent
  let reflect = Typed.to_refl parent

  type t = int
  let name = "SampleIdentInt_Coretypes"
end

module SampleIdentIntOption_Coretypes : Sample = struct
  open Bindoj_typedesc.Typed_type_desc
  include SampleIdentInt_base

  let parent = Coretypes.(ident' child |> option |> to_typed_type_decl ~env "parent")

  let decl = Typed.decl parent
  let reflect = Typed.to_refl parent

  type t = int option
  let name = "SampleIdentIntOption_Coretypes"
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
end

module SampleIdentStudent_Coretypes : Sample = struct
  open Bindoj_typedesc.Typed_type_desc
  include SampleIdentStudent_base

  let parent = Coretypes.(ident' child |> to_typed_type_decl ~env "parent")

  let decl = Typed.decl parent
  let reflect = Typed.to_refl parent

  type t = Ex01.t
  let name = "SampleIdentStudent_Coretypes"
end

module SampleIdentStudentOption_Coretypes : Sample = struct
  open Bindoj_typedesc.Typed_type_desc
  include SampleIdentStudent_base

  let parent = Coretypes.(ident' child |> option |> to_typed_type_decl ~env "parent")

  let decl = Typed.decl parent
  let reflect = Typed.to_refl parent

  type t = Ex01.t option
  let name = "SampleIdentStudentOption_Coretypes"
end

let create_test_cases (module S : Sample) =
  let typed_decl = Typed_type_desc.Typed.mk S.decl S.reflect in
  S.name, (S.samples |&> (fun (name, jv, (msg, path)) ->
    let msg =
      if List.empty path then sprintf "%s at root" msg
      else sprintf "%s at path %s" msg (Kxclib.Json.unparse_jvpath path)
    in
    test_case name `Quick(fun () ->
      let res_msg, res_path =
        Bindoj_codec.Json.of_json' ~env:S.env typed_decl jv
        |> function
        | Error (msg, path, _) -> Some (msg), Some(path)
        | _ -> None, None
      in
      check (option string) "error message" (Some msg) res_msg;
      check (option Testables.jvpath) "error jvpath" (Some path) res_path
    )))

let () =
  ([(module SampleEx01);
    (module SampleEx02);
    (module SampleEx03);
    (module SampleEx04);
    (module SampleEx05);
    (module SampleEx06);
    (module SampleIdentInt_Refl);
    (module SampleIdentInt_Coretypes);
    (module SampleIdentIntOption_Coretypes);
    (module SampleIdentStudent_Refl);
    (module SampleIdentStudent_Coretypes);
    (module SampleIdentStudentOption_Coretypes);
  ] : (module Sample) list)
  |&> create_test_cases
  |> Alcotest.run "lib_codec.of_json'"
