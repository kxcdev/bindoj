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
open Bindoj_base.Typed_type_desc
open Bindoj_example_shared_typedesc_generated.Typedesc_generated

include Bindoj_example_ex01_generated.Ex01
include Bindoj_example_ex01_typedesc.Typedesc

let my_int = Typed.mk my_int_decl my_int_reflect
let my_tuple = Typed.mk my_tuple_decl my_tuple_reflect
let student = Typed.mk student_decl student_reflect
let person = Typed.mk person_decl person_reflect

let env =
  let open Bindoj_typedesc.Typed_type_desc in
  { Type_decl_environment.empty with
    alias_ident_typemap =
      StringMap.of_list [
        "my_int", (Boxed my_int);
        "my_tuple", (Boxed my_tuple);
        "student", (Boxed student);
        "person", (Boxed person);
      ]}

module MyInt : T with type t = my_int = struct
  type t = int [@@deriving show]

  let decl = my_int_decl
  let reflect = my_int_reflect
  let json_shape_explanation = my_int_json_shape_explanation
  let to_json = my_int_to_json
  let of_json' = my_int_of_json'
end

module MyTuple : T with type t = my_tuple = struct
  type t = float * string [@@deriving show]

  let decl = my_tuple_decl
  let reflect = my_tuple_reflect
  let json_shape_explanation = my_tuple_json_shape_explanation
  let to_json = my_tuple_to_json
  let of_json' = my_tuple_of_json'
end

module Student : T with type t = student = struct
  type t = student = {
    admission_year: int;
    name: string;
  } [@@deriving show]

  let decl = student_decl
  let reflect = student_reflect
  let json_shape_explanation = student_json_shape_explanation
  let to_json = student_to_json
  let of_json' = student_of_json'
end

module Person : T with type t = person = struct
  type t = person =
    | Anonymous
    | With_id of int
    | Student of {
      student_id: int;
      name: string;
    }
    | Teacher of {
      faculty_id: int;
      name: string;
      department: string;
    } [@@deriving show]

  let decl = person_decl
  let reflect = person_reflect
  let json_shape_explanation = person_json_shape_explanation
  let to_json = person_to_json
  let of_json' = person_of_json'
end
