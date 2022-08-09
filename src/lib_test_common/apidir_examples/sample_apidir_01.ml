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
[@@@warning "-33-32"]

open Bindoj_apidir_shared
open Bindoj_typedesc.Typed_type_desc
open Utils

module Types = struct
  open Bindoj_test_common_typedesc_generated_examples

  type student = Ex01.student
  let student : student typed_type_decl = Typed.mk Ex01.decl Ex01.reflect

  type person = Ex02.person
  let person : person typed_type_decl = Typed.mk Ex02.decl Ex02.reflect
end

open struct
  module R = MakeRegistry()
  module T = Types
end

let get_any_student =
  R.register_get "get-any-student"
    ~urlpath:"/student/any-one"
    ~resp_type:T.student
    ~resp_name:"student"
    ~resp_doc:"a student record (could be anyone) in the database"

let get_student_from_person =
  R.register_post "get-student-from-person"
    ~urlpath:"/student/from-person"
    ~req_type:T.person
    ~req_name:"person"
    ~req_doc:"a person record identifying a student"
    ~resp_type:T.student
    ~resp_name:"student"
    ~resp_doc:"the student record corresponding to the supplied person"

include R.Public
