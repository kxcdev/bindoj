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

module Types = struct
  open Bindoj_test_common_typedesc_generated_examples
  open Bindoj_runtime

  type int_list = Ex03_objtuple.int_list
  let int_list : int_list typed_type_decl = Typed.mk Ex03_objtuple.decl Ex03_objtuple.reflect

  let int : int typed_type_decl =
    let decl = alias_decl "int" (Coretype.mk_prim `int) in
    let reflect = Reflects.int_reflect in
    Typed.mk decl reflect
end

open struct
  module R = MakeRegistry()
  module T = Types
end

let get_any_int_list =
  R.register_get "get-any-int-list"
    ~urlpath:"/int-list/any-one"
    ~resp_type:T.int_list
    ~resp_name:"int-list"
    ~resp_doc:"a int-list record (could be anyone) in the database"

let inc_int_list =
  R.register_post "inc-int-list"
    ~urlpath:"/int-list/inc"
    ~req_type:T.int_list
    ~req_name:"int-list"
    ~req_doc:"an int list"
    ~resp_type:T.int_list
    ~resp_name:"int-list"
    ~resp_doc:"an int list with all elements of the supplied int list inclimented"

let sum_of_int_list =
  R.register_post "sum-of-int-list"
    ~urlpath:"/int-list/sum"
    ~req_type:T.int_list
    ~req_name:"int-list"
    ~req_doc:"an int-list"
    ~resp_type:T.int
    ~resp_name:"int"
    ~resp_doc:"sum of the supplied int list"

include R.Public

open Alcotest
open Utils

let test_individual_invocation_points() = begin
  check_invp "get_any_int_list" get_any_int_list
    ~ip_name:"get-any-int-list"
    ~ip_urlpath:"/int-list/any-one"
    ~ip_method:`get;

  check_invp "inc_int_list" inc_int_list
    ~ip_name:"inc-int-list"
    ~ip_urlpath:"/int-list/inc"
    ~ip_method:`post;

  check_invp "sum_of_int_list" sum_of_int_list
    ~ip_name:"sum-of-int-list"
    ~ip_urlpath:"/int-list/sum"
    ~ip_method:`post
end

let test_invocation_point_collection() = begin
    let invps, _ =  registry_info() in
    check (list string) "registry_info has all invp listed"
      (List.sort compare
         ["get-any-int-list";
          "inc-int-list";
          "sum-of-int-list"])
      (List.sort compare (invps |&> (fun (Invp invp) -> invp.ip_name)))
  end

let tests =  [
  test_case "individual_invocation_points" `Quick
    test_individual_invocation_points;
  test_case "invocation_point_collection" `Quick
    test_invocation_point_collection;
]
