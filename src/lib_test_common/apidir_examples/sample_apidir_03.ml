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

  type person = Ex02.person
  let person : person typed_type_decl = Typed.mk Ex02.decl Ex02.reflect

  let int : int typed_type_decl =
    let decl = alias_decl "int" (Coretype.mk_prim `int) in
    let reflect = Reflects.int_reflect in
    Typed.mk decl reflect

  let string : string typed_type_decl =
    let decl = alias_decl "string" (Coretype.mk_prim `string) in
    let reflect = Reflects.string_reflect in
    Typed.mk decl reflect
end

open struct
  module R = MakeRegistry()
  module T = Types
end

let id_of_person =
  R.register_post' "id-of-person"
    ~urlpath:"/person/id"
    ~req_type:T.person
    ~req_doc:"the person record to get the id"
    [ make_response_case ~status:(`status_code 200)
        ~name:"id"
        ~doc:"The ID of the given person."
        ~pack:(fun n -> `Ok n) ~unpack:(function `Ok x -> Some x | `Error _ -> None)
        T.int;
      make_response_case ~status:(`status_range `_4XX)
        ~name:"error_message"
        ~doc:"The reason why the ID of the given person is unavailable."
        ~pack:(fun s -> `Error s) ~unpack:(function `Error x -> Some x | `Ok _ -> None)
        T.string; ]

include R.Public

open Alcotest
open Utils

let test_individual_invocation_points () = begin
  check_invp "id_of_person" id_of_person
    ~ip_name:"id-of-person"
    ~ip_urlpath:"/person/id"
    ~ip_method:`post;
end

let test_invocation_point_collection () = begin
  let invps, _ =  registry_info () in
  check (list string) "registry_info has all invp listed"
    (List.sort compare ["id-of-person"])
    (List.sort compare (invps |&> (fun (Invp invp) -> invp.ip_name)))
end

let tests =  [
  test_case "individual_invocation_points" `Quick
    test_individual_invocation_points;
  test_case "invocation_point_collection" `Quick
    test_invocation_point_collection;
]

let build_mock_server (module M: MockServerBuilder) =
  let open M.Io in
  let open Bindoj_test_common_typedesc_generated_examples in
  let open Sample_value in

  let () (* id-of-person *) =
    let invp = id_of_person in

    let anonymousError = "Cannot get the ID of an anonymous person." in
    let teacherError = "This teacher does not have a personal ID." in

    let id_of_person = Ex02.(function
      | Anonymous -> (403, `Error anonymousError)
      | With_id id -> (200, `Ok id)
      | Student { student_id; _ } -> (200, `Ok student_id)
      | Teacher _ -> (404, `Error teacherError)) in

    let pp_result ppf = function
      | `Ok i -> pp_int ppf i
      | `Error s -> pp_string ppf s in

    let reg_sample { orig; jv } =
      M.register_post_example invp.ip_urlpath (Invp invp)
        ~orig_resp:(id_of_person orig |> snd) ~orig_req:orig
        ~jv_resp:(match id_of_person orig |> snd with
          | `Ok i -> `num (float_of_int i)
          | `Error s -> `str s)
        ~jv_req:jv
        ~pp:pp_result in

    Ex02.sample_values |> List.iter reg_sample;
    M.register_post_handler invp (fun x -> return (id_of_person x)) in
  ()
