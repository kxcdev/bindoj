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
open Bindoj_apidir_runtime
open Bindoj_apidir_runtime.Utils
open Bindoj_apidir_shared
open Test_utils
module Samples = Bindoj_test_common.Apidir_examples
module ExD = Bindoj_test_common.Typedesc_examples
module ExG = Bindoj_test_common.Typedesc_generated_examples

module MakeTest (Dir : Samples.T) (Io : IoStyle) = struct

  module Dir = Dir

  module Io = Io

  module Bridge = Apidir_server_bridge.Make (Dir) (Io)

  module Client =
    Apidir_client.Make (Dir) (struct
        module IoStyle = Io
        let assert_empty_header, assert_empty_query_params =
          (function [] -> ()
                  | headers -> invalid_arg'
                           "headers is expected to be empty but got %a"
                           (List.pp pp_string)
                           headers),
          (function [] -> ()
                  | headers -> invalid_arg'
                           "query_params is expected to be empty but got %a"
                           (List.pp (pp_string %% (fun (k,v) -> sprintf "%s: %s" k v)))
                           headers)
        let assert_empty_header_and_query_params ~headers ~query_params =
          assert_empty_header headers; assert_empty_query_params query_params
        let perform_get ~urlpath ~headers ~query_params =
          assert_empty_header_and_query_params ~headers ~query_params;
          Bridge.handle_path_json_get urlpath
        let perform_post ~urlpath ~headers ~query_params ~body =
          assert_empty_header_and_query_params ~headers ~query_params;
          Bridge.handle_path_json_post urlpath body
      end)

  let invps, _ = Dir.registry_info ()
  let get_paths = Hashtbl.to_seq_keys Bridge.index_get |> List.of_seq
  let post_paths = Hashtbl.to_seq_keys Bridge.index_post |> List.of_seq
end

module type TestS = sig
  module Dir : Samples.T
  module Io : IoStyle
  module Bridge : Apidir_server_bridge.T with type 'resp io = 'resp Io.t
  module Client : Apidir_client.T with type 'resp io = 'resp Io.t

  val invps : invocation_point_collection
  val get_paths : string list
  val post_paths : string list
end

let all =
  Samples.all |&> fun (name, sample) ->
    let module Sample = (val sample) in
    let module Test = MakeTest (Sample) (DirectIo) in
    (name, (module Test : TestS))

let make_examples () =
  let examples = Examples.empty () in
  (* register expected value to expected and invocation point info to server bridge *)
  let () =
    List.iter begin fun (name, test) ->
      let module Test = (val test : TestS) in
      let open Test in
      List.iter begin fun (Invp invp as uinvp) ->
        match name with
        | "sample_apidir_01" ->
          begin match invp.ip_name with
            | "get-any-student" ->
              (* register a GET example *)
              let ExG.Sample_value.{ orig; jv; } = ExG.Ex01.sample_value01 in
              Examples.register_get examples
                invp.ip_urlpath uinvp
                ~orig ~jv ~pp:ExG.Ex01.pp;

              (* register POST method to server bridge *)
              Bridge.register_get_handler
                (Obj.magic invp)
                (fun () -> Io.return orig)

            | "get-student-from-person" ->
              let student_of_person = function
                (** returns ``response'' from ``request'' *)
                | ExG.Ex02.Student { student_id; name; } -> ExG.Ex01.{ admission_year = student_id; name; }
                | Anonymous -> failwith "anonymous, not student"
                | With_id _ -> failwith "with_id, not student"
                | Teacher _ -> failwith "teacher, not student" in

              (* register POST examples *)
              let ExG.Sample_value.{ orig=orig1; jv=jv1; } = ExG.Ex02.sample_value03 in
              Examples.register_post examples
                invp.ip_urlpath uinvp
                ~orig_resp:(student_of_person orig1)
                ~orig_req:orig1
                ~jv_resp:(student_of_person orig1 |> ExG.Ex01.student_to_json)
                ~jv_req:jv1
                ~pp:ExG.Ex01.pp;

              let orig2 =
                ExG.Ex02.(Student {
                    student_id = 1984;
                    name = "William Gibson";
                  }) in
              let jv2 =
                ExG.Sample_value.JvHelper.ctor_record
                  "Student" [
                  ("student_id", `num 1984.);
                  ("name", `str "William Gibson");
                ] in
              Examples.register_post examples
                invp.ip_urlpath uinvp
                ~orig_resp:(student_of_person orig2)
                ~orig_req:orig2
                ~jv_resp:(student_of_person orig2 |> ExG.Ex01.student_to_json)
                ~jv_req:jv2
                ~pp:ExG.Ex01.pp;

              (* register POST method to server bridge *)
              Bridge.register_post_handler
                (Obj.magic invp)
                (fun x -> student_of_person x |> Io.return)

            | _ -> failwith "unknown invocation point info name"
          end

        | "sample_apidir_02" ->
          begin match invp.ip_name with
            | "get-any-int-list" ->
              let ExG.Sample_value.{ orig; jv; } = ExG.Ex03_objtuple.sample_value03 in
              Examples.register_get examples
                invp.ip_urlpath uinvp
                ~orig ~jv ~pp:ExG.Ex03_objtuple.pp;
              Bridge.register_get_handler
                (Obj.magic invp)
                (fun () -> Io.return orig)

            | "inc-int-list" ->
              let rec inc_int_list = ExG.Ex03_objtuple.(function
                  | IntNil -> IntNil
                  | IntCons (n, ns) -> IntCons (n + 1, inc_int_list ns)) in

              let ExG.Sample_value.{ orig=orig1; jv=jv1; } = ExG.Ex03_objtuple.sample_value01 in
              Examples.register_post examples
                invp.ip_urlpath uinvp
                ~orig_resp:(inc_int_list orig1)
                ~orig_req:orig1
                ~jv_resp:(inc_int_list orig1 |> ExG.Ex03_objtuple.int_list_to_json)
                ~jv_req:jv1
                ~pp:ExG.Ex03_objtuple.pp;

              let ExG.Sample_value.{ orig=orig2; jv=jv2; } = ExG.Ex03_objtuple.sample_value03 in
              Examples.register_post examples
                invp.ip_urlpath uinvp
                ~orig_resp:(inc_int_list orig2)
                ~orig_req:orig2
                ~jv_resp:(inc_int_list orig2 |> ExG.Ex03_objtuple.int_list_to_json)
                ~jv_req:jv2
                ~pp:ExG.Ex03_objtuple.pp;

              Bridge.register_post_handler
                (Obj.magic invp)
                (fun x -> inc_int_list x |> Io.return)

            | "sum-of-int-list" ->
              let rec sum_of_int_list = function
                | ExG.Ex03_objtuple.IntNil -> 0
                | ExG.Ex03_objtuple.IntCons (n, ns) -> n + sum_of_int_list ns in

              let ExG.Sample_value.{ orig=orig1; jv=jv1; } = ExG.Ex03_objtuple.sample_value01 in
              Examples.register_post examples
                invp.ip_urlpath uinvp
                ~orig_resp:(sum_of_int_list orig1)
                ~orig_req:orig1
                ~jv_resp:(`num (sum_of_int_list orig1 |> float_of_int))
                ~jv_req:jv1
                ~pp:pp_int;

              let ExG.Sample_value.{ orig=orig2; jv=jv2; } = ExG.Ex03_objtuple.sample_value03 in
              Examples.register_post examples
                invp.ip_urlpath uinvp
                ~orig_resp:(sum_of_int_list orig2)
                ~orig_req:orig2
                ~jv_resp:(`num (sum_of_int_list orig2 |> float_of_int))
                ~jv_req:jv2
                ~pp:pp_int;

              Bridge.register_post_handler
                (Obj.magic invp)
                (fun x -> sum_of_int_list x |> Io.return)

            | _ -> failwith "unknown invocation point info name"
          end
        | _ -> failwith "unknown sample"
      end invps;
    end all in
  examples
let examples = make_examples ()

open Alcotest

let (^^) a b = a ^ ", " ^ b

let bridge_path_handler_cases =
  (all |&>
   fun (name, test) ->
     let module Test = (val test : TestS) in
     let open Test in
     let jv_io = testable (Io.pp Utils.pp_jv) ( = ) in
     begin get_paths |&> fun path ->
         test_case "register_get_handler, path_index_get and handle_path_json_get work"
           `Quick
           (fun () ->
              match Examples.find_get_by_path examples path with
              | None ->
                fail "No GET method example of the given path"
              | Some get ->
                check jv_io (name ^^ path ^^ "GET")
                  (Io.return get.jv)
                  (Bridge.handle_path_json_get path))
     end @ begin post_paths |&> fun path ->
         test_case "register_post_handler, path_index_post and handle_path_json_post work."
           `Quick
           (fun () ->
              match Examples.find_post_by_path examples path with
              | [] ->
                fail "No POST method example of the given path"
              | posts ->
                List.iter (fun (post : ('resp, 'req) Examples.post) ->
                    check jv_io (name ^^ path ^^ "POST")
                      (Io.return post.jv_resp)
                      (Bridge.handle_path_json_post path post.jv_req))
                  posts)
     end) |> List.concat

let bridge_handler_cases =
  (all |&>
   fun (name, test) ->
     let module Test = (val test : TestS) in
     let open Test in
     invps |&> fun (Invp invp as uinvp) ->
       test_case "register_get/post_handler and handle_json_get/post work"
         `Quick
         (fun () ->
            match invp.ip_method with
            | `get -> begin match Examples.find_get_by_invp examples uinvp with
                | None ->
                  fail "No GET method example of the given path"
                | Some get ->
                  check (testable (Io.pp pp_jv) ( = )) (name ^^ invp.ip_urlpath ^^ "GET")
                    (Io.return get.jv)
                    (Bridge.handle_json_get uinvp)
              end
            | `post -> begin match Examples.find_post_by_invp examples uinvp with
                | [] ->
                  fail "No POST method example of the given path"
                | posts ->
                  List.iter (fun (post : ('resp, 'req) Examples.post) ->
                      check (testable (Io.pp pp_jv) ( = )) (name ^^ invp.ip_urlpath ^^ "POST")
                        (Io.return post.jv_resp)
                        (Bridge.handle_json_post uinvp post.jv_req))
                    posts
              end)) |> List.concat

let client_cases =
  (all |&>
   fun (name, test) ->
     let module Test = (val test : TestS) in
     let open Test in
     begin invps |&> fun (Invp invp as uinvp) ->
         test_case "Bridge.register_get_handler and Client.perform_json_get work."
           `Quick
           (fun () ->
              match invp.ip_method with
              | `get -> begin match Examples.find_get_by_invp examples uinvp with
                  | None ->
                    fail "No GET method example of the given path"
                  | Some get ->
                    check (testable (Io.pp get.pp_get) ( = )) (name ^^ invp.ip_urlpath ^^ "GET")
                      (Io.return get.orig)
                      (Client.perform_json_get (Obj.magic invp))
                end
              | `post -> begin match Examples.find_post_by_invp examples uinvp with
                  | [] ->
                    fail "No POST method example of the given path"
                  | posts ->
                    List.iter (fun (post : ('resp, 'req) Examples.post) ->
                        check (testable (Io.pp post.pp_post) ( = )) (name ^^ invp.ip_urlpath ^^ "POST")
                          (Io.return post.orig_resp)
                          (Client.perform_json_post (Obj.magic invp) post.orig_req))
                      posts
                end)
     end) |> List.concat

let () =
  run "lib_apidir_runtime_test.ml" [
    "apidir_server_bridge", bridge_path_handler_cases @ bridge_handler_cases;
    "apidir_client", client_cases;
  ]
