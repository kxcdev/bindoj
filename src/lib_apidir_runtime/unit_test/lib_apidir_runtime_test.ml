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
      module Response = Bindoj_apidir_runtime.Apidir_server_bridge.TupleJsonResponse

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
    List.iter begin fun (_, (module Test : TestS)) ->
      let open Test in
      let builder : (module Samples.MockServerBuilder) = (module struct
        module Io = Io
        let register_get_handler = Bridge.register_get_handler
        let register_post_handler = Bridge.register_post_handler

        let register_get_example path uinvp ~orig ~jv ~pp =
          Examples.register_get examples path uinvp ~orig ~jv ~pp

        let register_post_example path uinvp ~orig_resp ~orig_req ~jv_resp ~jv_req ~pp =
          Examples.register_post examples path uinvp ~orig_resp ~orig_req ~jv_resp ~jv_req ~pp
      end) in
      Dir.build_mock_server builder
    end all in
  examples
let examples = make_examples ()

open Alcotest

let (^^) a b = a ^ ", " ^ b

module MakeJvIo(Io : IoStyle) = struct
  let jv_io =
    let open Kxclib in
    let id_counter = ref 0 in
    testable (fun ppf mx ->
        let id = get_and_incr id_counter in
        let open MonadOps(Io) in
        (mx >>= fun x ->
         sprintf "(async:jv.%d) %a"
           id Utils.pp_jv x
         |> return)
        |> Io.trace;
        fprintf ppf "[async:jv.%d]" id
      ) (=)
end


let bridge_path_handler_cases =
  (all |&>
   fun (name, test) ->
     let module Test = (val test : TestS) in
     let open Test in
     let open MonadOps(Io) in
     let open MakeJvIo(Io) in
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
                  (Bridge.handle_path_json_get path >|= snd))
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
                      (Bridge.handle_path_json_post path post.jv_req >|= snd))
                  posts)
     end) |> List.concat

let bridge_handler_cases =
  (all |&>
   fun (name, test) ->
     let module Test = (val test : TestS) in
     let open Test in
     let open MonadOps(Io) in
     let open MakeJvIo(Io) in
     invps |&> fun (Invp invp as uinvp) ->
       test_case "register_get/post_handler and handle_json_get/post work"
         `Quick
         (fun () ->
            match invp.ip_method with
            | `get -> begin match Examples.find_get_by_invp examples uinvp with
                | None ->
                  fail "No GET method example of the given path"
                | Some get ->
                  check jv_io (name ^^ invp.ip_urlpath ^^ "GET")
                    (Io.return get.jv)
                    (Bridge.handle_json_get uinvp >|= snd)
              end
            | `post -> begin match Examples.find_post_by_invp examples uinvp with
                | [] ->
                  fail "No POST method example of the given path"
                | posts ->
                  List.iter (fun (post : ('resp, 'req) Examples.post) ->
                      check jv_io (name ^^ invp.ip_urlpath ^^ "POST")
                        (Io.return post.jv_resp)
                        (Bridge.handle_json_post uinvp post.jv_req >|= snd))
                    posts
              end)) |> List.concat

let client_cases =
  (all |&>
   fun (name, test) ->
     let module Test = (val test : TestS) in
     let open Test in
     begin invps |&> fun (Invp invp as uinvp) ->
         let open MakeJvIo(Io) in
         test_case "Bridge.register_get_handler and Client.perform_json_get work."
           `Quick
           (fun () ->
              match invp.ip_method with
              | `get -> begin match Examples.find_get_by_invp examples uinvp with
                  | None ->
                    fail "No GET method example of the given path"
                  | Some get ->
                    check jv_io (name ^^ invp.ip_urlpath ^^ "GET")
                      (Io.return get.orig)
                      (Client.perform_json_get (Obj.magic invp))
                end
              | `post -> begin match Examples.find_post_by_invp examples uinvp with
                  | [] ->
                    fail "No POST method example of the given path"
                  | posts ->
                    List.iter (fun (post : ('resp, 'req) Examples.post) ->
                        check jv_io (name ^^ invp.ip_urlpath ^^ "POST")
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
