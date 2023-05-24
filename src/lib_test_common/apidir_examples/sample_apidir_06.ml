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
open Bindoj_typedesc
open Bindoj_typedesc.Typed_type_desc
open Bindoj_test_common_typedesc_generated_examples

module Types = struct
  open Bindoj_runtime

  let xy_opt : Ex10.xy_opt typed_type_decl =
    Typed.mk Ex10.decl Ex10.reflect

  let int_opt : int option typed_type_decl =
    Coretypes.(Prims.int |> option |> to_typed_type_decl "int_opt")

  let int_opt_to_json : int option -> Json.jv = function
    | None -> `null
    | Some n -> `num (float_of_int n)
end

open struct
  module R = MakeRegistry()
  module T = Types
end

let () = begin
  R.register_type_decl_info T.int_opt;
  R.register_type_decl_info T.xy_opt;
end

let get_x =
  R.register_post "get-x"
    ~urlpath:"/xy-opt/x"
    ~req_type:T.xy_opt
    ~resp_type:T.int_opt

let get_y =
  R.register_post "get-y"
    ~urlpath:"/xy-opt/y"
    ~req_type:T.xy_opt
    ~resp_type:T.int_opt

let () = begin
  get_x
  |> R.register_usage_samples
    ( Ex10.sample_values
      |&> (fun { orig; _ } -> (orig, orig.x_opt, `default), `nodoc));

  get_y
  |> R.register_usage_samples
    ( Ex10.sample_values
      |&> (fun { orig; _ } -> (orig, orig.y_opt, `default), `nodoc));
end

include R.Public

open Alcotest
open Utils

let test_individual_invocation_points () = begin
  check_invp "get-x" get_x
    ~ip_name:"get-x"
    ~ip_urlpath:"/xy-opt/x"
    ~ip_method:`post;
  check_invp "get-y" get_y
    ~ip_name:"get-y"
    ~ip_urlpath:"/xy-opt/y"
    ~ip_method:`post;
end

let test_invocation_point_collection () = begin
  let invps, _ =  registry_info () in
  check (list string) "registry_info has all invp listed"
    (List.sort compare ["get-x"; "get-y"])
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
  let open Sample_value in

  let () (* get-x *) =
    let invp = get_x in
    let reg_sample { orig : Ex10.t; jv } =
      M.register_post_example invp.ip_urlpath (Invp invp)
        ~orig_req:orig ~orig_resp:orig.x_opt
        ~jv_req:jv ~jv_resp:(orig.x_opt |> T.int_opt_to_json)
        ~pp:(Option.pp pp_int) in
    let open Bindoj_test_common_typedesc_generated_examples in
    List.iter reg_sample Ex10.sample_values;
    M.register_post_handler invp (fun (t : Ex10.t) -> return (200, t.x_opt )) in

  let () (* get-y *) =
    let invp = get_y in
    let reg_sample { orig : Ex10.t; jv } =
      M.register_post_example invp.ip_urlpath (Invp invp)
        ~orig_req:orig ~orig_resp:orig.y_opt
        ~jv_req:jv ~jv_resp:(orig.y_opt |> T.int_opt_to_json)
        ~pp:(Option.pp pp_int) in
    let open Bindoj_test_common_typedesc_generated_examples in
    List.iter reg_sample Ex10.sample_values;
    M.register_post_handler invp (fun (t : Ex10.t) -> return (200, t.y_opt)) in
  ()
