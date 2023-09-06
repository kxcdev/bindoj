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
[@@@warning "-33-32"]

open Utils
open Bindoj_apidir_shared
open Bindoj_typedesc
open Bindoj_typedesc.Typed_type_desc
open Bindoj_test_common_typedesc_generated_examples

module Types = struct
  open Bindoj_runtime

  let string : string typed_type_decl =
    Coretypes.(Prims.string |> to_typed_type_decl "string")

  let int_opt : int option typed_type_decl =
    Coretypes.(Prims.int |> option |> to_typed_type_decl "int_opt")

  let int_opt_to_json : int option -> Json.jv = function
    | None -> `null
    | Some n -> `num (float_of_int n)

  let complex : Ex05_notuple.t typed_type_decl =
    Typed.mk Ex05_notuple.decl Ex05_notuple.reflect
end

open struct
  module R = MakeRegistry()
  module T = Types
end

let () = begin
  R.register_type_decl_info T.int_opt;
  R.register_type_decl_info T.complex;
end

let int_of_string =
  R.register_post "int-of-string"
    ~urlpath:"/option/int-of-string"
    ~req_type:T.string
    ~resp_type:T.int_opt

let option_of_complex =
  R.register_post "option-of-complex"
    ~urlpath:"/option/of-complex"
    ~req_type:T.complex
    ~resp_type:T.int_opt

let () = begin
  int_of_string
  |-> R.register_usage_samples
    ( [ ""; "0"; "2020"; "a"; "11.2" ]
      |&> (fun s -> (s, int_of_string_opt s, `default), `nodoc))
  |-> R.register_request_sample "42"
  |> ignore;

  option_of_complex
  |> R.register_usage_samples
    ( Ex05_notuple.sample_values
      |&> (fun { orig; _ } -> (orig, orig.option, `default), `nodoc));
end

include R.Public

open Alcotest

let test_individual_invocation_points () = begin
  check_invp "int-of-string" int_of_string
    ~ip_name:"int-of-string"
    ~ip_urlpath:"/option/int-of-string"
    ~ip_method:`post;
  check_invp "option-of-complex" option_of_complex
    ~ip_name:"option-of-complex"
    ~ip_urlpath:"/option/of-complex"
    ~ip_method:`post;
end

let test_invocation_point_collection () = begin
  let invps, _ =  registry_info () in
  check (list string) "registry_info has all invp listed"
    (List.sort compare ["int-of-string"; "option-of-complex"])
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

  let () (* int-of-string *) =
    let invp = int_of_string in
    let reg_sample { orig; jv } =
      M.register_post_example invp.ip_urlpath (Invp invp)
        ~orig_req:orig ~orig_resp:(int_of_string_opt orig)
        ~jv_req:jv ~jv_resp:(int_of_string_opt orig |> T.int_opt_to_json)
        ~pp:(Option.pp pp_int) in
    let open Bindoj_test_common_typedesc_generated_examples in
    List.iter reg_sample
      Sample_value.[
        { orig = ""; jv = `str ""; };
        { orig = "0"; jv = `str "0"; };
        { orig = "2020"; jv = `str "2020"; };
        { orig = "a"; jv = `str "a"; };
        { orig = "11.2"; jv = `str "11.2"; };
      ];
    M.register_post_handler invp (fun x -> return (200, int_of_string_opt x)) in

  let () (* option-of-complex *) =
    let invp = option_of_complex in
    let reg_sample { orig; jv } =
      M.register_post_example invp.ip_urlpath (Invp invp)
        ~orig_req:orig ~orig_resp:Ex05_notuple.(orig.option)
        ~jv_req:jv ~jv_resp:(Ex05_notuple.(orig.option) |> T.int_opt_to_json)
        ~pp:(Option.pp pp_int) in
    List.iter reg_sample Ex05_notuple.sample_values;
    M.register_post_handler invp (fun x -> return (200, Ex05_notuple.(x.option))) in
  ()
