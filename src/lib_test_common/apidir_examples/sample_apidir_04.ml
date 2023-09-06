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

  let named_json : Ex08.named_json typed_type_decl =
    Typed.mk Ex08.decl Ex08.reflect

  let json = Bindoj_std.Json_value.Type_decl.typed

  let string : string typed_type_decl =
    Coretypes.(Prims.string |> to_typed_type_decl "string")
end

open struct
  module R = MakeRegistry()
  module T = Types
end

let () =
  R.add_type_decl_environment_wrapper Bindoj_std.Tdenv_wrappers.json;
  R.register_type_decl_info Types.named_json;
  R.register_type_decl_info Types.string

let name_of_named_json =
  R.register_post "name-of-named_json"
    ~urlpath:"/named_json/name"
    ~req_type:T.named_json
    ~resp_type:T.string

let json_of_named_json =
  R.register_post "json-of-named_json"
    ~urlpath:"/named_json/json"
    ~req_type:T.named_json
    ~resp_type:T.json

let () = begin
  name_of_named_json
  |> R.register_usage_samples
    ( Ex08.sample_values
      |&> (fun { orig; _ } -> (orig, orig.name, `default), `nodoc));

  json_of_named_json
  |> R.register_usage_samples
    ( Ex08.sample_values
      |&> (fun { orig; _ } -> (orig, orig.json, `default), `nodoc));
end

include R.Public

open Alcotest

let test_individual_invocation_points () = begin
  check_invp "name_of_named_json" name_of_named_json
    ~ip_name:"name-of-named_json"
    ~ip_urlpath:"/named_json/name"
    ~ip_method:`post;
  check_invp "json_of_named_json" json_of_named_json
    ~ip_name:"json-of-named_json"
    ~ip_urlpath:"/named_json/json"
    ~ip_method:`post
end

let test_invocation_point_collection () = begin
  let invps, _ =  registry_info () in
  check (list string) "registry_info has all invp listed"
    (List.sort compare ["name-of-named_json"; "json-of-named_json"])
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

  let () (* name-of-named_json*) =
    let invp = name_of_named_json in
    let name_of_named_json Ex08.{ name; _ } = name in
    let reg_sample { orig; jv } =
      M.register_post_example invp.ip_urlpath (Invp invp)
        ~orig_req:orig ~orig_resp:(name_of_named_json orig)
        ~jv_req:jv ~jv_resp:(name_of_named_json orig |> fun x -> `str x)
        ~pp:pp_string in
    List.iter reg_sample Ex08.sample_values;
    M.register_post_handler invp (fun x -> return (200, name_of_named_json x)) in

  let () (* json-of-named_json *) =
    let invp = json_of_named_json in
    let json_of_named_json Ex08.{ json; _ } = json in
    let rec pp_json ppf = function
      | `null -> Format.fprintf ppf "null"
      | `bool b -> Format.fprintf ppf "%B" b
      | `num f -> Format.fprintf ppf "%f" f
      | `str s -> Format.fprintf ppf "%s" s
      | `arr vs ->
        List.pp ~sep:", " ~parens:("[", "]") pp_json ppf vs
      | `obj kvs ->
        List.pp ~sep:", " ~parens:("{", "}")
          (fun ppf (k, v) -> Format.fprintf ppf "%s: %a" k pp_json v)
          ppf kvs
    in
    let reg_sample { orig; jv } =
      M.register_post_example invp.ip_urlpath (Invp invp)
        ~orig_req:orig ~orig_resp:(json_of_named_json orig)
        ~jv_req:jv ~jv_resp:(json_of_named_json orig)
        ~pp:pp_json in
    List.iter reg_sample Ex08.sample_values;
    M.register_post_handler invp (fun x -> return (200, json_of_named_json x)) in
  ()
