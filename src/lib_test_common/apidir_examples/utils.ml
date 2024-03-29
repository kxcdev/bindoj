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
type http_method = [ `get | `post ] [@@deriving show]
let http_method = Alcotest.of_pp pp_http_method

let check_invp name invp ~ip_name ~ip_urlpath ~ip_method =
  let open Alcotest in
  let open Bindoj_apidir_shared in
  check string (sprintf "%s.ip_name" name) ip_name invp.ip_name;
  check string (sprintf "%s.ip_urlpath" name) ip_urlpath invp.ip_urlpath;
  check http_method (sprintf "%s.ip_method" name) ip_method invp.ip_method

module type MockServerBuilder = sig
  open Bindoj_apidir_shared

  module Io : Kxclib.Monadic

  val register_get_handler :
    (unit, 'respty) invocation_point_info ->
    (unit -> (int * 'respty) Io.t) ->
    unit

  val register_post_handler :
    ('reqty, 'respty) invocation_point_info ->
    ('reqty -> (int * 'respty) Io.t) ->
    unit

  val register_get_example :
    string ->
    untyped_invocation_point_info ->
    orig:'a ->
    jv:Json.jv ->
    pp:(ppf -> 'a -> unit) ->
    unit

  val register_post_example :
    string ->
    untyped_invocation_point_info ->
    orig_resp:'a ->
    orig_req:'b ->
    jv_resp:Json.jv ->
    jv_req:Json.jv ->
    pp:(ppf -> 'a -> unit) ->
    unit
end
