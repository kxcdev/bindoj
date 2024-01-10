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

open Bindoj_test_common
open Bindoj_test_common_jsoo_utils

module Bridge(Dir: Bindoj_apidir_shared.ApiDirManifest) = struct
  module Io = FutexnIo
  let register_get_example _ _ ~orig:_ ~jv:_ ~pp:_ = ()
  let register_post_example _ _ ~orig_resp:_ ~orig_req:_ ~jv_resp:_ ~jv_req:_ ~pp:_ = ()
  include Bindoj_apidir_runtime.Apidir_server_bridge.Make(Dir)(FutexnIo)
end

module type Sample_apidir = sig
  include Bindoj_apidir_shared.ApiDirManifest
  include Apidir_examples.T
end

module type Apidir_bridge =
  Bindoj_apidir_runtime.Apidir_server_bridge.T
  with type 'x io = 'x FutexnIo.t

let export_to_js (module Dir : Sample_apidir) =
  let module Bridge_dir = Bridge(Dir) in
  let () = Dir.build_mock_server (module Bridge_dir) in
  let prepare_response_for_js resp : Json.jv =
    let open Bindoj_apidir_runtime.Apidir_server_bridge in
    let status, body =
    TupleJsonResponse.(
      status resp, body resp) in
    `obj [
        "status_code", `num (float_of_int status);
        "body", body
      ]
  in

  let make_server_mock_js (module Bridge : Apidir_bridge) =
    let postprocess resp =
      let open FutexnIoOps in
      resp >|= prepare_response_for_js
      >|= cast % Kxclib_js.Json_ext.to_xjv
      |> FutexnIo.to_promise in
    object%js
      method handle_path_json_get_js path =
        let path = ostr path in
        Bridge.handle_path_json_get path
        |> postprocess
      method handle_path_json_post_js path reqbody =
        let path = ostr path in
        let reqbody : Json.jv =
          reqbody |> Kxclib_js.Json_ext.of_xjv |> cast in
        Bridge.handle_path_json_post path reqbody
        |> postprocess
    end
  in
  Js_of_ocaml.Js.export "bindoj_jsoo_bridge" (object%js
    val coverage_helper_js = coverage_helper_js
    val server_mock_js = make_server_mock_js (module Bridge_dir)
  end)
