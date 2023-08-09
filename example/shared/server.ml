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
open Bindoj_apidir_runtime
open Bindoj_example_shared_apidir.Apidir

module Make_request_handler (Io: Monadic) (Dir: Apidirectory) = struct
  module Io = Io
  module Bridge = struct
    module Io = Io
    include Apidir_server_bridge.Make (Dir) (Io)
  end

  open Bindoj_apidir_runtime.Utils
  let handle_request :
    path:string
    -> meth:[> `GET | `POST of Json.jv ]
    -> Apidir_server_bridge.TupleJsonResponse.t Io.t =
    fun ~path ~meth ->
      try
        match meth with
        | `GET -> Bridge.handle_path_json_get path
        | `POST body -> Bridge.handle_path_json_post path body
        | _ -> Io.return (403, `str "This method is not allowed")
      with
        | Unrecognized_route s ->
          Io.return (404, `str ("Unrecognized route: "^s))
        | Bad_request s ->
          Io.return (400, `str ("Bad request: "^s))
        | Invalid_argument s ->
          Io.return (503, `str ("Internal server error: "^s))
end

open Cohttp
open Cohttp_lwt_unix

module Make_cohttp_server (Dir: Apidirectory) = struct
  open MonadOps(Lwt)

  include Make_request_handler(Lwt)(Dir)

  let make_server () =
    Server.make () ~callback:(fun _conn req body ->
      let open Lwt in
      let path = req |> Request.uri |> Uri.path in
      let headers = Header.(init () |> Fn.flip add_list [
          "Content-Type", "application/json"
        ])
      in
      begin
        match Request.meth req, path with
        | `GET, "/_health" -> Io.return (200, `str "i am not a tea pot")
        | `GET, _ ->
           handle_request ~path ~meth:`GET
        | `POST, _ ->
          Cohttp_lwt.Body.to_string body
          >|= (Yojson.Safe.from_string &> Json.of_yojson)
          >>= (fun body -> handle_request ~path ~meth:(`POST body))
        | _ -> Lwt.return (405, `str "This method is not allowed")
      end
      >>= fun (code, body) ->
        Server.respond_string ()
          ~headers
          ~status:(`Code code)
          ~body:(body |> Json.to_yojson |> Yojson.Safe.to_string)
    )
end
