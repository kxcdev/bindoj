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
open Bindoj_apidir_runtime.Apidir_client

module LwtIoStyle = struct
  type 'x t = ('x, exn*backtrace_info option) result Lwt.t

  let return : 'x -> 'x t = fun x -> Result.ok x |> Lwt.return

  let bind : 'x t -> ('x -> 'y t) -> 'y t =
    let open Lwt in
    fun x f ->
      x >>= (function
      | Ok x -> f x
      | Error e -> Error e |> return)

  let inject_error : exn -> 'x t =
    fun exn ->
      (exn, Some (`ocaml_backtrace (Printexc.get_raw_backtrace ())))
      |> Lwt.return_error

  let extract_error : 'x t -> ('x, exn*backtrace_info option) result t =
    fun x -> Lwt.map (Result.ok) x

  let trace : string t -> unit =
    let open Lwt in
    fun s ->
    s >|= Result.iter (
      Log0.log "%s" ~label:"trace"
        ~header_style:(Some `Thin) ~header_color:`Yellow)
    |> Lwt_main.run
end

module type Client_config = sig
  val base_url : string
end

module Make_cohttp_fetcher(Config: Client_config) = struct
  open struct
    let assert_empty_header, assert_empty_query_params =
      let assert_empty label pp = function
        | [] -> ()
        | xs -> invalid_arg' "%s is expected to be empty but got %a" label (List.pp pp) xs
      in
      assert_empty "headers" pp_string,
      assert_empty "query_params" (pp_string %% (fun (k,v) -> sprintf "%s: %s" k v))
  end

  module IoStyle = LwtIoStyle
  module Response = Apidir_server_bridge.TupleJsonResponse

  open MonadOps(Lwt)

  open struct
    open Cohttp
    open Cohttp_lwt_unix

    let perform =
      fun (resp, body) ->
        let code = resp |> Response.status |> Code.code_of_status in
        Cohttp_lwt.Body.to_string body
        >|= (Yojson.Safe.from_string &> Json.of_yojson)
        >|= fun body -> Result.ok(code, body)
  end

  let perform_get =
    fun ~(urlpath: string) ~(headers: string list) ~(query_params: (string * string) list): Response.t IoStyle.t ->
    let open Cohttp in
    let open Cohttp_lwt_unix in
    assert_empty_header headers; assert_empty_query_params query_params;
    Client.get (Uri.of_string (Config.base_url^urlpath)) >>= perform

  let perform_post =
    fun ~(urlpath: string) ~(headers: string list) ~(query_params: (string * string) list) ~(body: Json.jv): Response.t IoStyle.t ->
    let open Cohttp in
    let open Cohttp_lwt_unix in
    assert_empty_header headers; assert_empty_query_params query_params;
    let body =
      Json.to_yojson body
      |> Yojson.Safe.to_string
      |> Cohttp_lwt.Body.of_string
    in
    Client.post ~body (Uri.of_string (Config.base_url^urlpath)) >>= perform
end
