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
open Kxclib
open Kxclib.Json
open Bindoj_base
open Bindoj_apidir_shared

type error =
  | Unexpected_response of { status: int; body: jv }
  | Bad_response of {
      body: jv;
      desc: Typed_type_desc.boxed_type_decl;
      msg: string option
    }

exception Apidir_client_error of error

module type JsonResponse = Apidir_base.JsonResponse

module type ScopedJsonFetcher = sig
  module IoStyle : Utils.IoStyle
  module Response : JsonResponse

  val perform_get :
    urlpath:string
    -> headers:string list
    -> query_params:(string*string) list
    -> Response.t IoStyle.t

  val perform_post :
    urlpath:string
    -> headers:string list
    -> query_params:(string*string) list
    -> body:jv
    -> Response.t IoStyle.t
end

module type T = sig
  type 'resp io

  val perform_json_post :
    ?additional_headers:string list ->
    ?additional_query_params:(string * string) list ->
    ('req, 'resp) invocation_point_info -> 'req -> 'resp io

  val perform_json_get :
    ?additional_headers:string list ->
    ?additional_query_params:(string * string) list ->
    (unit, 'resp) invocation_point_info -> 'resp io
end

module Make (Dir : ApiDirManifest) (Fetcher : ScopedJsonFetcher) = struct
  include Apidir_base.Make(Dir)(Fetcher.IoStyle)
  open IoOps

  let process_response ttd jv =
    let resp_type_name = (Typed_type_desc.Typed.decl ttd).td_name in
    match Bindoj_codec.Json.of_json ~env:tdenv ttd jv with
    | exception exn ->
      Apidir_client_error (
        Bad_response {
          body = jv;
          desc = Typed_type_desc.Boxed ttd;
          msg  = Printexc.to_string exn |> some;
        }
      )|> Fetcher.IoStyle.inject_error
    | None ->
      Apidir_client_error (
        Bad_response {
          body = jv;
          desc = Typed_type_desc.Boxed ttd;
          msg =
            sprintf "Bindoj_codec.Json.to_json (%s) returns None"
              resp_type_name |> some;
        }
      ) |> Fetcher.IoStyle.inject_error
    | Some x -> return x

  let match_response (responses: 'respty response_case list) (resp: Fetcher.Response.t) =
    let resp_status, resp_body = Fetcher.Response.(status resp, body resp) in
    let case =
      responses |> List.find_opt (function Response_case { status; _ } ->
        match status with
        | `default -> true
        | `status_code status when status = resp_status -> true
        | `status_range `_1XX when 100 <= resp_status && resp_status < 200 -> true
        | `status_range `_2XX when 200 <= resp_status && resp_status < 300 -> true
        | `status_range `_3XX when 300 <= resp_status && resp_status < 400 -> true
        | `status_range `_4XX when 400 <= resp_status && resp_status < 500 -> true
        | `status_range `_5XX when 500 <= resp_status && resp_status < 600 -> true
        | _ -> false
      )
    in
    match case with
    | None ->
      Apidir_client_error (
        Unexpected_response { status = resp_status; body = resp_body }
      ) |> Fetcher.IoStyle.inject_error
    | Some (Response_case { response; pack; _ }) ->
      process_response
        (Utils.ttd_of_media_type response.rs_media_type)
        resp_body
      >|= pack

  let perform_json_post :
        'req 'resp.
        ?additional_headers:string list
        -> ?additional_query_params:(string*string) list
        -> ('req, 'resp) invocation_point_info
        -> 'req
        -> 'resp io =
    fun ?additional_headers ?additional_query_params invp reqbody ->
    let req = match invp.ip_method, invp.ip_request_body with
      | `get, _ -> invalid_arg' "perform_json_post called on GET invp: %s" invp.ip_name
      | `post, None -> invalid_arg' "POST method must have a request body definition: %s" invp.ip_name
      | `post, Some desc ->
         let ttd = Utils.ttd_of_media_type desc.rq_media_type in
         reqbody |> Bindoj_codec.Json.to_json ~env:tdenv ttd in
    let urlpath = invp.ip_urlpath in
    let query_params = additional_query_params |? [] in
    let headers = additional_headers |? [] in
    Fetcher.perform_post ~urlpath ~headers ~query_params ~body:req
    >>= match_response invp.ip_responses

  let perform_json_get :
        'resp.
        ?additional_headers:string list
        -> ?additional_query_params:(string*string) list
        -> (unit, 'resp) invocation_point_info -> 'resp io =
    fun ?additional_headers ?additional_query_params invp ->
    let urlpath = invp.ip_urlpath in
    let query_params = additional_query_params |? [] in
    let headers = additional_headers |? [] in
    Fetcher.perform_get ~urlpath ~headers ~query_params
    >>= match_response invp.ip_responses
end
