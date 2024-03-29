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
open Bindoj_typedesc.Typed_type_desc
open Bindoj_runtime
open Bindoj_gen.Json_codec
open Bindoj_apidir_shared
module OpenApi = Bindoj_openapi.V3
module Codec = Bindoj_codec

let content_type = "application/json" (* assert Content-Type is always application/json *)

let server_of_url url =
  let url = Uri.of_string url in
  match Uri.host url with
  | Some host ->
    let host_url = Uri.make ~scheme:(Uri.scheme url |? "https") ~host () in
    Some (Uri.to_string host_url)
  | None -> None

let path_of_url url =
  let url = Uri.of_string url in
  Uri.path_and_query url

let rec gen_openapi_document_object :
  title:string -> version:string -> registry_info -> OpenApi.Document_object.t =
  fun ~title ~version reg_info ->
  let components = openapi_components_object_of_type_decl_collection reg_info in
  let paths_object = openapi_paths_object_of_invocation_point_collection reg_info in
  let info_object = OpenApi.Info_object.mk title version in
  let openapi_version = "3.0.3" in
  OpenApi.Document_object.mk ~components openapi_version info_object paths_object

and openapi_components_object_of_type_decl_collection :
  registry_info -> OpenApi.Components_object.t =
  fun registry_info ->
  let { prim_ident_typemap; alias_ident_typemap; } : tdenv =
    tdenv_of_registry_info registry_info in
  let decls =
    (prim_ident_typemap
     |> StringMap.to_list |&> fun (_, Boxed_prim (_, typed)) -> Typed.decl typed) @
    (alias_ident_typemap
     |> StringMap.to_list |&> fun (_, boxed) -> (Typed.decl % Typed.unbox) boxed) in
  let decl_schemas =
    decls |> List.map (fun td ->
      fst (Json_config.get_mangled_name_of_type td),
      Either.left (openapi_schema_object_of_type_decl td))
    in
  let references =
    alias_ident_typemap
    |> StringMap.to_list
    |> List.filter_map (fun (name, boxed) ->
      let decl = (Typed.decl % Typed.unbox) boxed in
      if name = decl.td_name then None
      else
        let json_name = fst (Json_config.get_mangled_name_of_type decl) in
        let ref = OpenApi.Reference_object.mk (sprintf "#/components/schemas/%s" json_name) in
        Some (json_name, Either.right ref)) in
  let schemas =
    decl_schemas @ references
    |> List.sort_uniq (fun (n1, _) (n2, _) -> compare n1 n2) in
  OpenApi.Components_object.mk ~schemas ()

and openapi_paths_object_of_invocation_point_collection :
  registry_info -> OpenApi.Path_item_object.paths_object =
  fun reg_info ->
  OpenApi.Path_item_object.paths (
    reg_info
    |> fst
    |&> function
      | Invp invp ->
        let invp : ('reqty, 'respty) invocation_point_info = Obj.magic invp in
        let path_item = openapi_path_item_object_of_invocation_point_info reg_info invp in
        (path_of_url invp.ip_urlpath, path_item))

and openapi_path_item_object_of_invocation_point_info :
  registry_info -> ('reqty, 'respty) invocation_point_info -> OpenApi.Path_item_object.t =
  fun reg_info invp ->
  let { ip_name; ip_urlpath; ip_method;
        ip_request_body; ip_responses; ip_deprecated;
        ip_summary = summary; ip_description = description; ip_tags;
        ip_external_doc; ip_usage_samples } = invp in
  let summary = summary |? ip_name in
  let tags = match ip_tags with [] -> None | xs -> Some xs in
  let req_samples =
    ip_usage_samples
    |> List.filter_map (function
      | Req_sample s -> Some s
      | Resp_sample _ -> None
      | Usage_sample ((req, _, _), doc) -> Some (req,doc))
  in
  let resp_samples =
    ip_usage_samples
    |> List.filter_map (function
    | Req_sample _ -> None
    | Resp_sample s -> Some s
    | Usage_sample ((_, resp, status), doc) -> Some ((resp, status), doc))
  in
  let mk_path_item_object op =
    let servers =
      match server_of_url ip_urlpath with
      | Some server -> Some [OpenApi.Server_object.mk server]
      | None -> None
    in
    match ip_method with
    | `get -> OpenApi.Path_item_object.mk ~summary ?description ?servers ~get:op ()
    | `post -> OpenApi.Path_item_object.mk ~summary ?description ?servers ~post:op ()
  in
  let mk_operation_object request_body responses =
    let externalDocs = ip_external_doc |> Option.map openapi_external_documentation_object_of_external_doc in
    let requestBody = request_body |> Option.map Either.left in
    OpenApi.Path_item_object.operation
      ~deprecated:ip_deprecated
      ~summary ?description ?tags ?externalDocs ?requestBody
      responses
  in
  let request_body =
    (Obj.magic ip_request_body) |> Option.map (openapi_request_body_object_of_request_body reg_info (Obj.magic req_samples)) in
  let responses =
    ip_responses |&> function Response_case { status; response; samples; unpack; _ } ->
      let samples : 'a with_doc list =
        resp_samples
        |&?> (function
          | ((v, st), doc) when st = status ->
            unpack v >? (fun v -> (v, doc))
          | _ -> None)
        |> List.append samples
        |> Obj.magic
      in
      let response = openapi_response_object_of_response reg_info (samples) (Obj.magic response) in
      (status, Either.left response) in
  let operation = mk_operation_object request_body responses in
  mk_path_item_object operation

and openapi_request_body_object_of_request_body : registry_info -> 't with_doc list -> 't request_body -> OpenApi.Request_body_object.t =
  fun reg_info req_samples { rq_media_type; rq_description; rq_required; } ->
  let media_type = openapi_media_type_object_of_media_type reg_info req_samples rq_media_type in
  OpenApi.Request_body_object.mk
    ~description:rq_description
    ~required:rq_required
    [(content_type, media_type)]

and openapi_response_object_of_response : registry_info -> 't with_doc list -> 't response -> OpenApi.Response_object.t =
  fun reg_info resp_samples { rs_media_type; rs_description; rs_headers; } ->
  let media_type = openapi_media_type_object_of_media_type reg_info resp_samples rs_media_type in
  let headers =
    rs_headers |&> fun header ->
      let { hd_name; _; } = header in
      let header = openapi_header_object_of_header reg_info header in
      (hd_name, Either.left header) in
  match headers with
  | [] -> OpenApi.Response_object.mk
            ~content:[(content_type, media_type)]
            rs_description
  | _ -> OpenApi.Response_object.mk
           ~headers
           ~content:[(content_type, media_type)]
           rs_description

and openapi_header_object_of_header : registry_info -> 't header -> OpenApi.Header_object.t =
  fun reg_info { hd_name=_; hd_description; hd_required; hd_deprecated; hd_type_decl; } ->
  let type_decl = Typed.decl hd_type_decl in
  OpenApi.Header_object.mk
    ~description:hd_description
    ~required:hd_required
    ~deprecated:hd_deprecated
    ~schema:(openapi_schema_or_reference_of_type_decl reg_info type_decl)
    ()

and openapi_media_type_object_of_media_type :
  registry_info -> 't with_doc list -> 't media_type -> OpenApi.Header_object.media_type_object =
  fun reg_info samples { mt_type; mt_external_examples; } ->
  let _, {
      type_declarations;
      type_decl_environment_wrappers
    } = reg_info in
  let alias_ident_typemap =
    type_declarations
    |> foldl (fun acc info ->
           acc |> StringMap.add info.tdi_name info.tdi_decl
         ) StringMap.empty in
  let env0 = Type_decl_environment.{
        alias_ident_typemap;
        prim_ident_typemap = StringMap.empty;
             } in
  let env = type_decl_environment_wrappers |> List.foldl (|>) env0 in
  let type_decl = Typed.decl mt_type in
  let examples =
    (samples |> List.mapi (fun index (sample_val, sample_name) ->
        ((match sample_name with `docstr s -> s | `nodoc -> sprintf "example_%d" index),
         OpenApi.Example_object.mk
           ~value:(Codec.Json.to_json ~env mt_type sample_val)
           ()
         |> Either.left))) @
    (mt_external_examples |&> fun (ex_name, ex_url) ->
        (ex_name,
         OpenApi.Example_object.mk
           ~externalValue:ex_url
           ()
         |> Either.left)) in
  OpenApi.Header_object.media_type
    ~examples
    ~schema:(openapi_schema_or_reference_of_type_decl reg_info type_decl) ()

and openapi_external_documentation_object_of_external_doc :
  external_doc -> OpenApi.External_documentation_object.t =
  fun { ed_urlpath; ed_description = description; } ->
  OpenApi.External_documentation_object.mk ed_urlpath ?description

and openapi_schema_object_of_type_decl : type_decl -> OpenApi.Schema_object.t = gen_openapi_schema

and openapi_schema_or_reference_of_type_decl : registry_info -> type_decl -> (OpenApi.Schema_object.t, OpenApi.Reference_object.t) either =
  fun (_, tdcoll) td ->
    tdcoll.type_declarations |> List.exists (fun tdi ->
      let (Boxed ttd) = tdi.tdi_decl in
      let td' = Typed.decl ttd in
      td.td_name = td'.td_name
    )
    |> function
    | true ->
      Json_config.get_mangled_name_of_type td
      |> fst
      |> sprintf "#/components/schemas/%s"
      |> OpenApi.Reference_object.mk
      |> Either.right
    | false -> openapi_schema_object_of_type_decl td |> Either.left

module Internals = struct
  let openapi_components_object_of_type_decl_collection =
    openapi_components_object_of_type_decl_collection

  let openapi_paths_object_of_invocation_point_collection =
    openapi_paths_object_of_invocation_point_collection

  let openapi_path_item_object_of_invocation_point_info =
    openapi_path_item_object_of_invocation_point_info

  let openapi_request_body_object_of_request_body =
    openapi_request_body_object_of_request_body

  let openapi_response_object_of_response =
    openapi_response_object_of_response

  let openapi_header_object_of_header =
    openapi_header_object_of_header

  let openapi_media_type_object_of_media_type =
    openapi_media_type_object_of_media_type

  let openapi_external_documentation_object_of_external_doc =
    openapi_external_documentation_object_of_external_doc

  let openapi_schema_object_of_type_decl = openapi_schema_object_of_type_decl

  let openapi_schema_or_reference_of_type_decl = openapi_schema_or_reference_of_type_decl
end
