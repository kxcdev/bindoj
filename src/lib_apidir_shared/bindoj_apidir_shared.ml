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
open Bindoj_typedesc.Typed_type_desc

(** TODO.future - temporary solution before the arrival of type cosmos *)
type type_decl_collection = {
    type_declarations : type_decl_info list;
    type_decl_environment_wrappers : tdenv endo list;
  }
and type_decl_info = {
  tdi_name : string;
  tdi_doc : string;
  tdi_decl : boxed_type_decl;
}

let type_decl_info_of_typed_decl : string -> string -> 't typed_type_decl -> type_decl_info = fun name doc decl ->
  { tdi_name = name;
    tdi_doc = doc;
    tdi_decl = Boxed decl; }

type http_status = [
  | `default
  | `status_code of int
  | `status_range of [`_1XX | `_2XX | `_3XX | `_4XX | `_5XX]
]

let string_of_http_status : http_status -> string = function
  | `default -> "default"
  | `status_code code -> string_of_int code
  | `status_range `_1XX -> "1XX"
  | `status_range `_2XX -> "2XX"
  | `status_range `_3XX -> "3XX"
  | `status_range `_4XX -> "4XX"
  | `status_range `_5XX -> "5XX"

type ('reqty, 'respty) invocation_point_info = {
  ip_name : string;
  ip_urlpath : string;
  ip_method : [ `get | `post ];
  ip_request_body : 'reqty request_body option;
  ip_responses : 'respty response_case list;
  ip_deprecated : bool;
  ip_summary : string option;
  ip_description : string option;
  ip_external_doc : external_doc option;
}

and 't request_body = {
  rq_media_type : 't media_type;
  rq_description : string;
  rq_required : bool;
}

and 't response = {
  rs_media_type : 't media_type;
  rs_description : string;
  rs_headers : 't header list;
}

and 't response_case =
  | Response_case : {
      name: string;
      doc: string;
      status: http_status;
      response: 'a response;
      pack: 'a -> 't;
      unpack: 't -> 'a option;
    } -> 't response_case

and external_doc = {
  ed_urlpath : string;
  ed_description : string option;
}

(* TODO.future #222
and parameter = {
  pr_name : string;
  pr_loc : parameter_location;
  pr_description : string;
  pr_required : bool;
  pr_deprecated : bool;
  pr_type_decl : type_decl;
  pr_examples : example list;
}

and parameter_location = Query | Header | Path | Cookie
*)

and 't media_type = {
  mt_type : 't typed_type_decl;
  mt_examples : (string * 't) list;
  mt_external_examples : (string * string) list;
  (* mt_encoding : encoding; *) (* TODO.future - assume that encoding codec is UTF8 #223 *)
}

and 't header = {
  hd_name : string;
  hd_description : string;
  hd_required : bool;
  hd_deprecated : bool;
  hd_type_decl : 't typed_type_decl;
}

and external_example = {
  urlpath: string;
  summary : string;
  description : string;
}

let make_response_case ?(status=`default) ?name ?doc ~pack ~unpack resp_type =
  let resp_name =
    match name with
    | None -> (Typed.decl resp_type).td_name
    | Some name -> name in
  let resp_doc =
    match doc with
    | None -> resp_name
    | Some doc -> doc in
  let response = {
    rs_media_type = {
      mt_type = resp_type;
      mt_examples = [];
      mt_external_examples = [];
    };
    rs_description = resp_doc;
    rs_headers = [];
  } in
  Response_case {
    name = resp_name; doc = resp_doc;
    status; response; pack; unpack
  }

type untyped_invocation_point_info =
  | Invp : (_, _) invocation_point_info -> untyped_invocation_point_info

type invocation_point_collection = untyped_invocation_point_info list

type registry_info = invocation_point_collection * type_decl_collection

module type ApiDirManifest = sig
  val registry_info : unit -> registry_info
end

module type RegistryInfo = sig
  type nonrec ('reqty, 'respty) invocation_point_info = ('reqty, 'respty) invocation_point_info
  type nonrec registry_info = registry_info
  include ApiDirManifest
end

module type MakeRegistryS = sig

  (** TODO.future - allow specifying examples for req/resp #221 *)

  val register_type_decl_info :
    ?name:string
    -> ?doc:string
    -> 'a typed_type_decl
    -> unit

  (** TODO.future - temporary solution before the arrival of type cosmos *)
  val add_type_decl_environment_wrapper :
    (tdenv -> tdenv) -> unit

  val register_get :
    ?summary:string
    -> ?description:string
    -> ?resp_name:string
    -> ?resp_doc:string
    -> urlpath:string
    -> resp_type:('respty typed_type_decl)
    -> string (** name of the invocation point *)
    -> (unit, 'respty) invocation_point_info

  val register_get' :
    ?summary:string
    -> ?description:string
    -> urlpath:string
    -> string (** name of the invocation point *)
    -> 'respty response_case list
    -> (unit, 'respty) invocation_point_info

  val register_post :
    ?summary:string
    -> ?description:string
    -> ?req_name:string
    -> ?req_doc:string
    -> ?resp_name:string
    -> ?resp_doc:string
    -> urlpath:string
    -> req_type:('reqty typed_type_decl)
    -> resp_type:('respty typed_type_decl)
    -> string (** name of the invocation point *)
    -> ('reqty, 'respty) invocation_point_info

  val register_post' :
    ?summary:string
    -> ?description:string
    -> ?req_name:string
    -> ?req_doc:string
    -> urlpath:string
    -> req_type:('reqty typed_type_decl)
    -> string (** name of the invocation point *)
    -> 'respty response_case list
    -> ('reqty, 'respty) invocation_point_info

  module Public : RegistryInfo
end

module MakeRegistry () : MakeRegistryS = struct
  let invp_registry : invocation_point_collection ref = ref []
  let type_registry : type_decl_info list ref = ref []
  let tdenv_wrappers : tdenv endo list ref = ref []

  let register : type_decl_info list -> ('reqty, 'respty) invocation_point_info -> unit = fun typs invp ->
    refappend invp_registry (Invp invp);
    refupdate type_registry (( @ ) typs)

  let register_type_decl_info ?name ?doc ttd =
    let td = Typed.decl ttd in
    let tdi_name = name |? td.td_name in
    let tdi_doc = doc |? (match td.td_doc with
                          | `nodoc -> tdi_name
                          | `docstr str -> str ) in
    { tdi_name; tdi_doc; tdi_decl = Boxed ttd; }
    |> refappend type_registry

  let add_type_decl_environment_wrapper wrapper = refappend tdenv_wrappers wrapper

  let register_get' :
    ?summary:string
    -> ?description:string
    -> urlpath:string
    -> string (* name *)
    -> 'respty response_case list
    -> (unit, 'respty) invocation_point_info =
    fun ?summary ?description ~urlpath name responses ->
    let invp = {
      ip_name = name;
      ip_urlpath = urlpath;
      ip_method = `get;
      ip_request_body = None;
      ip_responses = responses;
      ip_deprecated = false;
      ip_summary = summary;
      ip_description = description;
      ip_external_doc = None;
    } in
    let decls =
      responses |> List.map (function Response_case { name; doc; response; _ } ->
        type_decl_info_of_typed_decl name doc response.rs_media_type.mt_type
      )
    in
    register decls invp; invp

  let register_get ?summary ?description ?resp_name ?resp_doc ~urlpath ~resp_type name =
    let resp_case =
      make_response_case ?name:resp_name ?doc:resp_doc
        ~pack:(fun x -> x) ~unpack:(fun x -> Some x) resp_type
    in
    register_get' ?summary ?description ~urlpath name [resp_case]

  let register_post' :
    ?summary:string
    -> ?description:string
    -> ?req_name:string
    -> ?req_doc:string
    -> urlpath:string
    -> req_type:('reqty typed_type_decl)
    -> string (* name *)
    -> 'respty response_case list
    -> ('reqty, 'respty) invocation_point_info =
    fun ?summary ?description ?req_name ?req_doc ~urlpath ~req_type name responses ->
    let req_name =
      match req_name with
      | None -> (Typed.decl req_type).td_name
      | Some name -> name in
    let req_doc =
      match req_doc with
      | None -> req_name
      | Some doc -> doc in
    let request_body = {
      rq_media_type = {
        mt_type = req_type;
        mt_examples = [];
        mt_external_examples = [];
      };
      rq_description = req_doc;
      rq_required = false;
    } in
    let invp = {
      ip_name = name;
      ip_urlpath = urlpath;
      ip_method = `post;
      ip_request_body = Some request_body;
      ip_responses = responses;
      ip_deprecated = false;
      ip_summary = summary;
      ip_description = description;
      ip_external_doc = None;
    } in
    let decls =
      responses |> List.map (function Response_case { name; doc; response; _ } ->
        type_decl_info_of_typed_decl name doc response.rs_media_type.mt_type
      )
    in
    register (type_decl_info_of_typed_decl req_name req_doc req_type :: decls) invp;
    invp

  let register_post ?summary ?description ?req_name ?req_doc ?resp_name ?resp_doc ~urlpath ~req_type ~resp_type name =
    let resp_case =
      make_response_case ?name:resp_name ?doc:resp_doc
        ~pack:(fun x -> x) ~unpack:(fun x -> Some x) resp_type
    in
    register_post' ?summary ?description ?req_name ?req_doc ~urlpath ~req_type name [resp_case]

  module Public = struct
    type nonrec ('reqty, 'respty) invocation_point_info = ('reqty, 'respty) invocation_point_info
    type registry_info = invocation_point_collection * type_decl_collection
    let registry_info () : registry_info =
      let tdcoll = {
          type_declarations = !type_registry;
          type_decl_environment_wrappers = !tdenv_wrappers;
        } in
      (!invp_registry, tdcoll)
  end
end
