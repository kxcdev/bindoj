(* Copyright 2022 Kotoi-Xie Consultancy, Inc.
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. *)

(* Acknowledgements - AnchorZ Inc.
The initial version or a significant portion of this file is developed
under the funding of AnchorZ Inc. to satisfy its needs in
product development. *)

open Bindoj_typedesc.Typed_type_desc

type type_decl_collection = type_decl_info list
and type_decl_info = {
  tdi_name : string;
  tdi_doc : string;
  tdi_decl : boxed_type_decl;
}

let type_decl_info_of_typed_decl : string -> string -> 't typed_type_decl -> type_decl_info = fun name doc decl ->
  { tdi_name = name;
    tdi_doc = doc;
    tdi_decl = Boxed decl; }

type ('reqty, 'respty) invocation_point_info = {
  ip_name : string;
  ip_urlpath : string;
  ip_method : [ `get | `post ];
  ip_request_body : 'reqty request_body option;
  ip_responses : ([`status_code of int | `default] * 'respty response) list;
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

and external_doc = {
  ed_urlpath : string;
  ed_description : string option;
}

(* future work
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
  (* mt_encoding : encoding; *) (* assume that encoding codec is UTF8 *)
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

type untyped_invocation_point_info =
  | Invp : (_, _) invocation_point_info -> untyped_invocation_point_info

type invocation_point_collection = untyped_invocation_point_info list

module type MakeRegistryS = sig

  (** TODO - allow specifying multiple response types *)
  (** TODO - allow specifying examples for req/resp  *)

  val register_get :
    ?summary:string
    -> ?description:string
    -> ?resp_name:string
    -> ?resp_doc:string
    -> urlpath:string
    -> resp_type:('respty typed_type_decl)
    -> string (** name of the invocation point *)
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

  module Public : sig
    type nonrec ('reqty, 'respty) invocation_point_info = ('reqty, 'respty) invocation_point_info
    val registry_info : unit -> invocation_point_collection * type_decl_collection
  end
end

module MakeRegistry () : MakeRegistryS = struct
  let invp_registry : invocation_point_collection ref = ref []
  let type_registry : type_decl_collection ref = ref []

  let register : type_decl_info list -> ('reqty, 'respty) invocation_point_info -> unit = fun typs invp ->
    refappend invp_registry (Invp invp);
    refupdate type_registry (( @ ) typs)

  let register_get :
    ?summary:string
    -> ?description:string
    -> ?resp_name:string
    -> ?resp_doc:string
    -> urlpath:string
    -> resp_type:('respty typed_type_decl)
    -> string (* name *)
    -> (unit, 'respty) invocation_point_info =
    fun ?summary ?description ?resp_name ?resp_doc ~urlpath ~resp_type name ->
    let resp_name = match resp_name with
      | None -> (Typed.decl resp_type).td_name
      | Some name -> name in
    let resp_doc = match resp_doc with
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
    let invp = {
      ip_name = name;
      ip_urlpath = urlpath;
      ip_method = `get;
      ip_request_body = None;
      ip_responses = [`default, response];
      ip_deprecated = false;
      ip_summary = summary;
      ip_description = description;
      ip_external_doc = None;
    } in
    register [type_decl_info_of_typed_decl resp_name resp_doc resp_type] invp; invp

  let register_post :
    ?summary:string
    -> ?description:string
    -> ?req_name:string
    -> ?req_doc:string
    -> ?resp_name:string
    -> ?resp_doc:string
    -> urlpath:string
    -> req_type:('reqty typed_type_decl)
    -> resp_type:('respty typed_type_decl)
    -> string (* name *)
    -> ('reqty, 'respty) invocation_point_info =
    fun ?summary ?description ?req_name ?req_doc ?resp_name ?resp_doc ~urlpath ~req_type ~resp_type name ->
    let req_name = match req_name with
      | None -> (Typed.decl req_type).td_name
      | Some name -> name in
    let req_doc = match req_doc with
      | None -> req_name
      | Some doc -> doc in
    let resp_name = match resp_name with
      | None -> (Typed.decl resp_type).td_name
      | Some name -> name in
    let resp_doc = match resp_doc with
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
      ip_responses = [`default, response];
      ip_deprecated = false;
      ip_summary = summary;
      ip_description = description;
      ip_external_doc = None;
    } in
    register [type_decl_info_of_typed_decl req_name req_doc req_type;
              type_decl_info_of_typed_decl resp_name resp_doc resp_type;]
      invp;
    invp

  module Public = struct
    type nonrec ('reqty, 'respty) invocation_point_info = ('reqty, 'respty) invocation_point_info
    let registry_info () = (!invp_registry, !type_registry)
  end
end
