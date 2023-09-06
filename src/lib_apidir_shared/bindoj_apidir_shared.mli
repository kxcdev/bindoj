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
open Bindoj_runtime
open Bindoj_typedesc.Typed_type_desc

module Utils : sig
  module PathComps : sig
    type t
    val combine : t -> t -> t
    val of_url_path : string -> t
    val to_url_path : ?leading_slash:bool -> t -> string
    val remake_url_path : ?leading_slash:bool -> string -> string
  end
end

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

type http_status = [
  | `default
  | `status_code of int
  | `status_range of [`_1XX | `_2XX | `_3XX | `_4XX | `_5XX]
]

val string_of_http_status : http_status -> string

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
  ip_usage_samples : ('reqty, 'respty) invp_usage_sample list;
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
      samples: 'a with_doc list;
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
  mt_external_examples : (string * string) list;
  (* mt_encoding : encoding; *) (* TODO.future assume that encoding codec is UTF8 #223 *)
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

and ('reqty, 'respty) invp_usage_sample =
  | Req_sample of 'reqty with_doc
  | Resp_sample of ('respty * http_status) with_doc
  | Usage_sample of ('reqty * 'respty * http_status) with_doc

val make_response_case :
  ?status:http_status
  -> ?name:string
  -> ?doc:string
  -> ?samples:('a with_doc list)
  -> pack:('a -> 'respty)
  -> unpack:('respty -> 'a option)
  -> 'a typed_type_decl
  -> 'respty response_case

type invocation_point_meta = {
  ipm_urlpath : string;
  ipm_method : [ `get | `post ];
}

type ('reqty, 'respty) invocation_point_additional_info = {
  ipa_name : string;
  ipa_request_body : 'reqty request_body option;
  ipa_responses : 'respty response_case list;
  ipa_deprecated : bool;
  ipa_summary : string option;
  ipa_description : string option;
  ipa_external_doc : external_doc option;
  ipa_usage_samples : ('reqty, 'respty) invp_usage_sample list;
}

type untyped_invocation_point_info =
  | Invp : (_, _) invocation_point_info -> untyped_invocation_point_info

type untyped_invocation_point_additional_info =
  | InvpAdditionalInfo : (_, _) invocation_point_additional_info -> untyped_invocation_point_additional_info

val to_invocation_point_info_meta : (_, _) invocation_point_info -> invocation_point_meta

val to_invocation_point_additional_info : ('reqty, 'respty) invocation_point_info -> ('reqty, 'respty) invocation_point_additional_info

val to_invocation_point_info : invocation_point_meta -> ('reqty, 'respty) invocation_point_additional_info -> ('reqty, 'respty) invocation_point_info

type invocation_point_collection = untyped_invocation_point_info list

type registry_info = invocation_point_collection * type_decl_collection

val tdenv_of_registry_info : registry_info -> tdenv

module type ApiDirManifest = sig
  val registry_info : unit -> registry_info
end

module MergedApiDirManifest : functor (_ : ApiDirManifest) (_ : ApiDirManifest) -> ApiDirManifest

module type RegistryInfo = sig
  type nonrec ('reqty, 'respty) invocation_point_info = ('reqty, 'respty) invocation_point_info
  type nonrec registry_info = registry_info
  include ApiDirManifest
end

module type MakeRegistryS = sig
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
    -> ?resp_samples:(('respty * http_status) with_doc list)
    -> urlpath:string
    -> resp_type:('respty typed_type_decl)
    -> string (* name of the invocation point *)
    -> (unit, 'respty) invocation_point_info

  val register_get' :
    ?summary:string
    -> ?description:string
    -> ?resp_samples:(('respty * http_status) with_doc list)
    -> urlpath:string
    -> string (** name of the invocation point *)
    -> 'respty response_case list
    -> (unit, 'respty) invocation_point_info

  val register_post :
    ?summary:string
    -> ?description:string
    -> ?req_name:string
    -> ?req_doc:string
    -> ?req_samples:('reqty with_doc list)
    -> ?resp_name:string
    -> ?resp_doc:string
    -> ?resp_samples:(('respty * http_status) with_doc list)
    -> ?usage_samples:(('reqty * 'respty * http_status) with_doc list)
    -> urlpath:string
    -> req_type:('reqty typed_type_decl)
    -> resp_type:('respty typed_type_decl)
    -> string (* name of the invocation point *)
    -> ('reqty, 'respty) invocation_point_info

  val register_post' :
    ?summary:string
    -> ?description:string
    -> ?req_name:string
    -> ?req_doc:string
    -> ?req_samples:('reqty with_doc list)
    -> ?resp_samples:(('respty * http_status) with_doc list)
    -> ?usage_samples:(('reqty * 'respty * http_status) with_doc list)
    -> urlpath:string
    -> req_type:('reqty typed_type_decl)
    -> string (** name of the invocation point *)
    -> 'respty response_case list
    -> ('reqty, 'respty) invocation_point_info

  val register_response_samples :
    ('respty * http_status) with_doc list
    -> (_, 'respty) invocation_point_info
    -> unit

  val register_response_sample :
    ?doc:string
    -> ?status:http_status
    -> 'respty
    -> (_, 'respty) invocation_point_info
    -> unit

  val register_request_samples :
    'reqty with_doc list
    -> ('reqty, _) invocation_point_info
    -> unit

  val register_request_sample :
    ?doc:string
    -> 'reqty
    -> ('reqty, _) invocation_point_info
    -> unit

  val register_usage_samples :
    ('reqty * 'respty * http_status) with_doc list
    -> ('reqty, 'respty) invocation_point_info
    -> unit

  val register_usage_sample :
    ?doc:string
    -> ?status:http_status
    -> req:'reqty
    -> resp:'respty
    -> ('reqty, 'respty) invocation_point_info
    -> unit

  val register_usage_sample' :
    ?doc:string
    -> ?status:http_status
    -> ('reqty * 'respty)
    -> ('reqty, 'respty) invocation_point_info
    -> unit

  module Public : RegistryInfo
end

module MakeRegistry () : MakeRegistryS
