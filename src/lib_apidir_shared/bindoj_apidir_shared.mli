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

  (** TODO.future - allow specifying multiple response types #216 *)
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
    -> string (* name of the invocation point *)
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
    -> string (* name of the invocation point *)
    -> ('reqty, 'respty) invocation_point_info

  module Public : RegistryInfo
end

module MakeRegistry () : MakeRegistryS
