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
open Bindoj_runtime
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

and ('reqty, 'respty) invp_usage_sample =
  | Req_sample of 'reqty with_doc
  | Resp_sample of ('respty * http_status) with_doc
  | Usage_sample of ('reqty * 'respty * http_status) with_doc

module InvpUsageSamples = struct
  let req_sample x = Req_sample(x)
  let resp_sample x = Resp_sample(x)
  let usage_sample x = Usage_sample(x)
end

let make_response_case ?(status=`default) ?name ?doc ?(samples=[]) ~pack ~unpack resp_type =
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
      mt_external_examples = [];
    };
    rs_description = resp_doc;
    rs_headers = [];
  } in
  Response_case {
    name = resp_name; doc = resp_doc; samples;
    status; response; pack; unpack
  }

type invocation_point_meta = {
  ipm_name : string;
  ipm_urlpath : string;
  ipm_method : [ `get | `post ];
}

type ('reqty, 'respty) invocation_point_additional_info = {
  mutable ipa_request_body : 'reqty request_body option;
  mutable ipa_responses : 'respty response_case list;
  mutable ipa_deprecated : bool;
  mutable ipa_summary : string option;
  mutable ipa_description : string option;
  mutable ipa_external_doc : external_doc option;
  mutable ipa_usage_samples : ('reqty, 'respty) invp_usage_sample list;
}

type untyped_invocation_point_info =
| Invp : (_, _) invocation_point_info -> untyped_invocation_point_info

type untyped_invocation_point_additional_info =
  | InvpAdditionalInfo : (_, _) invocation_point_additional_info -> untyped_invocation_point_additional_info

let to_invocation_point_info_meta invp =
  ({  ipm_name = invp.ip_name;
      ipm_urlpath = invp.ip_urlpath;
      ipm_method = invp.ip_method })

let to_invocation_point_additional_info invp =
  { ipa_request_body = invp.ip_request_body;
    ipa_responses = invp.ip_responses;
    ipa_deprecated = invp.ip_deprecated;
    ipa_summary = invp.ip_summary;
    ipa_description = invp.ip_description;
    ipa_external_doc = invp.ip_external_doc;
    ipa_usage_samples = invp.ip_usage_samples }

let to_invocation_point_info invp_meta invpa =
  { ip_name = invp_meta.ipm_name;
    ip_urlpath = invp_meta.ipm_urlpath;
    ip_method = invp_meta.ipm_method;
    ip_request_body = invpa.ipa_request_body;
    ip_responses = invpa.ipa_responses;
    ip_deprecated = invpa.ipa_deprecated;
    ip_summary = invpa.ipa_summary;
    ip_description = invpa.ipa_description;
    ip_external_doc = invpa.ipa_external_doc;
    ip_usage_samples = invpa.ipa_usage_samples }

type invocation_point_collection = untyped_invocation_point_info list

type registry_info = invocation_point_collection * type_decl_collection

let tdenv_of_registry_info registry_info =
  let open Bindoj_base in
  let open Typed_type_desc in
  let _, {
      type_declarations;
      type_decl_environment_wrappers
    } = registry_info in
  let alias_ident_typemap =
    type_declarations
    |> foldl (fun acc info ->
           acc |> StringMap.add info.tdi_name info.tdi_decl
         ) StringMap.empty in
  let env0 = Type_decl_environment.{
        alias_ident_typemap;
        prim_ident_typemap = StringMap.empty;
             } in
  type_decl_environment_wrappers |> List.foldl (|>) env0

module type ApiDirManifest = sig
  val registry_info : unit -> registry_info
end

module MergedApiDirManifest (A : ApiDirManifest) (B : ApiDirManifest) : ApiDirManifest = struct
  let registry_info() =
    let destruct_tc { type_declarations; type_decl_environment_wrappers } =
      type_declarations, type_decl_environment_wrappers in
    let invps1, ( tds1, tenv1 ) = A.registry_info() |> ?> destruct_tc in
    let invps2, ( tds2, tenv2 ) = B.registry_info() |> ?> destruct_tc in
    let invps = invps1 @ invps2 in
    let type_decl_environment_wrappers = tenv1 @ tenv2 in
    let type_declarations =
      List.sort_uniq (projected_compare (fun tdi -> tdi.tdi_name))
        (List.rev_append tds2 tds1) in
    invps, { type_declarations; type_decl_environment_wrappers }
end


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
    -> ?resp_samples : (('respty * http_status) with_doc list)
    -> urlpath:string
    -> resp_type:('respty typed_type_decl)
    -> string (** name of the invocation point *)
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
    -> string (** name of the invocation point *)
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

module MakeRegistry () : MakeRegistryS = struct
  let invp_meta_registry : invocation_point_meta list ref = ref []
  let invp_registry : (invocation_point_meta, untyped_invocation_point_additional_info) Hashtbl.t = Hashtbl.create 0
  let type_registry : type_decl_info list ref = ref []
  let tdenv_wrappers : tdenv endo list ref = ref []

  let register :
    type_decl_info list
    -> ('reqty, 'respty) invocation_point_info
    -> unit =
    fun typs invp ->
    let invp_meta = to_invocation_point_info_meta invp in
    Hashtbl.replace invp_registry invp_meta (InvpAdditionalInfo (to_invocation_point_additional_info invp));
    refappend invp_meta_registry invp_meta;
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
    -> ?resp_samples:(('respty * http_status) with_doc list)
    -> urlpath:string
    -> string (* name *)
    -> 'respty response_case list
    -> (unit, 'respty) invocation_point_info =
    fun ?summary ?description ?(resp_samples = []) ~urlpath name responses ->
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
      ip_usage_samples = resp_samples |&> InvpUsageSamples.resp_sample
    } in
    let decls =
      responses |> List.map (function Response_case { name; doc; response; _ } ->
        type_decl_info_of_typed_decl name doc response.rs_media_type.mt_type
      )
    in
    register decls invp; invp

  let register_get ?summary ?description ?resp_name ?resp_doc ?resp_samples ~urlpath ~resp_type name =
    let resp_case =
      make_response_case ?name:resp_name ?doc:resp_doc
        ~pack:(fun x -> x) ~unpack:(fun x -> Some x) resp_type
    in
    register_get' ?summary ?description ?resp_samples ~urlpath name [resp_case]

  let register_post' :
    ?summary:string
    -> ?description:string
    -> ?req_name:string
    -> ?req_doc:string
    -> ?req_samples:('reqty with_doc list)
    -> ?resp_samples:(('respty * http_status) with_doc list)
    -> ?usage_samples:(('reqty * 'respty * http_status) with_doc list)
    -> urlpath:string
    -> req_type:('reqty typed_type_decl)
    -> string (* name *)
    -> 'respty response_case list
    -> ('reqty, 'respty) invocation_point_info =
    fun ?summary ?description ?req_name ?req_doc
      ?(req_samples=[]) ?(resp_samples=[]) ?(usage_samples = [])
      ~urlpath ~req_type name responses ->
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
      ip_usage_samples =
        (req_samples |&> InvpUsageSamples.req_sample)
        @ (resp_samples |&> InvpUsageSamples.resp_sample)
        @ (usage_samples |&> InvpUsageSamples.usage_sample)
    } in
    let decls =
      responses |> List.map (function Response_case { name; doc; response; _ } ->
        type_decl_info_of_typed_decl name doc response.rs_media_type.mt_type
      )
    in
    register (type_decl_info_of_typed_decl req_name req_doc req_type :: decls) invp;
    invp

  let register_post ?summary ?description ?req_name ?req_doc ?req_samples ?resp_name ?resp_doc ?resp_samples ?usage_samples ~urlpath ~req_type ~resp_type name =
    let resp_case =
      make_response_case ?name:resp_name ?doc:resp_doc
        ~pack:(fun x -> x) ~unpack:(fun x -> Some x) resp_type
    in
    register_post' ?summary ?description ?req_name ?req_doc ?req_samples ?resp_samples ?usage_samples ~urlpath ~req_type name [resp_case]

  let update_usage_samples :
    ('reqty, 'respty) invp_usage_sample list
    -> ('reqty, 'respty) invocation_point_info
    -> unit =
    fun samples invp ->
    Hashtbl.find_opt
      invp_registry
      (to_invocation_point_info_meta invp)
    |> Option.iter (fun (InvpAdditionalInfo invpa) ->
      let invpa = (Obj.magic invpa : ('reqty, 'respty) invocation_point_additional_info) in
      invpa.ipa_usage_samples <- samples @ invpa.ipa_usage_samples
    )

  let register_response_samples resp_samples =
    update_usage_samples (resp_samples |&> InvpUsageSamples.resp_sample)

  let register_response_sample ?doc ?(status = `default) resp_sample =
    update_usage_samples [ InvpUsageSamples.resp_sample ((resp_sample, status), doc_of_string_opt doc) ]

  let register_request_samples req_samples =
    update_usage_samples (req_samples |&> InvpUsageSamples.req_sample)

  let register_request_sample ?doc req_sample =
    update_usage_samples [ InvpUsageSamples.req_sample (req_sample, doc_of_string_opt doc) ]

  let register_usage_samples usage_samples =
    update_usage_samples (usage_samples |&> InvpUsageSamples.usage_sample)

  let register_usage_sample ?doc ?(status = `default) ~req ~resp =
    update_usage_samples [ InvpUsageSamples.usage_sample ((req, resp, status), doc_of_string_opt doc) ]

  let register_usage_sample' ?doc ?status (req, resp) = register_usage_sample ?doc ?status ~req ~resp

  module Public = struct
    type nonrec ('reqty, 'respty) invocation_point_info = ('reqty, 'respty) invocation_point_info
    type registry_info = invocation_point_collection * type_decl_collection
    let registry_info () : registry_info =
      let tdcoll = {
          type_declarations = !type_registry;
          type_decl_environment_wrappers = !tdenv_wrappers;
        } in
      let invp_collection : invocation_point_collection =
        !invp_meta_registry
        |> List.rev_map (fun invp_meta ->
          let InvpAdditionalInfo(invp_additional) = Hashtbl.find invp_registry invp_meta in
          Invp (to_invocation_point_info invp_meta invp_additional))
      in
      (invp_collection, tdcoll)
  end
end
