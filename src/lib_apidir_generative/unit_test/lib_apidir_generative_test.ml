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
open Bindoj_apidir_generative
open Bindoj_apidir_generative.Internals
open Bindoj_apidir_shared
module ExD = Bindoj_test_common.Typedesc_examples
module ExG = Bindoj_test_common.Typedesc_generated_examples
module OpenApi = Bindoj_openapi.V3

let testable_openapi_document_object =
  Alcotest.testable OpenApi.Document_object.pp ( = )

let testable_openapi_paths_object =
  Alcotest.testable OpenApi.Path_item_object.pp_paths_object ( = )

let testable_openapi_path_item_object =
  Alcotest.testable OpenApi.Path_item_object.pp ( = )

let testable_openapi_request_body_object =
  Alcotest.testable OpenApi.Request_body_object.pp ( = )

let testable_openapi_response_object =
  Alcotest.testable OpenApi.Response_object.pp ( = )

let testable_openapi_header_object =
  Alcotest.testable OpenApi.Header_object.pp ( = )

let testable_openapi_media_type_object =
  Alcotest.testable OpenApi.Header_object.pp_media_type_object ( = )

let testable_openapi_external_documentation_object =
  Alcotest.testable OpenApi.External_documentation_object.pp ( = )

module type Ex = sig
  val name : string
  val decl : type_decl
  type typ
  val typed : typ typed_type_decl
  val env : tdenv
  val sample_values : typ with_doc list
  val sample_jsons : (string * Json.jv) list
end

let all =
  List.map2 (fun (name, d) (name', g) ->
      assert (name = name');
      let module D = (val d : ExD.T) in
      let module G = (val g : ExG.T) in
      (module struct
        let name = name
        let decl = D.decl
        type typ = G.t
        let typed = Typed.mk decl G.reflect
        let env = G.env
        let sample_values =
          List.mapi (fun i ExG.Sample_value.{ orig; _; } ->
              (orig, `docstr ("ex" ^ string_of_int i)))
            G.sample_values
        let sample_jsons =
          List.mapi (fun i ExG.Sample_value.{ jv; _; } ->
              ("ex" ^ string_of_int i, jv))
          G.sample_values
      end : Ex))
    ExD.all ExG.all

let content_type = "application/json"

let create_cases ex =
  let module Ex = (val ex : Ex) in
  let check doc testable a b () =
    Alcotest.check testable doc a b in
  let test_case' doc testable a b =
    Alcotest.test_case
      (doc ^ " works")
      `Quick
      (check doc testable a b) in

  let name = Ex.name in
  let url = "https://example.com" in
  let get_name = "get-" ^ name in
  let post_name = "post-" ^ name in
  let get_path = "/" ^ get_name in
  let post_path = "/" ^ post_name in
  let get_urlpath = url ^ get_path in
  let post_urlpath = url ^ post_path in
  let get_invp_name = "get" in
  let post_invp_name = "post" in

  let registry_info =
    let module R = MakeRegistry () in
    let types = ref [] in
    let _ =
      StringMap.iter (fun name boxed ->
          if List.mem name !types then ()
          else begin
            R.register_type_decl_info ~name (Typed.unbox boxed);
            refappend types name
          end)
        Ex.env.alias_ident_typemap in
    let _ =
      R.register_post post_invp_name
        ~urlpath:post_urlpath
        ~req_name:post_name
        ~req_type:Ex.typed
        ~resp_name:post_name
        ~resp_type:Ex.typed in
    let _ =
      R.register_get get_invp_name
        ~urlpath:get_urlpath
        ~resp_name:get_name
        ~resp_type:Ex.typed in
    let _ =
      R.add_type_decl_environment_wrapper
        Bindoj_std.Tdenv_wrappers.json in
    R.Public.registry_info () /< List.rev in

  let invocation_point_collection_cases =
    let title = "document_title" in
    let version = "0.0.1" in
    let openapi_version = "3.0.3" in

    let paths_object =
      OpenApi.Path_item_object.paths [
        get_path,
        OpenApi.Path_item_object.mk
          ~summary:get_invp_name
          ~get:(OpenApi.Path_item_object.operation
                  ~summary:get_invp_name
                  ~deprecated:false
                  [ `default,
                    OpenApi.Response_object.mk
                      ~content:([content_type,
                                 OpenApi.Header_object.media_type
                                   ~examples:[]
                                   ~schema:(Internals.openapi_schema_or_reference_of_type_decl registry_info Ex.decl)
                                   ()])
                      get_name
                    |> Either.left ])
          ~servers:[ OpenApi.Server_object.mk url ]
          ();
        post_path,
        OpenApi.Path_item_object.mk
          ~summary:post_invp_name
          ~post:(OpenApi.Path_item_object.operation
                   ~summary:post_invp_name
                   ~deprecated:false
                   ~requestBody:(OpenApi.Request_body_object.mk
                                   ~description:post_name
                                   ~required:false
                                   [content_type,
                                    OpenApi.Header_object.media_type
                                      ~examples:[]
                                      ~schema:(Internals.openapi_schema_or_reference_of_type_decl registry_info Ex.decl)
                                      ()]
                                 |> Either.left)
                   [ `default,
                     OpenApi.Response_object.mk
                       ~content:([content_type,
                                  OpenApi.Header_object.media_type
                                    ~examples:[]
                                    ~schema:(Internals.openapi_schema_or_reference_of_type_decl registry_info Ex.decl)
                                    ()])
                       post_name
                     |> Either.left ])
          ~servers:[ OpenApi.Server_object.mk url ]
          ();
      ] in
    let document_object =
      OpenApi.Document_object.mk
        ~components:(Internals.openapi_components_object_of_type_decl_collection registry_info)
        openapi_version
        (OpenApi.Info_object.mk title version)
        paths_object in
    [ test_case'
        "gen_openapi_document_object"
        testable_openapi_document_object
        document_object
        (registry_info |> gen_openapi_document_object ~title ~version);
      test_case'
        "openapi_paths_object_of_invocation_point_collection"
        testable_openapi_paths_object
        paths_object
        (registry_info |> openapi_paths_object_of_invocation_point_collection) ] in

  let invocation_point_info_cases =
    let resp_desc = "response description in invocation_point_info" in
    let resp_status = `default in
    let resp =
      make_response_case ~status:resp_status ~doc:resp_desc ~pack:(fun x -> x) ~unpack:some Ex.typed
    in
    let invocation_point_info =
      { ip_name = name;
        ip_urlpath = url ^ get_path;
        ip_method = `get;
        ip_request_body = None;
        ip_responses = [resp];
        ip_deprecated = false;
        ip_summary = None;
        ip_description = None;
        ip_external_doc = None;
        ip_usage_samples = [] } in
    let path_item_object =
      OpenApi.Path_item_object.mk
        ~summary:name
        ~get:(OpenApi.Path_item_object.operation
                ~summary:name
                ~deprecated:false
                [ resp_status,
                  (OpenApi.Response_object.mk
                     ~content:([content_type,
                                OpenApi.Header_object.media_type
                                  ~examples:[]
                                  ~schema:(Internals.openapi_schema_or_reference_of_type_decl registry_info Ex.decl)
                                  ()])
                     resp_desc
                   |> Either.left); ])
        ~servers:[ OpenApi.Server_object.mk url ]
        () in
    [ test_case'
      "openapi_path_item_object_of_invocation_point_info"
      testable_openapi_path_item_object
      (openapi_path_item_object_of_invocation_point_info registry_info invocation_point_info)
      path_item_object ] in

  let request_body_cases = [
    test_case'
      "openapi_request_body_object_of_request_body"
      testable_openapi_request_body_object
      ({ rq_media_type = { mt_type = Ex.typed;
                           mt_external_examples = []; };
         rq_description = "request body description";
         rq_required = false; }
       |> openapi_request_body_object_of_request_body registry_info [])
      (OpenApi.Request_body_object.mk
         ~description:"request body description"
         ~required:false
         [ content_type,
           OpenApi.Header_object.media_type
             ~examples:[]
             ~schema:(Internals.openapi_schema_or_reference_of_type_decl registry_info Ex.decl)
             () ])
  ] in

  let response_cases = [
    test_case'
      "openapi_response_object_of_response"
      testable_openapi_response_object
      ({ rs_media_type = { mt_type = Ex.typed;
                           mt_external_examples = []; };
         rs_description = "";
         rs_headers = []; }
       |> openapi_response_object_of_response registry_info [])
      (OpenApi.Response_object.mk
         ~content:[(content_type,
                    OpenApi.Header_object.media_type
                      ~examples:[]
                      ~schema:(Internals.openapi_schema_or_reference_of_type_decl registry_info Ex.decl)
                      ())]
         "");
  ] in

  let header_cases = [
    (* https://spec.openapis.org/oas/v3.0.3#response-object *)
    let name = "X-Rate-Limit-Limit" in
    let desc = "The number of allowed requests in the current period" in
    test_case'
      "openapi_header_object_of_header"
      testable_openapi_header_object
      ({ hd_name = name;
         hd_description = desc;
         hd_required = false;
         hd_deprecated = false;
         hd_type_decl = Ex.typed; }
       |> openapi_header_object_of_header registry_info)
      (OpenApi.Header_object.mk
         ~description:desc
         ~required:false
         ~deprecated:false
         ~schema:(Internals.openapi_schema_or_reference_of_type_decl registry_info Ex.decl)
         ());
  ] in

  let media_type_cases = [
    test_case'
      "openapi_schema_or_reference_of_type_decl"
      testable_openapi_media_type_object
      ({ mt_type = Ex.typed ;
         mt_external_examples = []; }
       |> openapi_media_type_object_of_media_type registry_info [])
      (OpenApi.Header_object.media_type
         ~examples:[]
         ~schema:(Internals.openapi_schema_or_reference_of_type_decl registry_info Ex.decl)
         ());
    test_case'
      "openapi_schema_or_reference_of_type_decl"
      testable_openapi_media_type_object
      ({ mt_type = Ex.typed ;
         mt_external_examples = []; }
       |> openapi_media_type_object_of_media_type registry_info Ex.sample_values)
      (OpenApi.Header_object.media_type
         ~examples:(Ex.sample_jsons |&> fun (name, ex) ->
             (name, OpenApi.Example_object.mk ~value:ex () |> Either.left))
         ~schema:(Internals.openapi_schema_or_reference_of_type_decl registry_info Ex.decl)
         ());
  ] in

  let external_doc_cases = [
    (* https://spec.openapis.org/oas/v3.0.3#external-documentation-object *)
    test_case'
      "openapi_external_documentation_object_of_external_doc"
      testable_openapi_external_documentation_object
      ({ ed_urlpath = "https://example.com";
         ed_description = None; }
       |> openapi_external_documentation_object_of_external_doc)
      (OpenApi.External_documentation_object.mk "https://example.com");
  ] in

  [ Ex.name,
    invocation_point_collection_cases @ invocation_point_info_cases
    @ request_body_cases @ response_cases @ header_cases
    @ media_type_cases @ external_doc_cases ]

let try_create_cases ex =
  try
    create_cases ex
  with
    | Bindoj_gen.Json_codec.Incompatible_with_openapi_v3 _ ->
      let (module Ex) = ex in
      [Ex.name, [Alcotest.test_case "(skipped)" `Quick (fun () -> ())]]

let () =
  let open Alcotest in
  all
  |&> try_create_cases
  |> List.concat
  |> run "lib_apidir_generative"
