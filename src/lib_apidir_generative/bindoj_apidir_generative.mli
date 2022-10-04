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
open Bindoj_apidir_shared
module OpenApi = Bindoj_openapi.V3

val gen_openapi_document_object :
  title:string -> version:string -> registry_info -> OpenApi.Document_object.t

module Internals : sig
  val openapi_components_object_of_type_decl_collection :
    registry_info -> OpenApi.Components_object.t

  val openapi_paths_object_of_invocation_point_collection :
    registry_info -> OpenApi.Path_item_object.paths_object

  val openapi_path_item_object_of_invocation_point_info :
    registry_info -> ('reqty, 'respty) invocation_point_info -> OpenApi.Path_item_object.t

  val openapi_request_body_object_of_request_body :
    registry_info -> 't request_body -> OpenApi.Request_body_object.t

  val openapi_response_object_of_response :
    registry_info -> 't response -> OpenApi.Response_object.t

  val openapi_header_object_of_header :
    registry_info -> 't header -> OpenApi.Header_object.t

  val openapi_media_type_object_of_media_type :
    registry_info -> 't media_type -> OpenApi.Header_object.media_type_object

  val openapi_external_documentation_object_of_external_doc :
    external_doc -> OpenApi.External_documentation_object.t

  val openapi_schema_object_of_type_decl : Bindoj_base.Type_desc.type_decl -> OpenApi.Schema_object.t

  val openapi_schema_or_reference_of_type_decl :
    registry_info -> Bindoj_base.Type_desc.type_decl -> (OpenApi.Schema_object.t, OpenApi.Reference_object.t) either
end
