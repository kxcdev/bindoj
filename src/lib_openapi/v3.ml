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
module Info_object = Bindoj_openapi_info_object.V3
module Server_object = Bindoj_openapi_server_object.V3
module Components_object = Bindoj_openapi_components_object.V3
module Path_item_object = Bindoj_openapi_path_item_object.V3
module External_documentation_object = Bindoj_openapi_external_documentation_object.V3
module Parameter_object = Bindoj_openapi_parameter_object.V3
module Request_body_object = Bindoj_openapi_request_body_object.V3
module Response_object = Bindoj_openapi_response_object.V3
module Example_object = Bindoj_openapi_example_object.V3
module Link_object = Bindoj_openapi_link_object.V3
module Header_object = Bindoj_openapi_header_object.V3
module Tag_object = Bindoj_openapi_tag_object.V3
module Reference_object = Bindoj_openapi_reference_object.V3
module Schema_object = Bindoj_openapi_schema_object.V3
module Security_scheme_object = Bindoj_openapi_security_scheme_object.V3
module Security_requirement_object = Bindoj_openapi_security_requirement_object.V3
module Style_value = Bindoj_openapi_style_value.V3

module Document_object : sig
  open Bindoj_openapi_util.V3

  module Info_object = Bindoj_openapi_info_object.V3
  module Server_object = Bindoj_openapi_server_object.V3
  module Components_object = Bindoj_openapi_components_object.V3
  module Path_item_object = Bindoj_openapi_path_item_object.V3
  module External_documentation_object = Bindoj_openapi_external_documentation_object.V3
  module Tag_object = Bindoj_openapi_tag_object.V3
  module Security_requirement_object = Bindoj_openapi_security_requirement_object.V3

  type t

  val mk :
    ?servers:Server_object.t list
    -> ?components:Components_object.t
    -> ?security:Security_requirement_object.t list
    -> ?tags:Tag_object.t list
    -> ?externalDocs:External_documentation_object.t
    -> string
    -> Info_object.t
    -> Path_item_object.paths_object
    -> t

  val to_json : t -> jv

  val pp : ppf -> t -> unit
end = struct
  open Bindoj_openapi_util.V3

  module Info_object = Bindoj_openapi_info_object.V3
  module Server_object = Bindoj_openapi_server_object.V3
  module Components_object = Bindoj_openapi_components_object.V3
  module Path_item_object = Bindoj_openapi_path_item_object.V3
  module External_documentation_object = Bindoj_openapi_external_documentation_object.V3
  module Tag_object = Bindoj_openapi_tag_object.V3
  module Security_requirement_object = Bindoj_openapi_security_requirement_object.V3

  (* https://spec.openapis.org/oas/v3.0.3#openapi-object *)
  type t = {
    openapi : string;
    info : Info_object.t;
    servers : Server_object.t list option [@yojson.option];
    paths : Path_item_object.paths_object;
    components : Components_object.t option [@yojson.option];
    security : Security_requirement_object.t list option [@yojson.option];
    tags : Tag_object.t list option [@yojson.option];
    externalDocs : External_documentation_object.t option [@yojson.option];
  } [@@deriving show, yojson_of]

  let mk ?servers ?components ?security ?tags ?externalDocs openapi info paths = {
    openapi = openapi;
    info = info;
    servers = servers;
    paths = paths;
    components = components;
    security = security;
    tags = tags;
    externalDocs = externalDocs;
  }

  let to_json : t -> jv = fun t ->
    t |> yojson_of_t |> Json.of_yojson
end
