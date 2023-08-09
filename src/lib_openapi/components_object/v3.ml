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
open Bindoj_openapi_util.V3

module Path_item_object = Bindoj_openapi_path_item_object.V3
module Parameter_object = Bindoj_openapi_parameter_object.V3
module Request_body_object = Bindoj_openapi_request_body_object.V3
module Response_object = Bindoj_openapi_response_object.V3
module Example_object = Bindoj_openapi_example_object.V3
module Link_object = Bindoj_openapi_link_object.V3
module Header_object = Bindoj_openapi_header_object.V3
module Reference_object = Bindoj_openapi_reference_object.V3
module Schema_object = Bindoj_openapi_schema_object.V3
module Security_scheme_object = Bindoj_openapi_security_scheme_object.V3

type t = {
  schemas : (Schema_object.t, Reference_object.t) either assoc option [@yojson.option];
  responses : (Response_object.t, Reference_object.t) either assoc option [@yojson.option];
  parameters :(Parameter_object.t, Reference_object.t) either assoc option [@yojson.option];
  examples : (Example_object.t, Reference_object.t) either assoc option [@yojson.option];
  requestBodies : (Request_body_object.t, Reference_object.t) either assoc option [@yojson.option];
  headers : (Header_object.t, Reference_object.t) either assoc option [@yojson.option];
  securitySchemes : (Security_scheme_object.t, Reference_object.t) either assoc option [@yojson.option];
  links : (Link_object.t, Reference_object.t) either assoc option [@yojson.option];
  callbacks : (Path_item_object.callback_object, Reference_object.t) either assoc option [@yojson.option];
} [@@deriving show, yojson_of]

let mk ?schemas ?responses ?parameters ?examples ?requestBodies
    ?headers ?securitySchemes ?links ?callbacks () = {
  schemas = schemas;
  responses = responses;
  parameters = parameters;
  examples = examples;
  requestBodies = requestBodies;
  headers = headers;
  securitySchemes = securitySchemes;
  links = links;
  callbacks = callbacks;
}

let to_json : t -> jv = fun t ->
  t |> yojson_of_t |> Json.of_yojson
