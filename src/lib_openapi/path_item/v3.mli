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
open Bindoj_openapi_util.V3

module Server_object = Bindoj_openapi_server_object.V3
module External_documentation_object = Bindoj_openapi_external_documentation_object.V3
module Parameter_object = Bindoj_openapi_parameter_object.V3
module Request_body_object = Bindoj_openapi_request_body_object.V3
module Response_object = Bindoj_openapi_response_object.V3
module Reference_object = Bindoj_openapi_reference_object.V3
module Security_requirement_object = Bindoj_openapi_security_requirement_object.V3

type t

type operation_object

type callback_object

type paths_object

val mk :
  ?ref:string
  -> ?summary:string
  -> ?description:string
  -> ?get:operation_object
  -> ?put:operation_object
  -> ?post:operation_object
  -> ?delete:operation_object
  -> ?options:operation_object
  -> ?head:operation_object
  -> ?patch:operation_object
  -> ?trace:operation_object
  -> ?servers:Server_object.t list
  -> ?parameters:Parameter_object.t list
  -> unit -> t

val paths : (string * t) list -> paths_object

val operation :
  ?tags:string list
  -> ?summary:string
  -> ?description:string
  -> ?externalDocs:External_documentation_object.t
  -> ?operationId:string
  -> ?parameters:(Parameter_object.t, Reference_object.t) either list
  -> ?requestBody:(Request_body_object.t, Reference_object.t) either
  -> ?callbacks:(string * (callback_object, Reference_object.t) either) list
  -> ?deprecated:bool
  -> ?security:Security_requirement_object.t list
  -> ?servers:Server_object.t list
  -> Response_object.responses_object
  -> operation_object

val callback : (string * t) list -> callback_object

val pp : ppf -> t -> unit

val to_json : t -> jv

val yojson_of_t : t -> yojson

val pp_paths_object : ppf -> paths_object -> unit

val paths_object_to_json : paths_object -> jv

val yojson_of_paths_object : paths_object -> yojson

val pp_operation_object : ppf -> operation_object -> unit

val operation_object_to_json : operation_object -> jv

val yojson_of_operation_object : operation_object -> yojson

val pp_callback_object : ppf -> callback_object -> unit

val callback_object_to_json : callback_object -> jv

val yojson_of_callback_object : callback_object -> yojson
