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

type t

val mk :
  ?schemas:(string * (Schema_object.t, Reference_object.t) either) list
  -> ?responses:(string * (Response_object.t, Reference_object.t) either) list
  -> ?parameters:(string * (Parameter_object.t, Reference_object.t) either) list
  -> ?examples:(string * (Example_object.t, Reference_object.t) either) list
  -> ?requestBodies:(string * (Request_body_object.t, Reference_object.t) either) list
  -> ?headers:(string * (Header_object.t, Reference_object.t) either) list
  -> ?securitySchemes:(string * (Security_scheme_object.t, Reference_object.t) either) list
  -> ?links:(string * (Link_object.t, Reference_object.t) either) list
  -> ?callbacks:(string * (Path_item_object.callback_object, Reference_object.t) either) list
  -> unit -> t

val pp : ppf -> t -> unit

val to_json : t -> jv

val yojson_of_t : t -> yojson
