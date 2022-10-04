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

type t

type api_key_location = Query | Header | Cookie

type oauth_flows_object

type implicit_oauth_flow_object

type password_oauth_flow_object

type clientCredentials_oauth_flow_object

type authorizationCode_oauth_flow_object

val apiKey : ?description:string -> string -> api_key_location -> t

val http : ?description:string -> ?bearerFormat:string -> string -> t

val oauth2 : ?description:string -> oauth_flows_object -> t

val openIdConnect : ?description:string -> ?openIdConnectUrl:string -> unit -> t

val flows :
  ?implicit:implicit_oauth_flow_object
  -> ?password:password_oauth_flow_object
  -> ?clientCredentials:clientCredentials_oauth_flow_object
  -> ?authorizationCode:authorizationCode_oauth_flow_object
  -> unit -> oauth_flows_object

val implicit_flow :
  ?refreshUrl:string ->
  (string * string) list ->
  string ->
  implicit_oauth_flow_object

val password_flow :
  ?refreshUrl:string ->
  (string * string) list ->
  string ->
  password_oauth_flow_object

val clientCredentials_flow :
  ?refreshUrl:string
  -> (string * string) list
  -> string
  -> clientCredentials_oauth_flow_object

val authorizationCode_flow :
  ?refreshUrl:string
  -> (string * string) list
  -> string
  -> string
  -> authorizationCode_oauth_flow_object

val pp : ppf -> t -> unit

val to_json : t -> jv

val yojson_of_t : t -> yojson
