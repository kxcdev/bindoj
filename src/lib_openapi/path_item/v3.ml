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

module type Path_item_object_sig = sig
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
end

module Make_operation_object (Path_item_object : Path_item_object_sig) = struct
  type path_item_object = Path_item_object.t

  let pp_path_item_object = Path_item_object.pp

  type callback_object = (string * path_item_object) list [@@deriving show]

  let yojson_of_path_item_object = Path_item_object.yojson_of_t
  let yojson_of_callback_object callback =
    `Assoc (callback |&> fun (k, v) -> (k, yojson_of_path_item_object v))

  type t = {
    tags : string list option [@yojson.option];
    summary : string option [@yojson.option];
    description : string option [@yojson.option];
    externalDocs : External_documentation_object.t option [@yojson.option];
    operationId : string option [@yojson.option];
    parameters : (Parameter_object.t, Reference_object.t) either list option [@yojson.option];
    requestBody : (Request_body_object.t, Reference_object.t) either option [@yojson.option];
    responses : Response_object.responses_object;
    callbacks : (callback_object, Reference_object.t) either assoc option [@yojson.option];
    deprecated : bool option [@yojson.option];
    security : Security_requirement_object.t list option [@yojson.option];
    servers : Server_object.t list option [@yojson.option];
  } [@@deriving show, yojson_of]
end

module rec Path_item_object : Path_item_object_sig = struct
  module Operation_object = Make_operation_object (Path_item_object)

  type operation_object = Operation_object.t
  type callback_object = Operation_object.callback_object

  let pp_operation_object = Operation_object.pp
  let pp_callback_object = Operation_object.pp_callback_object

  let yojson_of_operation_object = Operation_object.yojson_of_t
  let yojson_of_callback_object = Operation_object.yojson_of_callback_object

  type t = {
    ref : string option [@yojson.option];
    summary : string option [@yojson.option];
    description : string option [@yojson.option];
    get : operation_object option [@yojson.option];
    put : operation_object option [@yojson.option];
    post : operation_object option [@yojson.option];
    delete : operation_object option [@yojson.option];
    options : operation_object option [@yojson.option];
    head : operation_object option [@yojson.option];
    patch : operation_object option [@yojson.option];
    trace : operation_object option [@yojson.option];
    servers : Server_object.t list option [@yojson.option];
    parameters : Parameter_object.t list option [@yojson.option];
  } [@@deriving show, yojson_of]

  let yojson_of_t t =
    let yojson = yojson_of_t t in
    match yojson with
    | `Assoc fields ->
      `Assoc (fields |&> fun (k, v) ->
          if k = "ref" then ("$ref", v) else (k, v))
    | _ -> failwith "impossible yojson"

  type paths_object = (string * t) list [@@deriving show]

  let yojson_of_paths_object paths =
    `Assoc (paths |&> fun (path, t) -> (path, yojson_of_t t))

  let mk ?ref ?summary ?description
      ?get ?put ?post ?delete
      ?options ?head ?patch ?trace
      ?servers ?parameters () = {
    ref = ref;
    summary = summary;
    description = description;
    get = get;
    put = put;
    post = post;
    delete = delete;
    options = options;
    head = head;
    patch = patch;
    trace = trace;
    servers = servers;
    parameters = parameters;
  }

  let paths = identity

  let operation
      ?tags ?summary ?description
      ?externalDocs ?operationId ?parameters ?requestBody
      ?callbacks ?deprecated ?security ?servers
      responses = Operation_object.{
      tags = tags;
      summary = summary;
      description = description;
      externalDocs = externalDocs;
      operationId = operationId;
      parameters = parameters;
      requestBody = requestBody;
      responses = responses;
      callbacks = callbacks;
      deprecated = deprecated;
      security = security;
      servers = servers;
    }

  let callback = identity

  let to_json t =
    t |> yojson_of_t |> Json.of_yojson

  let paths_object_to_json t =
    t |> yojson_of_paths_object |> Json.of_yojson

  let operation_object_to_json t =
    t |> yojson_of_operation_object |> Json.of_yojson

  let callback_object_to_json t =
    t |> yojson_of_callback_object |> Json.of_yojson
end

include Path_item_object
