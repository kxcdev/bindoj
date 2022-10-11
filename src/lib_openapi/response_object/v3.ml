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

module Link_object = Bindoj_openapi_link_object.V3
module Header_object = Bindoj_openapi_header_object.V3
module Reference_object = Bindoj_openapi_reference_object.V3

type t = {
  description : string;
  headers : (Header_object.t, Reference_object.t) either assoc option [@yojson.option];
  content : Header_object.media_type_object assoc option [@yojson.option];
  links : (Link_object.t, Reference_object.t) either assoc option [@yojson.option];
} [@@deriving show, yojson_of]

type responses_object_key = [
  | `default
  | `status_range of [`_1XX | `_2XX | `_3XX | `_4XX | `_5XX]
  | `status_code of int
] [@@deriving show]

type responses_object = (responses_object_key * (t, Reference_object.t) either) list [@@deriving show]

let yojson_of_responses_object : responses_object -> yojson = fun resps ->
  let yojson_of_resp resp = yojson_of_either yojson_of_t Reference_object.yojson_of_t resp in
  let key_of_range = function
    | `_1XX -> "1XX"
    | `_2XX -> "2XX"
    | `_3XX -> "3XX"
    | `_4XX -> "4XX"
    | `_5XX -> "5XX"
  in
  `Assoc (resps |&> function
      | (`default, resp) -> ("default", yojson_of_resp resp)
      | (`status_code n, resp) -> (string_of_int n, yojson_of_resp resp)
      | (`status_range r, resp) -> (key_of_range r, yojson_of_resp resp))

let mk ?headers ?content ?links description = {
  description = description;
  headers = headers;
  content = content;
  links = links;
}

let to_json : t -> jv = fun t ->
  t |> yojson_of_t |> Json.of_yojson

let responses_object_to_json : responses_object -> jv = fun t ->
  t |> yojson_of_responses_object |> Json.of_yojson
