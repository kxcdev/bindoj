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

module Example_object = Bindoj_openapi_example_object.V3
module Header_object = Bindoj_openapi_header_object.V3
module Reference_object = Bindoj_openapi_reference_object.V3
module Schema_object = Bindoj_openapi_schema_object.V3
module Style_value = Bindoj_openapi_style_value.V3

type parameter_location = Query | Header | Path | Cookie [@@deriving show]

let yojson_of_parameter_location : parameter_location -> yojson = function
  | Query -> `String "query"
  | Header -> `String "header"
  | Path -> `String "path"
  | Cookie -> `String "cookie"

type t = {
  name : string;
  in_ : parameter_location;
  description : string option [@yojson.option];
  required : bool option [@yojson.option]; (* If the in_ is Path, this property must be Some true *)
  deprecated : bool option [@yojson.option];
  allow_empty_value : bool option [@yojson.option];
  style : Style_value.t option [@yojson.option];
  explode : bool option [@yojson.option];
  allow_reserved : bool option [@yojson.option];
  schema : (Schema_object.t, Reference_object.t) either option [@yojson.option];
  example : jv option [@yojson.option];
  examples : (Example_object.t, Reference_object.t) either assoc option [@yojson.option];
  content : Header_object.media_type_object assoc option [@yojson.option];
} [@@deriving show, yojson_of]

let yojson_of_t t =
  t |> yojson_of_t |> function
  | `Assoc fields -> `Assoc (fields |&> function
      | ("in_", fvalue) -> ("in", fvalue)
      | x -> x)
  | t -> t

let mk
    ?description ?required ?deprecated
    ?allow_empty_value ?style ?explode
    ?allow_reserved ?schema ?example
    ?examples ?content
    name in_ = {
  name = name;
  in_ = in_;
  description = description;
  required = required;
  deprecated = deprecated;
  allow_empty_value = allow_empty_value;
  style = style;
  explode = explode;
  allow_reserved = allow_reserved;
  schema = schema;
  example = example;
  examples = examples;
  content = content;
}

let to_json : t -> jv = fun t ->
  t |> yojson_of_t |> Json.of_yojson
