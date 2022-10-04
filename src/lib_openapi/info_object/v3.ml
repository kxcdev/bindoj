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

module Contact_object = struct
  type t = {
    name : string option [@yojson.option];
    url : string option [@yojson.option];
    email : string option [@yojson.option];
  } [@@deriving show, yojson_of]
end

module License_object = struct
  type t = {
    name : string;
    url : string option [@yojson.option];
  } [@@deriving show, yojson_of]
end

type contact_object = Contact_object.t
type license_object = License_object.t

let pp_contact_object = Contact_object.pp
let pp_license_object = License_object.pp

let yojson_of_contact_object = Contact_object.yojson_of_t
let yojson_of_license_object = License_object.yojson_of_t

type t = {
  title : string;
  description : string option [@yojson.option];
  termsOfService : string option [@yojson.option];
  contact : contact_object option [@yojson.option];
  license : license_object option [@yojson.option];
  version : string;
} [@@deriving show, yojson_of]

let mk ?description ?termsOfService ?contact ?license title version = {
  title = title;
  description = description;
  termsOfService = termsOfService;
  contact = contact;
  license = license;
  version = version;
}

let contact ?name ?url ?email () = Contact_object.{
  name = name;
  url = url;
  email = email;
}

let license ?url name = License_object.{
  name = name;
  url = url;
}

let to_json : t -> jv = fun t ->
  t |> yojson_of_t |> Json.of_yojson

let contact_object_to_json ct =
  ct |> Contact_object.yojson_of_t |> Json.of_yojson

let yojson_of_contact_object = Contact_object.yojson_of_t

let license_object_to_json lc =
  lc |> License_object.yojson_of_t |> Json.of_yojson

let yojson_of_license_object = License_object.yojson_of_t
