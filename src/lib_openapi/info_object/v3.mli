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

type t

type contact_object

type license_object

val mk :
  ?description:string -> ?termsOfService:string
  -> ?contact:contact_object -> ?license:license_object
  -> string -> string -> t

val contact : ?name:string -> ?url:string -> ?email:string -> unit -> contact_object

val license : ?url:string -> string -> license_object

val pp : ppf -> t -> unit

val to_json : t -> jv

val yojson_of_t : t -> yojson

val pp_contact_object : ppf -> contact_object -> unit

val contact_object_to_json : contact_object -> jv

val yojson_of_contact_object : contact_object -> yojson

val pp_license_object : ppf -> license_object -> unit

val license_object_to_json : license_object -> jv

val yojson_of_license_object : license_object -> yojson
