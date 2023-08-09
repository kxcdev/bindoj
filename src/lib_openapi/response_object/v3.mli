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

module Link_object = Bindoj_openapi_link_object.V3
module Header_object = Bindoj_openapi_header_object.V3
module Reference_object = Bindoj_openapi_reference_object.V3

type t

type responses_object_key = [
  | `default
  | `status_range of [`_1XX | `_2XX | `_3XX | `_4XX | `_5XX]
  | `status_code of int
]

type responses_object = (responses_object_key * (t, Reference_object.t) either) list

val mk :
  ?headers:(string * (Header_object.t, Reference_object.t) either) list
  -> ?content:(string * Header_object.media_type_object) list
  -> ?links:(string * (Link_object.t, Reference_object.t) either) list
  -> string -> t

val pp : ppf -> t -> unit

val to_json : t -> jv

val yojson_of_t : t -> yojson

val pp_responses_object : ppf -> responses_object -> unit

val responses_object_to_json : responses_object -> jv

val yojson_of_responses_object : responses_object -> yojson
