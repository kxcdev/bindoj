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

module Reference_object = Bindoj_openapi_reference_object.V3
module Example_object = Bindoj_openapi_example_object.V3
module Schema_object = Bindoj_openapi_schema_object.V3
module Style_value = Bindoj_openapi_style_value.V3

type t

type encoding_object

type media_type_object

val encoding :
  ?contentType:string ->
  ?headers:(string * (t, Reference_object.t) either) list ->
  ?style:Style_value.t ->
  ?explode:bool ->
  ?allowReserved:bool ->
  unit -> encoding_object

val media_type :
  ?schema:(Schema_object.t, Reference_object.t) either ->
  ?example:jv ->
  ?examples:(string * (Example_object.t, Reference_object.t) either) list ->
  ?encoding:(string * encoding_object) list ->
  unit -> media_type_object

val mk :
  ?description:string ->
  ?required:bool ->
  ?deprecated:bool ->
  ?allowEmptyValue:bool ->
  ?style:Style_value.t ->
  ?explode:bool ->
  ?schema:(Schema_object.t, Reference_object.t) either ->
  ?example:jv ->
  ?examples:(string * (Example_object.t, Reference_object.t) either) list ->
  ?content:(string * media_type_object) list ->
  unit -> t

val pp : ppf -> t -> unit

val to_json : t -> jv

val yojson_of_t : t -> yojson

val pp_encoding_object : ppf -> encoding_object -> unit

val encoding_object_to_json : encoding_object -> jv

val yojson_of_encoding_object : encoding_object -> yojson

val pp_media_type_object : ppf -> media_type_object -> unit

val media_type_object_to_json : media_type_object -> jv

val yojson_of_media_type_object : media_type_object -> yojson
