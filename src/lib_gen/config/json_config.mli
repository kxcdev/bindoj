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
open Bindoj_typedesc.Type_desc
open Bindoj_runtime

type json_schema
type ('tag, 'datatype_expr) foreign_language +=
   | Foreign_language_JSON_Schema :
       (json_schema, Bindoj_openapi.V3.Schema_object.t) foreign_language
val json_schema :
  (json_schema, Bindoj_openapi.V3.Schema_object.t) foreign_language

include module type of Bindoj_codec.Json.Json_config

val custom_json_schema :
  Bindoj_openapi.V3.Schema_object.t
  -> ([`coretype], [`foreign_type_expression]) config
