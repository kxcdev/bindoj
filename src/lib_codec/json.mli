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
open Bindoj_base
open Bindoj_runtime
open Typed_type_desc
open Kxclib.Json

module Json_config = Bindoj_codec_config.Json_config

val get_json_discriminator_value : 'a typed_type_decl -> 'a -> string

val explain_encoded_json_shape : env:tdenv -> 't typed_type_decl -> json_shape_explanation

val of_json' : env:tdenv -> 'a typed_type_decl -> jv -> 'a OfJsonResult.t
val of_json : env:tdenv -> 'a typed_type_decl -> jv -> 'a option
val to_json : env:tdenv -> 'a typed_type_decl -> 'a -> jv
