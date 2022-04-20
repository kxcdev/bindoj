(* Copyright 2022 Kotoi-Xie Consultancy

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. *)

type flavor = Bindoj_gen.Json_codec.variant_type_flavor

val gen_ts_type : ?export:bool -> ?flavor:flavor -> type_decl -> string
val gen_ts_case_analyzer : ?export:bool -> ?flavor:flavor -> type_decl -> string
