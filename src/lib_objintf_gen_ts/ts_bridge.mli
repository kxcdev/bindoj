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
open Bindoj_objintf_shared
open Bindoj_typedesc.Type_desc
open Bindoj_gen_ts
open Typescript_datatype

type type_decl_resolution_strategy = [
  | `import_location of string
  | `inline_type_definition
  | `infile_type_definition
  | `no_resolution
]

type ident_resolution_strategy = [
  | `import_location of string
  | `inline_type_definition of ts_type_desc
  | `infile_type_definition of ts_type_desc
  | `no_resolution
]

val ts_ast_of_objintf :
  type_decl_resolution_strategy:(type_decl -> Coretype.codec -> type_decl_resolution_strategy)
  -> ident_resolution_strategy:(Coretype.ident -> ident_resolution_strategy)
  -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
  -> bindoj_runtime_import_location:string
  -> 'bridgeable_ident sameworld_objintf
  -> Typescript_datatype.ts_ast

val gen_ts_bridge :
  type_decl_resolution_strategy:(type_decl -> Coretype.codec -> type_decl_resolution_strategy)
  -> ident_resolution_strategy:(Coretype.ident -> ident_resolution_strategy)
  -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
  -> bindoj_runtime_import_location:string
  -> 'bridgeable_ident sameworld_objintf
  -> string
