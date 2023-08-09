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
(** This module provides functions to generate TypeScript code. *)
open Bindoj_typedesc.Typed_type_desc
open Bindoj_typedesc.Type_desc

type resolution_strategy = [
  | `import_location of string
  | `no_resolution
]

val generate_import :
  resolution_strategy:(type_decl -> resolution_strategy)
  -> formatter:ppf
  -> type_decl list
  -> unit
(** Writes import statements for [`reused_inline_record] of the given type declarations. *)

val generate_env :
  env:tdenv
  -> formatter:ppf
  -> type_decl list
  -> unit
(** Writes infile type definitions for [env.alias_ident_typemap] of the given tdenv. *)

val generate_decl :
  formatter:ppf
  -> type_decl
  -> unit
(** Writes type definition of the given type declaration. *)

val generate :
  resolution_strategy:(type_decl -> resolution_strategy)
  -> env:tdenv
  -> formatter:ppf
  -> type_decl list
  -> unit
(** Writes the whole results of {!generate_import}, {!generate_env}, {!generate_decl}. *)
