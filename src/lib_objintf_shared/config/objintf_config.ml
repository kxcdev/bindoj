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
open Bindoj_base
open Typed_type_desc

type method_bundle_caml_style = [ `module_ | `object_ ]

type ('pos, 'kind) config +=
  | Config_objintf_caml_style :
    method_bundle_caml_style -> ([ `method_bundle_brideable_decl ], [ `objintf_caml_style ]) config
  | Config_idents_of_type_decls :
    type_decl list -> ([ `coretype ], [`idents_of_type_decls]) config

let default_caml_style = `module_
let caml_style s = Config_objintf_caml_style s
let get_caml_style =
  Configs.find_or_default ~default:default_caml_style (function
    | Config_objintf_caml_style s -> Some s
    | _ -> None)

let ident_of_type_decls tds = Config_idents_of_type_decls tds

let get_ident_of_type_decls =
  Configs.find_or_default ~default:[] (function
    | Config_idents_of_type_decls tds -> Some tds
    | _ -> None)
