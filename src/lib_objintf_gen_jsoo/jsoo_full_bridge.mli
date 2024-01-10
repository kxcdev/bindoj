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
open Bindoj_typedesc.Typed_type_desc
open Bindoj_objintf_shared
open Bindoj_objintf_gen

val jsoo_full_bridge_impl :
    resolution_strategy:(type_decl -> Coretype.codec -> Caml_bridge.resolution_strategy)
    -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
    -> 'bridgeable_ident sameworld_objintf
    -> (module Caml_bridge.Full_bridge_impl with type bridgeable_ident = 'bridgeable_ident)

val gen_full_bridge_impl :
    resolution_strategy:(type_decl -> Coretype.codec -> Caml_bridge.resolution_strategy)
    -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
    -> 'bridgeable_ident sameworld_objintf
    -> Ppxlib.structure

val gen_structure :
    ?resolution_strategy:(type_decl -> Coretype.codec -> Caml_bridge.resolution_strategy)
    -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
    -> 'bridgeable_ident sameworld_objintf
    -> Ppxlib.structure

val gen_full_bridge_impl_signature :
    resolution_strategy:(type_decl -> Coretype.codec -> Caml_bridge.resolution_strategy)
    -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
    -> 'bridgeable_ident sameworld_objintf
    -> Ppxlib.signature

val gen_signature :
    ?resolution_strategy:(type_decl -> Coretype.codec -> Caml_bridge.resolution_strategy)
    -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
    -> 'bridgeable_ident sameworld_objintf
    -> Ppxlib.signature