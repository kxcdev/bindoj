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

let simple_interfaces = "Simple_interfaces"
let complex_interfaces = "Complex_interfaces"
let endemic_object_registry_interface = "Endemic_object_registry_interface"
let concrete_bridge = "Concrete_bridge"
let interfaces = "Interfaces"
let peer_object_registry = "Peer_object_registry"
let peer_objects = "Peer_objects"
let endemic_object_registry = "Endemic_object_registry"

let concrete_bridge_interfaces = "Concrete_bridge_interfaces"

let setup_less_full_bridge = "Setup_less_full_bridge"
let endemic_setup_only_full_bridge = "Endemic_setup_only_full_bridge"
let peer_setup_only_full_bridge = "Peer_setup_only_full_bridge"
let dual_setup_full_bridge = "Dual_setup_full_bridge"

let initially_registry_objects = "initially_registry_objects"
let endemic_full_bridge = "endemic_full_bridge"

let functor_parameter_var = "M"

let bridge = "Bridge"

let peer_full_bridge = "peer_full_bridge"

let bridge_marker_ctor = "Br"

let access_peer = "access"
let bridge_endemic = "bridge"

let get_lookup_name = sprintf "lookup_%s"
let get_registry_name = sprintf "registry_of_%s"
let get_register_name = sprintf "register_%s"
let get_deregister_name = sprintf "deregister_%s"
