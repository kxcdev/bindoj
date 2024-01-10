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
type peer_raw_object_reference

type ('x, 'br) peer' =
  | Peer_object of {
      raw_object : peer_raw_object_reference;
      access : peer_raw_object_reference -> 'x;
      bridge : 'br;
    }

type ('x, 'br) endemic' =
  | Endemic_object of {
      underlying : 'x;
      bridge : 'br;
    }

let access (Peer_object { raw_object; access; _ }) = access raw_object
let bridge_generic ~bridge underlying = Endemic_object { underlying; bridge }

type peer_full_bridge_reference

type endemic_full_bridge_reference

exception Full_Bridge_already_setup
