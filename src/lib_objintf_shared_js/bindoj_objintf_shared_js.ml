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
open Bindoj_common

module Bridge_labels = struct
  let setup_called = "setupCalled"
  let setup = "setup"
  let instance = "instance"
  let peer_objects = "peerObjects"
  let peer_object_registry = "peerObjectRegistry"
  let endemic_object_registry = "endemicObjectRegistry"
  let endemic_objects = "endemicObjects"
  let labeledArgs = "labeledArgs"

  module EndemicObjectRegistry = struct
    let initial_objects = "initialObjects"

    let register = "register"
    let deregister = "deregister"
    let coordinate = "coordinate"
  end

  let mangle_field_name s =
    Mangling.snake_to_lower_camel ~preserve_version_substring:true s
    |> Mangling.(escape ~charmap:charmap_js_identifier)
  
  let lookup_peer_object s = mangle_field_name ("lookup_"^s)
end
