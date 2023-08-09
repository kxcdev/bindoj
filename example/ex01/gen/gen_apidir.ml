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
open Bindoj_openapi.V3
open Bindoj_example_shared_apidir.Apidir

let print_json title (module Dir : Apidirectory) =
  let reg_info = Dir.registry_info () in
  Bindoj_apidir_generative.gen_openapi_document_object
    ~title
    ~version:"0.0.1"
    reg_info
  |> Document_object.to_json
  |> Kxclib.Json.to_yojson
  |> Yojson.Safe.pretty_to_string
  |> print_endline

let () = print_json "ex01" (module Bindoj_example_ex01_apidir.Apidir)
