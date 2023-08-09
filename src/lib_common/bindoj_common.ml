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
module Mangling = struct
  let snake_to_upper_camel snake_str =
    String.split_on_char '_' snake_str
    |> List.map String.capitalize_ascii
    |> String.concat ""

  let snake_to_lower_camel snake_str =
    String.split_on_char '_' snake_str
    |> function
    | [] -> ""
    | h :: t ->
      (h :: (List.map String.capitalize_ascii t))
      |> String.concat ""

  let cap_snake_to_kebab cap_snake_str =
    String.split_on_char '_' cap_snake_str
    |> List.map String.lowercase_ascii
    |> String.concat "-"

  let snake_to_kebab snake_str =
    String.split_on_char '_' snake_str
    |> String.concat "-"
end
