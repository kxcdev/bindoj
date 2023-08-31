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
module Mangling : sig
  (** This module provides functions that transform the format of a given string. *)

  val snake_to_upper_camel :
    ?preserve_version_substring:bool (** default to [true] *)
    -> string -> string
  (** Takes a string in {b snake_case} format and transforms it into {b UpperCamelCase}.
      @param string the snake case string to convert.
      @return the transformed string in {b UpperCamelCase} format. *)

  val snake_to_lower_camel :
    ?preserve_version_substring:bool (** default to [true] *)
    -> string -> string
  (** Takes a string in {b snake_case} format and transforms it into {b lowerCamelCase}.
      @param string the snake case string to convert.
      @return the transformed string in {b lowerCamelCase} format. *)

  val cap_snake_to_kebab :
    ?preserve_version_substring:bool (** default to [true] *)
    -> string -> string
  (** Takes a string in {b Capitalized_b snake_case} format and transforms it into {b kebab-case}.
      @param string the capitalized snake case string to convert.
      @return the transformed string in {b kebab-case} format. *)

  val snake_to_kebab :
    ?preserve_version_substring:bool (** default to [true] *)
    -> string -> string
  (** Takes a string in {b snake_case} format and transforms it into {b kebab-case}.
      @param string the snake case string to convert.
      @return the transformed string in {b kebab-case} format. *)
end
