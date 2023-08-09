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
module StringMap = struct
  include Map.Make(String)

  let of_list xs = List.fold_left (fun m (k,v) -> add k v m) empty xs
  let to_list t = fold (fun k v acc -> (k, v) :: acc) t []
end

module Doc = struct

  (** This type represents a document. *)
  type t = [
    | `docstr of string
    | `nodoc
  ] [@@deriving show, eq]

  type 'x u = 'x * t

  let of_string_opt = function
    | None -> `nodoc
    | Some doc -> `docstr doc
end

(** This type represents a document. *)
type doc = Doc.t [@@deriving show, eq]

(** This type represents a value of type ['x] with a document. *)
type 'x with_doc = 'x Doc.u

let string_of_doc : doc -> string = show_doc

let doc_of_string_opt = Doc.of_string_opt
