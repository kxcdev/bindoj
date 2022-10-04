(* Copyright 2022 Kotoi-Xie Consultancy, Inc. This file is a part of the

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
type jv = Json.jv

type yojson = Json.yojson

let pp_either vppl vppr ppf either =
  let open Either in
  match either with
  | Left x -> Format.fprintf ppf "Left(%a)" vppl x
  | Right x -> Format.fprintf ppf "Right(%a)" vppr x

let pp_jv ppf jv =
  Yojson.pp ppf (Json.to_yojson jv :> Yojson.t)

let pp_yojson ppf (yojson : yojson) =
  Yojson.pp ppf (yojson :> Yojson.t)

let yojson_of_jv = Json.to_yojson

let yojson_of_either : ('a -> yojson) -> ('b -> yojson) -> ('a, 'b) either -> yojson =
  fun yojson_of_a yojson_of_b ->
  let open Either in
  function
  | Left x -> yojson_of_a x
  | Right x -> yojson_of_b x

type 'a assoc = (string * 'a) list [@@deriving show]
let yojson_of_assoc yojson_of_a fields : yojson =
  `Assoc (fields |&> fun (k, v) -> (k, yojson_of_a v))
