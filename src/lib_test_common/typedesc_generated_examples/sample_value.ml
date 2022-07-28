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
open Kxclib

type 'a t = {
  orig : 'a;
  jv : Json.jv;
}

let orig (x: 'a t) : 'a = x.orig

let jv (x: 'a t) : Json.jv = x.jv

let yojson (x: 'a t) : Json.yojson = Json.to_yojson x.jv

let jsonm (x: 'a t) : Json.jsonm = Json.to_jsonm x.jv

module JvHelper = struct
  open Json

  let ctor0 ?(discriminator="kind") name : jv = `obj [(discriminator, `str name)]

  let ctor1 ?(discriminator="kind") ?(arg="arg") name (value: jv) : jv =
    `obj [
      (discriminator, `str name);
      (arg, value);
    ]

  let ctor2 ?(discriminator="kind") ?(arg="arg") name (v1: jv) (v2: jv) : jv =
    `obj [
      (discriminator, `str name);
      (arg, `arr [v1; v2]);
    ]

  let ctorN ?(discriminator="kind") ?(arg="arg") name (values: jv list) : jv =
    `obj [
      (discriminator, `str name);
      (arg, `arr values);
    ]

  let ctor_record ?(discriminator="kind") name (fields: (string * jv) list) : jv =
    `obj ((discriminator, `str name) :: fields)
end
