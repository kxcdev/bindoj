(* Copyright 2022 Kotoi-Xie Consultancy

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. *)

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

  let ctor0 name : jv = `obj [("kind", `str name)]

  let ctor1 name (value: jv) : jv =
    `obj [
      ("kind", `str name);
      ("arg", value);
    ]

  let ctor2 name (v1: jv) (v2: jv) : jv =
    `obj [
      ("kind", `str name);
      ("arg", `arr [v1; v2]);
    ]

  let ctorN name (values: jv list) : jv =
    `obj [
      ("kind", `str name);
      ("arg", `arr values);
    ]

  let ctor_record name (fields: (string * jv) list) : jv =
    `obj (("kind", `str name) :: fields)
end
