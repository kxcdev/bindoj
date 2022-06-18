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

[@@@ocaml.warning "-32-33"]

open Js_of_ocaml

include Js

type any = Unsafe.any

let require specifier = Js.Unsafe.eval_string (Format.sprintf "require('%s')" specifier)

let clone (obj: 'a) : 'a = _JSON##parse (_JSON##stringify obj)

module Json = struct
  let stringify (obj: 'a) : string = _JSON##stringify obj |> to_string

  let parse (str: string) : 'a = _JSON##parse (string str)
end

module Console = struct
  let log (x: 'a) = Firebug.console##log x
end
