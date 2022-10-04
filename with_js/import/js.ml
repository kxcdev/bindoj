(* Copyright 2022 Kotoi-Xie Consultancy, Inc.
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. *)

(* Acknowledgements - AnchorZ Inc.
The initial version or a significant portion of this file is developed
under the funding of AnchorZ Inc. to satisfy its needs in
product development. *)

[@@@ocaml.warning "-32-33"]

open Js_of_ocaml

include Js

type any = Unsafe.any

let require specifier = Js.Unsafe.eval_string (Format.sprintf "require('%s')" specifier)

let clone (obj: 'a) : 'a = _JSON##parse (_JSON##stringify obj)

module Optional : Js.OPT = struct
  type 'a t = 'a
  open struct
    external cast : 'a -> 'b = "%identity"
    let null = Js.Unsafe.pure_js_expr "null"
    let undefined = Js.Unsafe.pure_js_expr "undefined"
    external equals : 'a -> 'b -> bool = "caml_js_equals"
    let is_null_or_undefined x =
      equals x null || equals x undefined
  end
  let empty = null
  let return x = x
  let bind x f = if is_null_or_undefined x then cast x else f x
  let map x f = if is_null_or_undefined x then cast x else return (f x)
  let test x = not (is_null_or_undefined x)
  let iter x f = if not (is_null_or_undefined x) then f x
  let case x f g = if is_null_or_undefined x then f () else g x
  let get x f = if is_null_or_undefined x then f () else x
  let option = function None -> empty | Some x -> return x
  let to_option x = case x (fun () -> None) (fun x -> Some x)
end

let optional x = Optional.return x |> Optional.to_option

module Json = struct
  let stringify (obj: 'a) : string = _JSON##stringify obj |> to_string

  let fmt (f: Format.formatter) (obj: 'a) =
    Format.pp_print_string f (stringify obj)

  let parse (str: string) : 'a = _JSON##parse (string str)
end

module Console = struct
  let log (x: 'a) = Firebug.console##log x
end

module Promise = struct
  open Ts2ocaml

  type 'a t = 'a Promise.t
  let t_of_js = Promise.t_of_js
  let t_to_js = Promise.t_to_js

  type error = Promise.error
  let error_of_js = Promise.error_of_js
  let error_to_js = Promise.error_to_js

  include [%js:
    val bind: 'T t -> ('T -> 'TResult1 t) -> 'TResult1 t [@@js.call "then"]
    val map: 'T t -> ('T -> 'TResult1) -> 'TResult1 t [@@js.call "then"]
    val return: 'T -> 'T t [@@js.global "Promise.resolve"]
    val all: 'T t list -> 'T list t [@@js.global "Promise.all"]
    val race: 'T t list -> 'T t [@@js.global "Promise.race"]
  ]

  let of_lwt lwt =
    Promise.create (fun ~resolve ~reject ->
      Lwt.on_any lwt
        (fun a -> resolve (`U1 a))
        (fun e -> reject ~reason:(Obj.magic e) ())
    )

  let to_lwt p =
    let lwt, wake = Lwt.task () in
    let () =
      Promise.then_ p
        ~onfulfilled:(fun a -> `U1 (Lwt.wakeup wake a))
        ~onrejected: (fun e -> `U1 (Lwt.wakeup_exn wake (Js_error.Exn (Obj.magic e))))
        ()
      |> ignore
    in
    lwt
end
