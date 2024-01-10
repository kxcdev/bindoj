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
open Js_of_ocaml

include Bindoj_objintf_gen_test_gen_utils.Utils

open struct
  let js_of_bytes =
    let encoder = Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "TextEncoder") [||] in
    fun bytes ->
      Js.Unsafe.meth_call encoder "encode"
        [|(Js.Unsafe.inject (Js.string (Bytes.to_string bytes)))|]
  and js_to_bytes =
    let decoder = Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "TextDecoder") [||] in
    fun jv ->
      Js.Unsafe.meth_call decoder "decode" [|jv|]
      |> Js.to_string
      |> Bytes.of_string
end

let encode_non_json_values_to_js __caml_obj =
  object%js
    val bytes_js = js_of_bytes(__caml_obj.bytes)
  end
and decode_non_json_values_of_js __js_obj =
  { bytes = Js.Unsafe.get __js_obj "bytes" |> js_to_bytes }

