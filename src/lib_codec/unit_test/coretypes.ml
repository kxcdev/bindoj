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
open Alcotest
open Kxclib
open Bindoj_typedesc
open Bindoj_typedesc.Typed_type_desc
open Bindoj_codec

module type Type = sig
  type t
  val pp : ppf -> t -> unit
  val ttd : t typed_type_decl
  val env : tdenv
  val samples : t list
end

let create_test_case (module T : Type) =
  let open T in
  let decl = Typed.decl ttd in
  let msg = sprintf "%s %s" decl.td_name in
  decl.td_name, [
    test_case "[interpreted] encoder and decoder works" `Quick (fun() ->
      samples |> List.iter (fun sample ->
        check (option (of_pp pp)) (msg "encoding") (some sample)
          (sample |> Json.to_json ~env ttd |> Json.of_json ~env ttd))

    )
  ]

module Tuple2 = struct
  type t = int * float [@@deriving show]
  let ttd =
    Coretypes.(
      Tuple.tup2 (Prims.int, Prims.float)
      |> to_typed_type_decl "tuple")
  let env = Type_decl_environment.empty
  let samples = [ (0, 0.1); (1, 1.1); (-1, -2.3) ]
end

let () =
  [ create_test_case (module Tuple2);
  ] |> Alcotest.run "lib_codec.coretypes"
