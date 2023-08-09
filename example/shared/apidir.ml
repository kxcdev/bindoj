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

module type Apidirectory = sig
  include Bindoj_apidir_shared.RegistryInfo
end

module Packed = struct
  type ('a, 'e) t = [ `Ok of 'a | `Error of 'e ]

  let ok a = `Ok a
  let error a = `Error a
  let unpack_ok = function
    | `Ok a -> Some a
    | `Error _ -> None
  let unpack_error = function
    | `Error a -> Some a
    | `Ok _ -> None
end

module type ServerBuilder = sig
  open Bindoj_apidir_shared

  module Io : Kxclib.Monadic

  val register_get_handler :
    (unit, 'respty) invocation_point_info ->
    (unit -> (int * 'respty) Io.t) ->
    unit

  val register_post_handler :
    ('reqty, 'respty) invocation_point_info ->
    ('reqty -> (int * 'respty) Io.t) ->
    unit
end

module Tdenv = struct
  open Bindoj_typedesc.Typed_type_desc
  let add_env : tdenv -> tdenv -> tdenv =
    fun a b ->
      { prim_ident_typemap =
        b.prim_ident_typemap
        |> StringMap.add_seq (StringMap.to_seq a.prim_ident_typemap);
      alias_ident_typemap =
        b.alias_ident_typemap
        |> StringMap.add_seq (StringMap.to_seq a.alias_ident_typemap); }
end
