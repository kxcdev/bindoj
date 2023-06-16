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
open Bindoj_runtime
open Kxclib.Json

module Json_value : sig
  type t = jv
  val to_json : t -> jv
  val of_json : jv -> t option
  val of_json' : ?path:jvpath -> jv -> (t, _) result
  val reflect : t Refl.t
  val json_shape_explanation : json_shape_explanation

  val json_codec : (t, t) External_format.codec
  val external_format_codecs : t External_format.codecs
end = struct
  type t = Json.jv
  let of_json x = Some x
  let of_json' ?path x = Ok x [@@warning "-27"]
  let to_json = identity
  let rec reflect : t Refl.t = lazy (
    Refl.Alias {
        get = (fun x -> Expr.Refl (reflect, x));
        mk = (function
             | Refl (refl, x) when refl == (Obj.magic reflect)
               -> Some (Obj.magic x)
             | _ -> None);
      })

  let json_shape_explanation = `any_json_value

  let json_codec : (t, jv) External_format.codec =
    { encode = to_json; decode = of_json }

  let external_format_codecs : t External_format.codecs =
    let module Map = External_format.LabelMap in
    Map.empty
    |> Map.add Wellknown.json_format' (
           External_format.Codec
             (Wellknown.json_format, json_codec))
end

type json_value = Json_value.t

let json_value_json_shape_explanation = Json_value.json_shape_explanation
