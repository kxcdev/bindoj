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
open Bindoj_base
open Bindoj_base.Type_desc

let gen_structure_with_json_codec
  ?self_contained
  ?gen_json_shape_explanation
  ?discriminator_value_accessor
  ?json_shape_explanation_resolution
  ?codec ~gen_type_decl ~formatter (decl, emp) =
  let open Bindoj_gen in
  let type_decl =
    if gen_type_decl then (
      `path (emp^".decl") |> some
    ) else none in
  let structure =
    Caml_datatype.gen_structure
      ?type_decl
      ?codec
      ~generators:[
        Json_codec.gen_json_codec
          ?self_contained
          ?gen_json_shape_explanation
          ?json_shape_explanation_resolution
          ?discriminator_value_accessor;
      ]
      decl
  in
  Astlib.Pprintast.structure formatter structure

let gen_signature_with_json_codec
  ?gen_json_shape_explanation
  ?discriminator_value_accessor
  ?codec ~gen_type_decl ~formatter (decl, _) =
  let open Bindoj_gen in
  let structure =
    Caml_datatype.gen_signature
      ~type_decl:gen_type_decl
      ?codec
      ~generators:[
        Json_codec.gen_json_codec_signature
          ?gen_json_shape_explanation
          ?discriminator_value_accessor;
      ]
      decl
  in
  Astlib.Pprintast.signature formatter structure
