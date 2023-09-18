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
open Kxclib
open struct
  module Td_ex_nested_multiply = Bindoj_test_common_typedesc_examples.Ex_nested_multiply
end

include Bindoj_gen_test_gen_output.Ex_nested_multiply_gen

module Record = struct
  type t = ex_nested_multiply_record = {
    nested_record: Ex_nested.Record.t;
    nested_record_spread: Ex_nested.Record.t;
  } [@@deriving show]

  let decl = Td_ex_nested_multiply.Record.decl
  let reflect = ex_nested_multiply_record_reflect

  let json_shape_explanation = ex_nested_multiply_record_json_shape_explanation
  let to_json = ex_nested_multiply_record_to_json
  let of_json' = ex_nested_multiply_record_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  open Util.Sample_value

  let sample_values =
    Ex_nested.Record.sample_values
    |&> (fun { orig; jv } ->
      let fields = match jv with
        | `obj fields -> fields
        | _ -> failwith "unexpected jv of type ex_nested_record."
      in
      { orig = {
          nested_record = orig;
          nested_record_spread = orig };
        jv = `obj (("nestedRecord",  jv) :: fields)
      })
end

module Variant = struct
  type t = ex_nested_multiply_variant =
    | Nested_record of {
      nested_record: Ex_nested.Record.t ;
      nested_record_spread: Ex_nested.Record.t }
    | Nested_variant of {
      nested_variant: Ex_nested.Variant.t ;
      nested_variant_spread: Ex_nested.Variant.t }
  [@@deriving show]

  let decl = Td_ex_nested_multiply.Variant.decl
  let reflect = ex_nested_multiply_variant_reflect

  let json_shape_explanation = ex_nested_multiply_variant_json_shape_explanation
  let to_json = ex_nested_multiply_variant_to_json
  let of_json' = ex_nested_multiply_variant_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  open Util.Sample_value

  let sample_values =
    (Ex_nested.Record.sample_values
      |&> (fun { orig; jv } ->
        let fields = match jv with
          | `obj fields -> fields
          | _ -> failwith "Unexpected jv of type ex_nested_record."
        in
        { orig = Nested_record {
            nested_record = orig;
            nested_record_spread = orig };
          jv = `obj (
            ("label", `str "nested-record")
            :: ("nestedRecord",  jv)
            :: fields)
        }))
    @ (Ex_nested.Variant.sample_values
        |&> (fun { orig; jv } ->
          let fields = match jv with
            | `obj fields -> fields
            | _ -> failwith "Unexpected jv of type ex_nested_variant."
          in
          { orig = Nested_variant {
              nested_variant = orig;
              nested_variant_spread = orig };
            jv = `obj (
              ("label", `str "nested-variant")
              :: ("nestedVariant", jv)
              :: fields)
          }))
end

let env =
  let open Bindoj_typedesc.Typed_type_desc in
  { Type_decl_environment.empty with
    alias_ident_typemap =
      Util.Env.to_alias_ident_typelist [
        (module Ex_nested.Record);
        (module Ex_nested.Variant);
      ]
      @ (Ex_nested.env.alias_ident_typemap
         |> StringMap.to_seq |> List.of_seq)
      |> StringMap.of_list }

let example_generated_descs : (module Util.Ex_generated_desc) list = [
  (module Record);
  (module Variant);
]
