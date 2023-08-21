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
open Bindoj_runtime
open Bindoj_typedesc.Type_desc

type 't ignore_order_list = 't list [@@deriving show, eq]

type ts_ast = ts_statement list [@@deriving show, eq]

and ts_statement = [
  | `type_alias_declaration of ts_type_alias_decl
  | `function_declaration of ts_func_decl
  | `value_declaration of ts_value_decl
  | `module_declaration of ts_mod_decl
  | `return_statement of ts_expression
  | `if_statement of ts_expression * ts_statement * ts_statement
  | `throw_statement of ts_expression
  | `block of ts_ast
] [@@deriving show, eq]

and ts_type_alias_decl = {
  tsa_modifiers : ts_modifier ignore_order_list;
  tsa_name : string;
  tsa_type_parameters : string list;
  tsa_type_desc : ts_type_desc;
} [@@deriving show, eq]

and ts_func_decl = {
  tsf_modifiers : ts_modifier ignore_order_list;
  tsf_name : string;
  tsf_type_parameters : string list;
  tsf_parameters : ts_parameter list;
  tsf_type_desc : ts_type_desc;
  tsf_body : ts_ast;
} [@@deriving show, eq]

and ts_value_decl = {
  tsv_modifiers : [ `export ] ignore_order_list;
  tsv_kind : [ `const | `let_ ];
  tsv_name : string;
  tsv_type_desc : ts_type_desc option;
  tsv_value : ts_expression;
} [@@deriving show, eq]

and ts_mod_decl = {
  tsm_modifiers : [ `export ] list;
  tsm_name : string;
  tsm_body : ts_ast;
} [@@deriving show, eq]

and ts_type_desc = [
  | `special of [
    | `void | `undefined | `null
    | `any | `unknown | `never
    ]
  | `type_reference of string (* includes primitive types except special *)
  | `type_construct of string*ts_type_desc list
  | `type_literal of ts_property_signature ignore_order_list
  | `literal_type of ts_literal_type
  | `tuple of ts_type_desc list
  | `union of ts_type_desc ignore_order_list
  | `intersection of ts_type_desc ignore_order_list
  | `array of ts_type_desc
  | `func_type of ts_func_type_desc
  | `record of ts_type_desc * ts_type_desc (* https://www.typescriptlang.org/docs/handbook/utility-types.html#recordkeys-type *)
  | `type_assertion of ts_type_desc * ts_type_desc
  | `typeof of ts_expression (* TypeScript has artificial(?) limitation on the sort of
                                expressions allowed, but do not care here *)
  | `keyof of ts_type_desc
] [@@deriving show, eq]

and ts_property_signature = {
  tsps_modifiers : [ `readonly ] ignore_order_list;
  tsps_name : string;
  tsps_optional : bool;
  tsps_type_desc : ts_type_desc;
} [@@deriving show, eq]

and ts_literal_type = [
  | `numeric_literal of float
  | `string_literal of string
  | `template_literal of string
] [@@deriving show, eq]

and ts_parameter = {
  tsp_name : string;
  tsp_type_desc : ts_type_desc;
} [@@deriving show, eq]

and ts_func_type_desc = {
  tsft_parameters : ts_parameter list;
  tsft_type_desc : ts_type_desc;
} [@@deriving show, eq]

and ts_expression = [
  | `identifier of string
  | `literal_expression of ts_literal_expression
  | `call_expression of ts_call_expression
  | `element_access_expression of ts_element_access_expression
  | `property_access_expression of ts_property_access_expression
  | `binary_expression of ts_binary_expression
  | `arrow_function of ts_arrow_function
  | `new_expression of ts_new_expression
  | `await_expression of ts_expression
  | `casted_expression of ts_expression * ts_type_desc
  | `const_assertion of ts_expression
] [@@deriving show, eq]

and ts_literal_expression = [
  | `numeric_literal of float
  | `string_literal of string
  | `template_literal of string
  | `object_literal of (string*ts_expression) ignore_order_list
] [@@deriving show, eq]

and ts_call_expression = {
  tsce_expression : ts_expression;
  tsce_arguments : ts_expression list;
} [@@deriving show, eq]

and ts_element_access_expression = {
  tsea_expression : ts_expression;
  tsea_argument : ts_expression;
} [@@deriving show, eq]

and ts_property_access_expression = {
  tspa_expression : ts_expression;
  tspa_name : string;
} [@@deriving show, eq]

and ts_binary_expression = {
  tsbe_left : ts_expression;
  tsbe_operator_token : string;
  tsbe_right : ts_expression;
} [@@deriving show, eq]

and ts_arrow_function = {
  tsaf_parameters : ts_parameter list;
  tsaf_body : ts_ast;
} [@@deriving show, eq]

and ts_new_expression = {
  tsne_expression : ts_expression;
  tsne_arguments : ts_expression list;
} [@@deriving show, eq]

and ts_modifier = [
  | `export
  | `async
  | `readonly
] [@@deriving show, eq]

type typescript
type ('tag, 'datatype_expr) foreign_language +=
   | Foreign_language_TypeScript :
       (typescript, ts_type_desc) foreign_language

val typescript : (typescript, ts_type_desc) foreign_language

module Ts_config : sig
  include module type of Bindoj_codec_config.Json_config

  type reused_variant_inline_record_style = [ `inline_fields | `intersection_type ]

  type ('pos, 'kind) config +=
    | Config_ts_reused_variant_inline_record_style : reused_variant_inline_record_style -> ('pos, reused_variant_inline_record_style) config

  val default_reused_variant_inline_record_style : reused_variant_inline_record_style

  val reused_variant_inline_record_style : reused_variant_inline_record_style -> ([< pos], reused_variant_inline_record_style) config

  val get_reused_variant_inline_record_style_opt : [< pos] configs -> reused_variant_inline_record_style option

  val typescript_type :
    ts_type_desc -> ([`coretype], [`foreign_type_expression]) config
end
