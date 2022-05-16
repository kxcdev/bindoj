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

open Bindoj_base.Type_desc
open Bindoj_gen.Json_codec

let decl : type_decl =
  { td_name = "int_list";
    td_kind =
      Variant_kind
        [Cstr_tuple { ct_name = "IntNil";
                      ct_args = [];
                      ct_codec = `default_codec;
                      ct_flvconfigs = [Flvconfig_flat_kind
                                          { kind_fname=Some "kind"; arg_fname=Some "arg"; }];
            }, `nodoc;
          Cstr_tuple { ct_name = "IntCons";
                      ct_args = ["int"; "int_list"];
                      ct_codec = `default_codec;
                      ct_flvconfigs = [Flvconfig_flat_kind
                                          { kind_fname=Some "kind"; arg_fname=Some "arg"; }]
            }, `nodoc],
      `nodoc; }

let decl_with_docstr : type_decl =
  { td_name = "int_list";
    td_kind =
      Variant_kind
        [Cstr_tuple { ct_name = "IntNil";
                      ct_args = [];
                      ct_codec = `default_codec;
                      ct_flvconfigs = [Flvconfig_flat_kind
                                          { kind_fname=Some "kind"; arg_fname=Some "arg"; }]
            }, `docstr "nil for int_list";
          Cstr_tuple { ct_name = "IntCons";
                      ct_args = ["int"; "int_list"];
                      ct_codec = `default_codec;
                      ct_flvconfigs = [Flvconfig_flat_kind
                                          { kind_fname=Some "kind"; arg_fname=Some "arg"; }]
            }, `docstr "cons for int_list"],
      `docstr "int list"; }
