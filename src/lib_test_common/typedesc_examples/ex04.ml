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
open Bindoj_gen.Caml_datatype
open Bindoj_gen_foreign.Foreign_datatype

let decl : type_decl = {
  td_name = "foo";
  td_kind =
    Variant_kind [
      Cstr_tuple {
        ct_name = "Foo0";
        ct_args = [];
        ct_codec = `default_codec;
        ct_flvconfigs = []
      }, `nodoc;
      Cstr_tuple {
        ct_name = "Foo1";
        ct_args = ["int"];
        ct_codec = `default_codec;
        ct_flvconfigs = []
      }, `nodoc;
      Cstr_tuple {
        ct_name = "Foo2";
        ct_args = ["int"; "int"];
        ct_codec = `default_codec;
        ct_flvconfigs = []
      }, `nodoc;
    ], `nodoc;
  td_flvconfigs = [
    Flvconfig_variant_flavor `polymorphic_variant
  ]
}

let decl_with_docstr : type_decl = {
  td_name = "foo";
  td_kind =
    Variant_kind [
      Cstr_tuple {
        ct_name = "Foo0";
        ct_args = [];
        ct_codec = `default_codec;
        ct_flvconfigs = []
      }, `docstr "polyvariant case (length=0)";
      Cstr_tuple {
        ct_name = "Foo1";
        ct_args = ["int"];
        ct_codec = `default_codec;
        ct_flvconfigs = []
      }, `docstr "polyvariant case (length=1)";
      Cstr_tuple {
        ct_name = "Foo2";
        ct_args = ["int"; "int"];
        ct_codec = `default_codec;
        ct_flvconfigs = []
      }, `docstr "polyvariant case (length=2)";
    ], `docstr "polyvariant";
  td_flvconfigs = [
    Flvconfig_variant_flavor `polymorphic_variant
  ]
}

let fwrt : (unit, unit) fwrt_decl =
  "foo", FwrtTypeEnv.(
    init
    |> bind ~annot:() "foo" []
    |> bind ~parent:"foo" ~annot:() "Foo0" []
    |> bind ~parent:"foo" ~annot:() "Foo1" [ item ~annot:() "arg" ["int"] ]
    |> bind ~parent:"foo" ~annot:() "Foo2" [ item ~annot:() "arg" ["int"; "int"] ]
  )
