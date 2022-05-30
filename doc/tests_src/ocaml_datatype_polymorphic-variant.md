<!-- Copyright 2022 Kotoi-Xie Consultancy

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. -->

```ocaml
# (* preparation *);;
# open Kxclib;;
# open Doctests_utils.Test_utils;;
```

### simple polymorphic variant type : int_or_string
```ocaml
# open Bindoj.Type_desc;;
# open Bindoj.Caml_gen;;
# open Bindoj.Caml_gen.Datatype;;

# let int_or_string_desc =
    { td_name = "int_or_string";
      td_kind =
        Variant_kind [
          Cstr_tuple {
            ct_name = "int";
            ct_args = ["int"];
            ct_codec = `default_codec;
            ct_flvconfigs = []
          }, `nodoc;
          Cstr_tuple {
            ct_name = "string";
            ct_args = ["string"];
            ct_codec = `default_codec;
            ct_flvconfigs = []
          }, `nodoc;
        ], `nodoc;
      td_flvconfigs = [
        Flvconfig_variant_flavor `polymorphic_variant
      ] };;
val int_or_string_desc : type_decl =
  {td_name = "int_or_string";
   td_kind =
    (Variant_kind
      [(Cstr_tuple
         {Bindoj.Type_desc.ct_name = "int"; ct_args = ["int"];
          ct_codec = `default_codec;
          ct_flvconfigs = Bindoj.Type_desc.FlavorConfigs.[]},
        `nodoc);
       (Cstr_tuple
         {Bindoj.Type_desc.ct_name = "string"; ct_args = ["string"];
          ct_codec = `default_codec;
          ct_flvconfigs = Bindoj.Type_desc.FlavorConfigs.[]},
        `nodoc)],
     `nodoc);
   td_flvconfigs =
    Bindoj.Type_desc.FlavorConfigs.(::)
     (Bindoj_gen.Caml_datatype.Flvconfig_variant_flavor `polymorphic_variant,
      Bindoj.Type_desc.FlavorConfigs.[])}

# Bindoj.(
   let int_or_string_decl = Caml_gen.Datatype.type_declaration_of_type_decl int_or_string_desc in
   Caml.Structure.([declaration int_or_string_decl] |> printf "%a@?" pp_caml));;
type int_or_string = [ `int of int  | `string of string ]
- : unit = ()
```
