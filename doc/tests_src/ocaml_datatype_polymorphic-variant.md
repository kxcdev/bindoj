<!-- Copyright 2022 Kotoi-Xie Consultancy, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. -->

<!-- Acknowledgements - AnchorZ Inc.
The initial version or a significant portion of this file is developed
under the funding of AnchorZ Inc. to satisfy its needs in
product development. -->

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
    variant_decl "int_or_string" [
      variant_constructor "int" (`tuple_like [Coretype.mk_prim `int]);
      variant_constructor "string" (`tuple_like [Coretype.mk_prim `string]);
    ] ~configs:[
      Caml_config.variant_type `polymorphic
    ];;
val int_or_string_desc : type_decl =
  {td_name = "int_or_string";
   td_configs =
    Bindoj.Type_desc.Configs.(::)
     (Bindoj_typedesc.Type_desc.Config_caml_variant_type `polymorphic,
      Bindoj.Type_desc.Configs.[]);
   td_kind =
    Variant_decl
     [{vc_name = "int";
       vc_param =
        `tuple_like
          [{Bindoj.Type_desc.Coretype.ct_desc =
             Bindoj.Type_desc.Coretype.Prim `int;
            ct_configs = Bindoj_typedesc.Type_desc.Configs.[]}];
       vc_configs = Bindoj.Type_desc.Configs.[]; vc_doc = `nodoc};
      {vc_name = "string";
       vc_param =
        `tuple_like
          [{Bindoj.Type_desc.Coretype.ct_desc =
             Bindoj.Type_desc.Coretype.Prim `string;
            ct_configs = Bindoj_typedesc.Type_desc.Configs.[]}];
       vc_configs = Bindoj.Type_desc.Configs.[]; vc_doc = `nodoc}];
   td_doc = `nodoc}

# Bindoj.(
   let int_or_string_decl = Caml_gen.Datatype.type_declaration_of_type_decl int_or_string_desc in
   Caml.Structure.([declaration int_or_string_decl] |> printf "%a@?" pp_caml));;
type int_or_string = [ `int of int  | `string of string ]
- : unit = ()
```
