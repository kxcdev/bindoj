<!-- Copyright 2022-2023 Kotoi-Xie Consultancy, Inc. This file is a part of the

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
                                                                             -->
<!-- Acknowledgements  --- AnchorZ Inc. ---  The current/initial version or a
significant portion of this file is developed under the funding provided by
AnchorZ Inc. to satisfy its needs in its product development workflow.
                                                                             -->
```ocaml
# (* preparation *);;
# open Kxclib;;
# open Doctests_utils.Test_utils;;
```

## Basic OCaml Datatype Declaration Generation

`Bindoj.Type_desc.type_decl` is a type meaning type declaration and there are
record type and variant type as type kinds. The following are datatype
declaration generation examples: `student` of record type and `person` of
variant type. `Bindoj.Type_desc.type_declaration_of_type_decl` is a function
converting `Bindoj.Type_desc.type_decl` to `type_declaration`.

### simple record type : student
```ocaml
# open Bindoj.Type_desc;;
# #show_type type_decl;;
type nonrec type_decl =
  type_decl = {
  td_name : string;
  td_configs : [ `type_decl ] configs;
  td_kind : type_decl_kind;
  td_doc : Bindoj_base.Runtime.doc;
}
# let student_desc =
    record_decl "student" [
      record_field "admission_year" (Coretype.mk_prim `int);
      record_field "full_name" (Coretype.mk_prim `string);
    ];;
val student_desc : type_decl =
  {td_name = "student"; td_configs = Bindoj.Type_desc.Configs.[];
   td_kind =
    Record_decl
     [{rf_name = "admission_year";
       rf_type =
        `direct
          {Bindoj.Type_desc.Coretype.ct_desc =
            Bindoj.Type_desc.Coretype.Prim `int;
           ct_configs = Bindoj_typedesc.Type_desc.Configs.[]};
       rf_configs = Bindoj.Type_desc.Configs.[]; rf_doc = `nodoc};
      {rf_name = "full_name";
       rf_type =
        `direct
          {Bindoj.Type_desc.Coretype.ct_desc =
            Bindoj.Type_desc.Coretype.Prim `string;
           ct_configs = Bindoj_typedesc.Type_desc.Configs.[]};
       rf_configs = Bindoj.Type_desc.Configs.[]; rf_doc = `nodoc}];
   td_doc = `nodoc}
```

### simple variant type : person
```ocaml
# open Bindoj_gen.Json_codec;;
# #show_type type_decl;;
type nonrec type_decl =
  type_decl = {
  td_name : string;
  td_configs : [ `type_decl ] configs;
  td_kind : type_decl_kind;
  td_doc : Bindoj_runtime.doc;
}
# let person_desc =
    variant_decl "person" [
      variant_constructor "Anonymous" `no_param;
      variant_constructor "With_id" (`tuple_like [variant_argument & Coretype.mk_prim `int]);
      variant_constructor "Student" (`inline_record [
        record_field "student_id" (Coretype.mk_prim `int);
        record_field "name" (Coretype.mk_prim `string);
      ]);
      variant_constructor "Teacher" (`inline_record [
        record_field "faculty_id" (Coretype.mk_prim `int);
        record_field "name" (Coretype.mk_prim `string);
        record_field "department" (Coretype.mk_prim `string);
      ])
    ];;
val person_desc : type_decl =
  {td_name = "person"; td_configs = Bindoj.Type_desc.Configs.[];
   td_kind =
    Variant_decl
     [{vc_name = "Anonymous"; vc_param = `no_param;
       vc_configs = Bindoj.Type_desc.Configs.[]; vc_doc = `nodoc};
      {vc_name = "With_id";
       vc_param =
        `tuple_like
          [{va_type =
             `direct
               {Bindoj.Type_desc.Coretype.ct_desc =
                 Bindoj.Type_desc.Coretype.Prim `int;
                ct_configs = Bindoj_typedesc.Type_desc.Configs.[]};
            va_configs = Bindoj.Type_desc.Configs.[]; va_doc = `nodoc}];
       vc_configs = Bindoj.Type_desc.Configs.[]; vc_doc = `nodoc};
      {vc_name = "Student";
       vc_param =
        `inline_record
          [{rf_name = "student_id";
            rf_type =
             `direct
               {Bindoj.Type_desc.Coretype.ct_desc =
                 Bindoj.Type_desc.Coretype.Prim `int;
                ct_configs = Bindoj_typedesc.Type_desc.Configs.[]};
            rf_configs = Bindoj.Type_desc.Configs.[]; rf_doc = `nodoc};
           {rf_name = "name";
            rf_type =
             `direct
               {Bindoj.Type_desc.Coretype.ct_desc =
                 Bindoj.Type_desc.Coretype.Prim `string;
                ct_configs = Bindoj_typedesc.Type_desc.Configs.[]};
            rf_configs = Bindoj.Type_desc.Configs.[]; rf_doc = `nodoc}];
       vc_configs = Bindoj.Type_desc.Configs.[]; vc_doc = `nodoc};
      {vc_name = "Teacher";
       vc_param =
        `inline_record
          [{rf_name = "faculty_id";
            rf_type =
             `direct
               {Bindoj.Type_desc.Coretype.ct_desc =
                 Bindoj.Type_desc.Coretype.Prim `int;
                ct_configs = Bindoj_typedesc.Type_desc.Configs.[]};
            rf_configs = Bindoj.Type_desc.Configs.[]; rf_doc = `nodoc};
           {rf_name = "name";
            rf_type =
             `direct
               {Bindoj.Type_desc.Coretype.ct_desc =
                 Bindoj.Type_desc.Coretype.Prim `string;
                ct_configs = Bindoj_typedesc.Type_desc.Configs.[]};
            rf_configs = Bindoj.Type_desc.Configs.[]; rf_doc = `nodoc};
           {rf_name = "department";
            rf_type =
             `direct
               {Bindoj.Type_desc.Coretype.ct_desc =
                 Bindoj.Type_desc.Coretype.Prim `string;
                ct_configs = Bindoj_typedesc.Type_desc.Configs.[]};
            rf_configs = Bindoj.Type_desc.Configs.[]; rf_doc = `nodoc}];
       vc_configs = Bindoj.Type_desc.Configs.[]; vc_doc = `nodoc}];
   td_doc = `nodoc}
```

### `Bindoj.Caml_gen`
```ocaml
# #show_module Bindoj.Caml_gen;;
module Caml_gen = Bindoj.Versioned.V0.Caml_gen
module Caml_gen :
  sig
    module Caml = Bindoj.Versioned.V0.Caml
    module Datatype = Bindoj_gen.Caml_datatype
    module Json_codec = Bindoj_gen.Json_codec
    module Type_module : sig ... end
  end
# #show_module Bindoj.Caml_gen.Datatype;;
module Datatype = Bindoj_gen.Caml_datatype
module Datatype = Bindoj_gen.Caml_datatype
module Datatype :
  sig
    val type_declaration_of_type_decl :
      ?type_name:string ->
      ?attrs:Ppxlib.attribute list ->
      type_decl -> Bindoj.Versioned.V0.Caml.type_declaration
    val gen_reflect :
      ?codec:Coretype.codec ->
      type_decl -> Bindoj.Versioned.V0.Caml.value_binding
    val gen_structure :
      ?type_name:string ->
      ?refl:bool ->
      ?attrs:Ppxlib.attribute list ->
      ?codec:Coretype.codec ->
      ?generators:(?codec:Coretype.codec -> type_decl -> Ppxlib.structure)
                  list ->
      ?type_decl:[ `expr of Ppxlib.expression | `path of string ] ->
      type_decl -> Ppxlib.structure
    val gen_reflect_signature :
      ?refl_type_abbr:string ->
      ?codec:Coretype.codec -> type_decl -> Ppxlib.value_description
    val gen_signature :
      ?type_name:string ->
      ?refl:bool ->
      ?attrs:Ppxlib.attribute list ->
      ?codec:Coretype.codec ->
      ?generators:(?codec:Coretype.codec ->
                   type_decl -> Bindoj.Versioned.V0.Caml.signature)
                  list ->
      ?type_decl:bool ->
      ?refl_type_abbr:string ->
      ?type_decl_type_abbr:string ->
      ?typed_type_decl_type_abbr:string ->
      type_decl -> Bindoj.Versioned.V0.Caml.signature
  end
# #show_module Bindoj.Caml;;
module Caml = Bindoj.Versioned.V0.Caml
module Caml :
  sig
    module Ppxlib = Ppxlib
    module Astlib = Astlib
    module Emitter = Bindoj_gen.Emitter
    module CommonTypes : sig ... end
    type signature_item = CommonTypes.signature_item
    type signature = CommonTypes.signature
    type structure_item = CommonTypes.structure_item
    type structure = structure_item list
    type value_binding = CommonTypes.value_binding
    type type_declaration = CommonTypes.type_declaration
    type core_type = CommonTypes.core_type
    module Structure : sig ... end
    module Signature : sig ... end
  end
```

```ocaml
# Bindoj.(
   let student_decl = Caml_gen.Datatype.type_declaration_of_type_decl student_desc in
   Caml.Structure.([declaration student_decl] |> printf "%a@?" pp_caml));;
type student = {
  admission_year: int ;
  full_name: string }
- : unit = ()
# Bindoj.(
   let person_decl = Caml_gen.Datatype.type_declaration_of_type_decl person_desc in
   Caml.Structure.([declaration person_decl] |> printf "%a@?" pp_caml));;
type person =
  | Anonymous
  | With_id of int
  | Student of {
  student_id: int ;
  name: string }
  | Teacher of {
  faculty_id: int ;
  name: string ;
  department: string }
- : unit = ()
```
