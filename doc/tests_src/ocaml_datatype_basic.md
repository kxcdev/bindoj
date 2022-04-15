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

## Basic OCaml Datatype Declaration Generation

### simple record type : student
```ocaml
# open Bindoj.Type_desc;;
# #show_type type_decl;;
type nonrec type_decl =
  type_decl = {
  td_name : string;
  td_kind : generic_kind with_docstr;
}
# let student_desc =
    { td_name = "student";
      td_kind =
        Record_kind
          ([{ rf_name = "admission_year"; rf_type = "int"; rf_codec = `default_codec }, `nodoc;
            { rf_name = "full_name"; rf_type = "string"; rf_codec = `default_codec }, `nodoc;]),
        `nodoc; };;
val student_desc : type_decl =
  {td_name = "student";
   td_kind =
    (Record_kind
      [({rf_name = "admission_year"; rf_type = "int";
         rf_codec = `default_codec},
        `nodoc);
       ({rf_name = "full_name"; rf_type = "string";
         rf_codec = `default_codec},
        `nodoc)],
     `nodoc)}
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
  end
# #show_module Bindoj.Caml_gen.Datatype;;
module Datatype = Bindoj_gen.Caml_datatype
module Datatype = Bindoj_gen.Caml_datatype
module Datatype :
  sig
    val type_declaration_of_type_decl :
      type_decl -> Bindoj.Versioned.V0.Caml.type_declaration
  end
# #show_module Bindoj.Caml;;
module Caml = Bindoj.Versioned.V0.Caml
module Caml :
  sig
    module Ppxlib = Ppxlib
    module Astlib = Astlib
    module Pprintast = Astlib.Pprintast
    module CommonTypes : sig ... end
    type structure_item = CommonTypes.structure_item
    type structure = structure_item list
    type value_binding = CommonTypes.value_binding
    type type_declaration = CommonTypes.type_declaration
    module Structure : sig ... end
  end
```

```ocaml
# Bindoj.(
   let decl = Caml_gen.Datatype.type_declaration_of_type_decl student_desc in
   Caml.Structure.([declaration decl] |> printf "%a@?" pp_caml));;
type student = {
  admission_year: int ;
  full_name: string }
- : unit = ()
```

