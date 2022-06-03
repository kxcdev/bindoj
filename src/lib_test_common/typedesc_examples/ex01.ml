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
open Bindoj_gen_foreign.Foreign_datatype
open Bindoj_gen_ts.Typescript_datatype

let decl : type_decl =
  { td_name = "student";
    td_kind =
      Record_kind
        ([{ rf_name = "admission_year"; rf_type = "int"; rf_codec = `default_codec }, `nodoc;
          { rf_name = "name"; rf_type = "string"; rf_codec = `default_codec }, `nodoc;]),
      `nodoc;
    td_flvconfigs = [];
  }

let decl_with_docstr : type_decl =
  { td_name = "student";
    td_kind =
      Record_kind
        [{ rf_name = "admission_year"; rf_type = "int"; rf_codec = `default_codec },
          `docstr "addmission_year field";
          { rf_name = "name"; rf_type = "string"; rf_codec = `default_codec },
          `docstr "name field";],
      `docstr "definition of student type";
    td_flvconfigs = [];
  }

let fwrt : (unit, unit) fwrt_decl =
  "student", FwrtTypeEnv.(
    init
    |> bind ~annot:() "student"
      [ item ~annot:() "admission_year" ["int"];
        item ~annot:() "name" ["string"]; ]
  )

let ts_ast : ts_ast option =
  Some
    [ `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = "student";
          tsa_type_parameters = [];
          tsa_type_desc =
            `type_literal
              [ { tsps_modifiers = [];
                  tsps_name = "admission_year";
                  tsps_type_desc = `type_reference "number"; };
                { tsps_modifiers = [];
                  tsps_name = "name";
                  tsps_type_desc = `type_reference "string"; }; ]; } ]
