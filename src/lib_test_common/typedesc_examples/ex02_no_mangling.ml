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
open Bindoj_base.Type_desc
open Bindoj_gen_ts.Typescript_datatype
open Bindoj_codec.Json

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex02_no_mangling"

let discriminator = "kind"

let cty_int = Coretype.mk_prim `int
let cty_string = Coretype.mk_prim `string

let json_name = "person_no_mangling"

let configs : [`type_decl] configs = Json_config.[ no_mangling; name json_name ]

let decl : type_decl =
  variant_decl "person" [
    variant_constructor "Anonymous" `no_param ~configs:[ Json_config.no_mangling ];
    variant_constructor "With_id" (`tuple_like [cty_int]) ~configs:[ Json_config.no_mangling ];
    variant_constructor "Student" (`inline_record [
      record_field "student_id" cty_int ~configs:[ Json_config.no_mangling ];
      record_field "name" cty_string ~configs:[ Json_config.no_mangling ];
    ]) ~configs:[ Json_config.no_mangling ];
    variant_constructor "Teacher" (`inline_record [
      record_field "faculty_id" cty_int ~configs:[ Json_config.no_mangling ];
      record_field "name" cty_string ~configs:[ Json_config.no_mangling ];
      record_field "department" cty_string ~configs:[ Json_config.no_mangling ];
    ]) ~configs:[ Json_config.no_mangling ]
  ] ~configs

let decl_with_docstr : type_decl =
  variant_decl "person" [
    variant_constructor "Anonymous" `no_param
      ~configs:[ Json_config.no_mangling ]
      ~doc:(`docstr "Anonymous constructor");

    variant_constructor "With_id" (`tuple_like [cty_int])
      ~configs:[ Json_config.no_mangling ]
      ~doc:(`docstr "With_id constructor");

    variant_constructor "Student" (`inline_record [
      record_field "student_id" cty_int
        ~configs:[ Json_config.no_mangling ]
        ~doc:(`docstr "student_id field in Student constructor");
      record_field "name" cty_string
        ~configs:[ Json_config.no_mangling ]
        ~doc:(`docstr "name field in Student constructor");
    ]) ~configs:[ Json_config.no_mangling ] ~doc:(`docstr "Student constructor");

    variant_constructor "Teacher" (`inline_record [
      record_field "faculty_id" cty_int
        ~configs:[ Json_config.no_mangling ]
        ~doc:(`docstr "faculty_id field in Teacher constructor");
      record_field "name" cty_string
        ~configs:[ Json_config.no_mangling ]
        ~doc:(`docstr "name field in Teacher constructor");
      record_field "department" cty_string
        ~configs:[ Json_config.no_mangling ]
        ~doc:(`docstr "dapartment field in Teacher constructor");
    ]) ~configs:[ Json_config.no_mangling ] ~doc:(`docstr "Teacher constructor")

  ] ~configs ~doc:(`docstr "definition of person type")

let fwrt : (unit, unit) ts_fwrt_decl =
  let parent = "person" in
  "person", Util.FwrtTypeEnv.(
    init
    |> bind_object "person" [] ~configs
    |> bind_constructor ~parent "Anonymous" ~configs:[ Json_config.no_mangling ]
    |> bind_constructor ~parent "With_id" ~args:[cty_int] ~configs:[ Json_config.no_mangling ]
    |> bind_constructor ~parent "Student" ~fields:[
      field "student_id" cty_int ~configs:[ Json_config.no_mangling ];
      field "name" cty_string ~configs:[ Json_config.no_mangling ] ]
      ~configs:[ Json_config.no_mangling ]
    |> bind_constructor ~parent "Teacher" ~fields:[
      field "faculty_id" cty_int ~configs:[ Json_config.no_mangling ];
      field "name" cty_string ~configs:[ Json_config.no_mangling ];
      field "department" cty_string ~configs:[ Json_config.no_mangling ] ]
      ~configs:[ Json_config.no_mangling ]
  )

let ts_ast : ts_ast option =
  let discriminator = "kind" in
  let arg_fname = "arg" in
  let anonymous =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "Anonymous"); } ] in
  let with_id =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "With_id"); };
        { tsps_modifiers = [];
          tsps_name = arg_fname;
          tsps_type_desc = `type_reference "number"; }; ] in
  let student =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "Student"); };
        { tsps_modifiers = [];
          tsps_name = "student_id";
          tsps_type_desc = `type_reference "number"; };
        { tsps_modifiers = [];
          tsps_name = "name";
          tsps_type_desc = `type_reference "string"; } ] in
  let teacher =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "Teacher"); };
        { tsps_modifiers = [];
          tsps_name = "faculty_id";
          tsps_type_desc = `type_reference "number"; };
        { tsps_modifiers = [];
          tsps_name = "name";
          tsps_type_desc = `type_reference "string"; };
        { tsps_modifiers = [];
          tsps_name = "department";
          tsps_type_desc = `type_reference "string"; } ] in
  let person = [
    "With_id", with_id;
    "Teacher", teacher;
    "Student", student;
    "Anonymous", anonymous;
  ] in
  let options : Util.Ts_ast.options =
    { discriminator;
      var_v = "__bindoj_v";
      var_x = "__bindoj_x";
      var_fns = "__bindoj_fns";
      ret = "__bindoj_ret" } in
  Some
    [ `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = json_name;
          tsa_type_parameters = [];
          tsa_type_desc = `union (List.map snd person); };
      Util.Ts_ast.case_analyzer json_name ("analyze_"^json_name) options person; ]

open Bindoj_openapi.V3

let schema_object : Schema_object.t option =
  Util.Schema_object.variant json_name
    Schema_object.[
      "Anonymous", [];
      "With_id", [ "arg", integer () ];
      "Student", [
        "student_id", integer ();
        "name", string ();
      ];
      "Teacher", [
        "faculty_id", integer ();
        "name", string ();
        "department", string ();
      ]; ]
  |> Option.some
