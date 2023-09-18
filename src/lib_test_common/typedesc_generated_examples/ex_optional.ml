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
open Bindoj_base
open struct
  module Td_ex_optional = Bindoj_test_common_typedesc_examples.Ex_optional
end

include Bindoj_gen_test_gen_output.Ex_optional_gen

module Xy_opt = struct
  type t = ex_optional_xy_opt = { x_opt : int option; y_opt : int option } [@@deriving show]
  let decl = Td_ex_optional.Xy_opt.decl
  let reflect = ex_optional_xy_opt_reflect

  let json_shape_explanation = ex_optional_xy_opt_json_shape_explanation
  let to_json = ex_optional_xy_opt_to_json
  let of_json' = ex_optional_xy_opt_of_json'

  let t : t Alcotest.testable = Alcotest.of_pp pp

  open Util.Sample_value

  let sample_value01 = {
    orig = {
      x_opt = None;
      y_opt = None;
    };
    jv = `obj [ ]
  }

  let sample_value02 = {
    orig = {
      x_opt = None;
      y_opt = Some 42;
    };
    jv = `obj [
      "yOpt", `num 42.;
    ]
  }

  let sample_value03 = {
    orig = {
      x_opt = Some (-25);
      y_opt = None;
    };
    jv = `obj [
      "xOpt", `num (-25.);
    ]
  }

  let sample_value04 = {
    orig = {
      x_opt = Some 512;
      y_opt = Some (-119);
    };
    jv = `obj [
      "xOpt", `num 512.;
      "yOpt", `num (-119.);
    ]
  }

  let sample_values = [
    sample_value01;
    sample_value02;
    sample_value03;
    sample_value04;
  ]
end

module Variant = struct
  type t = ex_optional_variant =
    | Tuple_like of int option
    | Tuple_like_alias of Ex_alias.Int_opt.t
    | Tuple_like_obj of int option * Ex_alias.Int_opt.t
    | Tuple_like_spreading of Xy_opt.t
    | Inline_record of {
      int_opt: Ex_alias.Int_opt.t;
      x_opt: int option;
      y_opt: int option;
      objtuple: int option * Ex_alias.Int_opt.t;
    }
    | Inline_record_spreading of {
      int_opt: Ex_alias.Int_opt.t;
      xy_opt: Xy_opt.t
    }
    | Reused_inline_record of {
      x_opt: int option;
      y_opt: int option;
    }
  [@@deriving show]

  let decl = Td_ex_optional.Variant.decl
  let reflect = ex_optional_variant_reflect

  let json_shape_explanation = ex_optional_variant_json_shape_explanation
  let to_json = ex_optional_variant_to_json
  let of_json' = ex_optional_variant_of_json'

  let t : t Alcotest.testable = Alcotest.of_pp pp

  open struct
    let sample_opts =
      [ None, `null; Some 42, `num 42. ]

    let sample_tuples =
      [ (None, None), [];
        (Some 42, None), [ "_0", `num 42. ];
        (None, Some 128), [ "_1", `num 128. ];
        (Some 256, Some 23), [ "_0", `num 256.; "_1", `num 23. ];
      ]

    let sample_xy_opts = Xy_opt.[
      { x_opt = None;
        y_opt = None;
      }, [];

      { x_opt = None;
        y_opt = Some 42;
      }, [
        "yOpt", `num 42.;
      ];

      { x_opt = Some (-25);
        y_opt = None;
      }, [
        "xOpt", `num (-25.);
      ];

      { x_opt = Some 512;
        y_opt = Some (-119);
      }, [
        "xOpt", `num 512.;
        "yOpt", `num (-119.);
      ];
    ]
  end

  open Kxclib

  let sample_values : t Util.Sample_value.t list =
    [ sample_opts |&> (fun (opt, jv) ->
      Tuple_like opt,
        [ ("tag", `str "tuple-like");
          ("value", jv) ]);

      sample_opts |&> (fun (opt, jv) ->
        Tuple_like_alias opt,
          [ ("tag", `str "tuple-like-alias");
            ("value", jv) ]);

      sample_tuples |&> (fun ((a, b), fields) ->
        Tuple_like_obj (a, b),
          ( ("tag", `str "tuple-like-obj")
            :: fields));

      sample_xy_opts |&> (fun (xy_opt, fields) ->
        Tuple_like_spreading xy_opt,
          ( ("tag", `str "tuple-like-spreading")
          :: fields));

      sample_opts |&>> (fun (int_opt, opt_jv) ->
        sample_xy_opts |&>> (fun ({ x_opt; y_opt }, fields) ->
          sample_tuples |&> (fun (objtuple, tuple_fields) ->
            Inline_record { int_opt; x_opt; y_opt; objtuple },
              ( ("tag", `str "inline-record")
              :: ("intOpt", opt_jv) :: fields @ [
                "objtuple", `obj tuple_fields
              ]))));

      sample_opts |&>> (fun (int_opt, jv) ->
        sample_xy_opts |&> (fun (xy_opt, fields) ->
          Inline_record_spreading { int_opt; xy_opt },
            ( ("tag", `str "inline-record-spreading")
              :: ("intOpt", jv) :: fields)));

      sample_xy_opts |&> (fun ({ x_opt; y_opt }, fields) ->
        Reused_inline_record { x_opt; y_opt },
          ( ("tag", `str "reused-inline-record")
            :: fields));
    ] |&>> (List.map (fun (orig, fields) ->
      { Util.Sample_value.orig = orig;
        jv = `obj (fields |?> (fun (_, jv) -> jv <> `null)); }))
end

let env =
  let open Bindoj_typedesc.Typed_type_desc in
  { Type_decl_environment.empty with
    alias_ident_typemap =
      StringMap.singleton
        Xy_opt.decl.td_name
        Xy_opt.(Boxed (Typed.mk decl reflect)) }

let example_generated_descs : (module Util.Ex_generated_desc) list = [
  (module Xy_opt);
  (module Variant);
]
