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
open Kxclib
open Bindoj_base
open struct
  module Td_ex_coretype = Bindoj_test_common_typedesc_examples.Ex_coretype
end

include Bindoj_gen_test_gen_output.Ex_coretype_gen

module Various_prim_types = struct
  type uchar = Uchar.t
  let pp_uchar ppf x = Format.pp_print_char ppf (Uchar.to_char x)

  type t = ex_coretype_various_prim_types = {
    unit: unit;
    bool: bool;
    int: int;
    float: float;
    string: string;
    uchar: uchar;
    byte: char;
    bytes: Bytes.t;
  } [@@deriving show]

  let decl = Td_ex_coretype.Various_prim_types.decl
  let reflect = ex_coretype_various_prim_types_reflect

  let json_shape_explanation = ex_coretype_various_prim_types_json_shape_explanation
  let to_json = ex_coretype_various_prim_types_to_json
  let of_json' = ex_coretype_various_prim_types_of_json'

  let t : t Alcotest.testable = Alcotest.of_pp pp

  open Util.Sample_value

  let sample_value01 = {
    orig = {
      unit = ();
      bool = true;
      int = 42;
      float = 4.2;
      string = "foo";
      uchar = Uchar.of_char 'a';
      byte = 'b';
      bytes = Bytes.of_string "Hello, world!";
    };
    jv = `obj [
      "unit", `num 1.;
      "bool", `bool true;
      "int", `num 42.;
      "float", `num 4.2;
      "string", `str "foo";
      "uchar", `str (String.of_seq (List.to_seq ['a']));
      "byte", `num (float_of_int (int_of_char 'b'));
      "bytes", `str "SGVsbG8sIHdvcmxkIQ==";
    ]
  }

  let sample_values = [ sample_value01 ];
end

module With_int53p = struct
  let pp_int53p ppf x = fprintf ppf "%s" (Int53p.to_string x)
  type t = ex_coretype_with_int53p = { value : int53p; } [@@deriving show]
  let decl = Td_ex_coretype.With_int53p.decl
  let reflect = ex_coretype_with_int53p_reflect

  let json_shape_explanation = ex_coretype_with_int53p_json_shape_explanation
  let to_json = ex_coretype_with_int53p_to_json
  let of_json' = ex_coretype_with_int53p_of_json'

  let t : t Alcotest.testable = Alcotest.of_pp pp

  let sample_value01 : t Util.Sample_value.t =
    {
      orig = {
        value = (Int53p.of_int 0);
      };
      jv = `obj [ "value", `num 0.; ]
    }

  let sample_value02 : t Util.Sample_value.t =
    {
      orig = {
        value = (Int53p.of_int 1);
      };
      jv = `obj [ "value", `num 1.; ]
    }

  let sample_value03 : t Util.Sample_value.t =
    {
      orig = {
        value = (Int53p.of_int (-1));
      };
      jv = `obj [ "value", `num (-1.); ]
    }

  let sample_value04 : t Util.Sample_value.t =
    {
      orig = {
        value = (Int53p.of_int 102);
      };
      jv = `obj [ "value", `num 102.; ]
    }

  let sample_value05 : t Util.Sample_value.t =
    {
      orig = {
        value = (Int53p.of_int64 (1_099_511_627_776L));
      };
      jv = `obj [ "value", `num 1_099_511_627_776.; ]
    }

  let sample_value06 : t Util.Sample_value.t =
    {
      orig = {
        value = (Int53p.of_int64 (-2_199_023_255_552L));
      };
      jv = `obj [ "value", `num (-2_199_023_255_552.); ]
    }

  let sample_values = [
    sample_value01;
    sample_value02;
    sample_value03;
    sample_value04;
    sample_value05;
    sample_value06;
  ]
end

module Various_complex_types = struct
  type t = ex_coretype_various_complex_types = {
    option: int option;
    list:   int list;
    map: (string * int) list;
  } [@@deriving show]
  let decl = Td_ex_coretype.Various_complex_types.decl
  let reflect = ex_coretype_various_complex_types_reflect

  let json_shape_explanation = ex_coretype_various_complex_types_json_shape_explanation
  let to_json = ex_coretype_various_complex_types_to_json
  let of_json' = ex_coretype_various_complex_types_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  open Util.Sample_value

  let sample_value01 = {
    orig = {
      option = Some 42;
      list = [1;2;3;4];
      map = ["foo", 4; "bar", 2]
    };
    jv = `obj [
      "option", `num 42.;
      "list", `arr [`num 1.; `num 2.; `num 3.; `num 4.];
      "map", `obj ["foo", `num 4.; "bar", `num 2.];
    ]
  }

  let sample_value02 = {
    orig = {
      option = None;
      list = [];
      map = [];
    };
    jv = `obj [
      "list", `arr [];
      "map", `obj [];
    ]
  }

  let sample_values = [
    sample_value01;
    sample_value02;
  ]
end

module Various_tuple_types = struct
  type t = ex_coretype_various_tuple_types = {
    tuple:  (int * int);
    objtuple: (int * int);
    nested: (int option * int list * (int * int));
  } [@@deriving show]
  let decl = Td_ex_coretype.Various_tuple_types.decl
  let reflect = ex_coretype_various_tuple_types_reflect

  let json_shape_explanation = ex_coretype_various_tuple_types_json_shape_explanation
  let to_json = ex_coretype_various_tuple_types_to_json
  let of_json' = ex_coretype_various_tuple_types_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  open Util.Sample_value

  let sample_value01 = {
    orig = {
      tuple = (4, 2);
      objtuple = (4, 2);
      nested = (Some 42, [4;2], (4,2));
    };
    jv = `obj [
      "tuple", `arr [`num 4.; `num 2.];
      "objtuple", `obj ["_0", `num 4.; "_1", `num 2.];
      "nested", `arr [
        `num 42.;
        `arr [`num 4.; `num 2.];
        `arr [`num 4.; `num 2.];
      ];
    ]
  }

  let sample_value02 = {
    orig = {
      tuple = (0, 0);
      objtuple = (0, 0);
      nested = (None, [], (0,0));
    };
    jv = `obj [
      "tuple", `arr [ `num 0.; `num 0. ];
      "objtuple", `obj ["_0", `num 0.; "_1", `num 0.];
      "nested", `arr [
        `null;
        `arr [];
        `arr [`num 0.; `num 0.];
      ];
    ]
  }

  let sample_values = [
    sample_value01;
    sample_value02;
  ]
end

module Named_json = struct
  open struct
    type jv = [
      | `null
      | `bool of bool
      | `num of float
      | `str of string
      | `arr of jv list
      | `obj of (string * jv) list
    ] [@@deriving show]
  end

  type t = ex_coretype_named_json = { name : string; json : jv; } [@@deriving show]
  let decl = Td_ex_coretype.Named_json.decl
  let reflect = ex_coretype_named_json_reflect

  let json_shape_explanation = ex_coretype_named_json_json_shape_explanation
  let to_json = ex_coretype_named_json_to_json
  let of_json' = ex_coretype_named_json_of_json'

  let t : t Alcotest.testable = Alcotest.of_pp pp

  let sample_value01 : t Util.Sample_value.t =
    let jv_pos = `obj [ ("x", `num 2.); ("y", `num (-5.)); ] in
    {
      orig = {
        name = "position";
        json = jv_pos;
      };
      jv = `obj [
          ("name", `str "position");
          ("json", jv_pos);
        ]
    }

  let sample_value02 : t Util.Sample_value.t =
    let jv_hello = `str "hello?" in
    {
      orig = {
        name = "greeting";
        json = jv_hello;
      };
      jv = `obj [
          ("name", `str "greeting");
          ("json", jv_hello);
        ]
    }

  let sample_value03 : t Util.Sample_value.t =
    let jv_tup = `arr [`str "x"; `num 2.; `str "y"; `num (-5.)] in
    {
      orig = {
        name = "tup_pos";
        json = jv_tup;
      };
      jv = `obj [
          ("name", `str "tup_pos");
          ("json", jv_tup);
        ]
    }

  let sample_value04 : t Util.Sample_value.t =
    let jv_bool = `bool true in
    {
      orig = {
        name = "flag";
        json = jv_bool;
      };
      jv = `obj [
          ("name", `str "flag");
          ("json", jv_bool);
        ]
    }

  let sample_value05 : t Util.Sample_value.t =
    let jv_null = `null in
    {
      orig = {
        name = "null_val";
        json = jv_null;
      };
      jv = `obj [
          ("name", `str "null_val");
          ("json", jv_null);
        ]
    }

  let sample_values = [
    sample_value01;
    sample_value02;
    sample_value03;
    sample_value04;
    sample_value05;
  ]
end

let env = empty_tdenv |> Bindoj_std.Tdenv_wrappers.json

let example_generated_descs : (module Util.Ex_generated_desc) list = [
  (module Various_prim_types);
  (module With_int53p);
  (module Various_complex_types);
  (module Various_tuple_types);
  (module Named_json);
]
