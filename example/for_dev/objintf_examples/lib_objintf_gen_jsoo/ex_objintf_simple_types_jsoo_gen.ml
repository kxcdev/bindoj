open Bindoj_objintf_gen_jsoo_test_gen_utils [@@warning "-33"]
module Simple_interfaces = struct end
open Simple_interfaces [@@warning "-33"]

module Concrete_bridge_interfaces = struct
  open Bindoj_objintf_shared

  (** marker type for this specific concrete bridge *)
  type nonrec br = Br

  type 'x peer = ('x, br) peer'
  type 'x endemic = ('x, br) endemic'

  let access : 'x peer -> 'x = access
  let bridge x = bridge_generic ~bridge:Br x

  module Simple_interfaces = Simple_interfaces
  module Complex_interfaces = struct end

  module Interfaces = struct
    module Simple_interfaces = Simple_interfaces
    module Complex_interfaces = Complex_interfaces
    include Simple_interfaces
    include Complex_interfaces
  end
end

module type Concrete_bridge = sig
  include module type of Concrete_bridge_interfaces
end

open Concrete_bridge_interfaces.Interfaces [@@warning "-33"]

module type Endemic_setup_only_full_bridge = functor
  (M : sig
     val cty_unit : unit
     val cty_unit_option : unit option
     val cty_unit_list : unit list
     val cty_unit_map : (string * unit) list
     val cty_unit_tuple_with_unit : unit * unit
     val cty_unit_tuple_with_bool : unit * bool
     val cty_unit_tuple_with_int : unit * int
     val cty_unit_tuple_with_int53p : unit * Kxclib.int53p
     val cty_unit_tuple_with_float : unit * float
     val cty_unit_tuple_with_string : unit * string
     val cty_unit_tuple_with_uchar : unit * Uchar.t
     val cty_unit_tuple_with_byte : unit * char
     val cty_unit_tuple_with_bytes : unit * Bytes.t
     val cty_bool : bool
     val cty_bool_option : bool option
     val cty_bool_list : bool list
     val cty_bool_map : (string * bool) list
     val cty_bool_tuple_with_unit : bool * unit
     val cty_bool_tuple_with_bool : bool * bool
     val cty_bool_tuple_with_int : bool * int
     val cty_bool_tuple_with_int53p : bool * Kxclib.int53p
     val cty_bool_tuple_with_float : bool * float
     val cty_bool_tuple_with_string : bool * string
     val cty_bool_tuple_with_uchar : bool * Uchar.t
     val cty_bool_tuple_with_byte : bool * char
     val cty_bool_tuple_with_bytes : bool * Bytes.t
     val cty_int : int
     val cty_int_option : int option
     val cty_int_list : int list
     val cty_int_map : (string * int) list
     val cty_int_tuple_with_unit : int * unit
     val cty_int_tuple_with_bool : int * bool
     val cty_int_tuple_with_int : int * int
     val cty_int_tuple_with_int53p : int * Kxclib.int53p
     val cty_int_tuple_with_float : int * float
     val cty_int_tuple_with_string : int * string
     val cty_int_tuple_with_uchar : int * Uchar.t
     val cty_int_tuple_with_byte : int * char
     val cty_int_tuple_with_bytes : int * Bytes.t
     val cty_int53p : Kxclib.int53p
     val cty_int53p_option : Kxclib.int53p option
     val cty_int53p_list : Kxclib.int53p list
     val cty_int53p_map : (string * Kxclib.int53p) list
     val cty_int53p_tuple_with_unit : Kxclib.int53p * unit
     val cty_int53p_tuple_with_bool : Kxclib.int53p * bool
     val cty_int53p_tuple_with_int : Kxclib.int53p * int
     val cty_int53p_tuple_with_int53p : Kxclib.int53p * Kxclib.int53p
     val cty_int53p_tuple_with_float : Kxclib.int53p * float
     val cty_int53p_tuple_with_string : Kxclib.int53p * string
     val cty_int53p_tuple_with_uchar : Kxclib.int53p * Uchar.t
     val cty_int53p_tuple_with_byte : Kxclib.int53p * char
     val cty_int53p_tuple_with_bytes : Kxclib.int53p * Bytes.t
     val cty_float : float
     val cty_float_option : float option
     val cty_float_list : float list
     val cty_float_map : (string * float) list
     val cty_float_tuple_with_unit : float * unit
     val cty_float_tuple_with_bool : float * bool
     val cty_float_tuple_with_int : float * int
     val cty_float_tuple_with_int53p : float * Kxclib.int53p
     val cty_float_tuple_with_float : float * float
     val cty_float_tuple_with_string : float * string
     val cty_float_tuple_with_uchar : float * Uchar.t
     val cty_float_tuple_with_byte : float * char
     val cty_float_tuple_with_bytes : float * Bytes.t
     val cty_string : string
     val cty_string_option : string option
     val cty_string_list : string list
     val cty_string_map : (string * string) list
     val cty_string_tuple_with_unit : string * unit
     val cty_string_tuple_with_bool : string * bool
     val cty_string_tuple_with_int : string * int
     val cty_string_tuple_with_int53p : string * Kxclib.int53p
     val cty_string_tuple_with_float : string * float
     val cty_string_tuple_with_string : string * string
     val cty_string_tuple_with_uchar : string * Uchar.t
     val cty_string_tuple_with_byte : string * char
     val cty_string_tuple_with_bytes : string * Bytes.t
     val cty_uchar : Uchar.t
     val cty_uchar_option : Uchar.t option
     val cty_uchar_list : Uchar.t list
     val cty_uchar_map : (string * Uchar.t) list
     val cty_uchar_tuple_with_unit : Uchar.t * unit
     val cty_uchar_tuple_with_bool : Uchar.t * bool
     val cty_uchar_tuple_with_int : Uchar.t * int
     val cty_uchar_tuple_with_int53p : Uchar.t * Kxclib.int53p
     val cty_uchar_tuple_with_float : Uchar.t * float
     val cty_uchar_tuple_with_string : Uchar.t * string
     val cty_uchar_tuple_with_uchar : Uchar.t * Uchar.t
     val cty_uchar_tuple_with_byte : Uchar.t * char
     val cty_uchar_tuple_with_bytes : Uchar.t * Bytes.t
     val cty_byte : char
     val cty_byte_option : char option
     val cty_byte_list : char list
     val cty_byte_map : (string * char) list
     val cty_byte_tuple_with_unit : char * unit
     val cty_byte_tuple_with_bool : char * bool
     val cty_byte_tuple_with_int : char * int
     val cty_byte_tuple_with_int53p : char * Kxclib.int53p
     val cty_byte_tuple_with_float : char * float
     val cty_byte_tuple_with_string : char * string
     val cty_byte_tuple_with_uchar : char * Uchar.t
     val cty_byte_tuple_with_byte : char * char
     val cty_byte_tuple_with_bytes : char * Bytes.t
     val cty_bytes : Bytes.t
     val cty_bytes_option : Bytes.t option
     val cty_bytes_list : Bytes.t list
     val cty_bytes_map : (string * Bytes.t) list
     val cty_bytes_tuple_with_unit : Bytes.t * unit
     val cty_bytes_tuple_with_bool : Bytes.t * bool
     val cty_bytes_tuple_with_int : Bytes.t * int
     val cty_bytes_tuple_with_int53p : Bytes.t * Kxclib.int53p
     val cty_bytes_tuple_with_float : Bytes.t * float
     val cty_bytes_tuple_with_string : Bytes.t * string
     val cty_bytes_tuple_with_uchar : Bytes.t * Uchar.t
     val cty_bytes_tuple_with_byte : Bytes.t * char
     val cty_bytes_tuple_with_bytes : Bytes.t * Bytes.t
     val cty_string_enum : [ `a | `b | `c ]

     val td_ex_coretype_various_prim_types :
       Bindoj_test_common_typedesc_generated_examples.Ex_coretype
       .ex_coretype_various_prim_types

     val td_ex_coretype_with_int53p :
       Bindoj_test_common_typedesc_generated_examples.Ex_coretype
       .ex_coretype_with_int53p

     val td_ex_coretype_various_complex_types :
       Bindoj_test_common_typedesc_generated_examples.Ex_coretype
       .ex_coretype_various_complex_types

     val td_ex_coretype_various_tuple_types :
       Bindoj_test_common_typedesc_generated_examples.Ex_coretype
       .ex_coretype_various_tuple_types

     val td_ex_coretype_named_json :
       Bindoj_test_common_typedesc_generated_examples.Ex_coretype
       .ex_coretype_named_json

     val td_ex_alias_unit :
       Bindoj_test_common_typedesc_generated_examples.Ex_alias.ex_alias_unit

     val td_ex_alias_int_opt :
       Bindoj_test_common_typedesc_generated_examples.Ex_alias.ex_alias_int_opt

     val td_ex_alias_objtuple :
       Bindoj_test_common_typedesc_generated_examples.Ex_alias.ex_alias_objtuple

     val td_ex_record_student :
       Bindoj_test_common_typedesc_generated_examples.Ex_record
       .ex_record_student

     val td_ex_record_teacher :
       Bindoj_test_common_typedesc_generated_examples.Ex_record
       .ex_record_teacher

     val td_ex_variant_person :
       Bindoj_test_common_typedesc_generated_examples.Ex_variant
       .ex_variant_person

     val td_ex_variant_person_reused :
       Bindoj_test_common_typedesc_generated_examples.Ex_variant
       .ex_variant_person_reused

     val td_ex_variant_int_list :
       Bindoj_test_common_typedesc_generated_examples.Ex_variant
       .ex_variant_int_list

     val td_ex_variant_int_list_objtuple :
       Bindoj_test_common_typedesc_generated_examples.Ex_variant
       .ex_variant_int_list_objtuple

     val td_ex_variant_foo :
       Bindoj_test_common_typedesc_generated_examples.Ex_variant.ex_variant_foo

     val td_ex_variant_customized_union :
       Bindoj_test_common_typedesc_generated_examples.Ex_variant
       .ex_variant_customized_union

     val td_ex_mangling_student_inherited :
       Bindoj_test_common_typedesc_generated_examples.Ex_mangling
       .ex_mangling_student_inherited

     val td_ex_mangling_person_no_mangling :
       Bindoj_test_common_typedesc_generated_examples.Ex_mangling
       .ex_mangling_person_no_mangling

     val td_ex_mangling_person_inherited :
       Bindoj_test_common_typedesc_generated_examples.Ex_mangling
       .ex_mangling_person_inherited

     val td_ex_mangling_enum :
       Bindoj_test_common_typedesc_generated_examples.Ex_mangling
       .ex_mangling_enum

     val td_ex_optional_xy_opt :
       Bindoj_test_common_typedesc_generated_examples.Ex_optional
       .ex_optional_xy_opt

     val td_ex_optional_variant :
       Bindoj_test_common_typedesc_generated_examples.Ex_optional
       .ex_optional_variant

     val td_ex_ident_student_pair :
       Bindoj_test_common_typedesc_generated_examples.Ex_ident
       .ex_ident_student_pair

     val td_ex_nested_point2 :
       Bindoj_test_common_typedesc_generated_examples.Ex_nested.ex_nested_point2

     val td_ex_nested_record :
       Bindoj_test_common_typedesc_generated_examples.Ex_nested.ex_nested_record

     val td_ex_nested_variant :
       Bindoj_test_common_typedesc_generated_examples.Ex_nested
       .ex_nested_variant

     val td_ex_nested_multiply_record :
       Bindoj_test_common_typedesc_generated_examples.Ex_nested_multiply
       .ex_nested_multiply_record

     val td_ex_nested_multiply_variant :
       Bindoj_test_common_typedesc_generated_examples.Ex_nested_multiply
       .ex_nested_multiply_variant

     val td_ex_version_substring_record_v3_2_1 :
       Bindoj_test_common_typedesc_generated_examples.Ex_version_substring
       .ex_version_substring_record_v3_2_1

     val td_ex_version_substring_variant_v1_0 :
       Bindoj_test_common_typedesc_generated_examples.Ex_version_substring
       .ex_version_substring_variant_v1_0
   end)
  -> sig
  include Concrete_bridge

  val endemic_full_bridge : Bindoj_objintf_shared.endemic_full_bridge_reference
end

module Full_bridge_with_jsoo : Endemic_setup_only_full_bridge =
functor
  (M : sig
     val cty_unit : unit
     val cty_unit_option : unit option
     val cty_unit_list : unit list
     val cty_unit_map : (string * unit) list
     val cty_unit_tuple_with_unit : unit * unit
     val cty_unit_tuple_with_bool : unit * bool
     val cty_unit_tuple_with_int : unit * int
     val cty_unit_tuple_with_int53p : unit * Kxclib.int53p
     val cty_unit_tuple_with_float : unit * float
     val cty_unit_tuple_with_string : unit * string
     val cty_unit_tuple_with_uchar : unit * Uchar.t
     val cty_unit_tuple_with_byte : unit * char
     val cty_unit_tuple_with_bytes : unit * Bytes.t
     val cty_bool : bool
     val cty_bool_option : bool option
     val cty_bool_list : bool list
     val cty_bool_map : (string * bool) list
     val cty_bool_tuple_with_unit : bool * unit
     val cty_bool_tuple_with_bool : bool * bool
     val cty_bool_tuple_with_int : bool * int
     val cty_bool_tuple_with_int53p : bool * Kxclib.int53p
     val cty_bool_tuple_with_float : bool * float
     val cty_bool_tuple_with_string : bool * string
     val cty_bool_tuple_with_uchar : bool * Uchar.t
     val cty_bool_tuple_with_byte : bool * char
     val cty_bool_tuple_with_bytes : bool * Bytes.t
     val cty_int : int
     val cty_int_option : int option
     val cty_int_list : int list
     val cty_int_map : (string * int) list
     val cty_int_tuple_with_unit : int * unit
     val cty_int_tuple_with_bool : int * bool
     val cty_int_tuple_with_int : int * int
     val cty_int_tuple_with_int53p : int * Kxclib.int53p
     val cty_int_tuple_with_float : int * float
     val cty_int_tuple_with_string : int * string
     val cty_int_tuple_with_uchar : int * Uchar.t
     val cty_int_tuple_with_byte : int * char
     val cty_int_tuple_with_bytes : int * Bytes.t
     val cty_int53p : Kxclib.int53p
     val cty_int53p_option : Kxclib.int53p option
     val cty_int53p_list : Kxclib.int53p list
     val cty_int53p_map : (string * Kxclib.int53p) list
     val cty_int53p_tuple_with_unit : Kxclib.int53p * unit
     val cty_int53p_tuple_with_bool : Kxclib.int53p * bool
     val cty_int53p_tuple_with_int : Kxclib.int53p * int
     val cty_int53p_tuple_with_int53p : Kxclib.int53p * Kxclib.int53p
     val cty_int53p_tuple_with_float : Kxclib.int53p * float
     val cty_int53p_tuple_with_string : Kxclib.int53p * string
     val cty_int53p_tuple_with_uchar : Kxclib.int53p * Uchar.t
     val cty_int53p_tuple_with_byte : Kxclib.int53p * char
     val cty_int53p_tuple_with_bytes : Kxclib.int53p * Bytes.t
     val cty_float : float
     val cty_float_option : float option
     val cty_float_list : float list
     val cty_float_map : (string * float) list
     val cty_float_tuple_with_unit : float * unit
     val cty_float_tuple_with_bool : float * bool
     val cty_float_tuple_with_int : float * int
     val cty_float_tuple_with_int53p : float * Kxclib.int53p
     val cty_float_tuple_with_float : float * float
     val cty_float_tuple_with_string : float * string
     val cty_float_tuple_with_uchar : float * Uchar.t
     val cty_float_tuple_with_byte : float * char
     val cty_float_tuple_with_bytes : float * Bytes.t
     val cty_string : string
     val cty_string_option : string option
     val cty_string_list : string list
     val cty_string_map : (string * string) list
     val cty_string_tuple_with_unit : string * unit
     val cty_string_tuple_with_bool : string * bool
     val cty_string_tuple_with_int : string * int
     val cty_string_tuple_with_int53p : string * Kxclib.int53p
     val cty_string_tuple_with_float : string * float
     val cty_string_tuple_with_string : string * string
     val cty_string_tuple_with_uchar : string * Uchar.t
     val cty_string_tuple_with_byte : string * char
     val cty_string_tuple_with_bytes : string * Bytes.t
     val cty_uchar : Uchar.t
     val cty_uchar_option : Uchar.t option
     val cty_uchar_list : Uchar.t list
     val cty_uchar_map : (string * Uchar.t) list
     val cty_uchar_tuple_with_unit : Uchar.t * unit
     val cty_uchar_tuple_with_bool : Uchar.t * bool
     val cty_uchar_tuple_with_int : Uchar.t * int
     val cty_uchar_tuple_with_int53p : Uchar.t * Kxclib.int53p
     val cty_uchar_tuple_with_float : Uchar.t * float
     val cty_uchar_tuple_with_string : Uchar.t * string
     val cty_uchar_tuple_with_uchar : Uchar.t * Uchar.t
     val cty_uchar_tuple_with_byte : Uchar.t * char
     val cty_uchar_tuple_with_bytes : Uchar.t * Bytes.t
     val cty_byte : char
     val cty_byte_option : char option
     val cty_byte_list : char list
     val cty_byte_map : (string * char) list
     val cty_byte_tuple_with_unit : char * unit
     val cty_byte_tuple_with_bool : char * bool
     val cty_byte_tuple_with_int : char * int
     val cty_byte_tuple_with_int53p : char * Kxclib.int53p
     val cty_byte_tuple_with_float : char * float
     val cty_byte_tuple_with_string : char * string
     val cty_byte_tuple_with_uchar : char * Uchar.t
     val cty_byte_tuple_with_byte : char * char
     val cty_byte_tuple_with_bytes : char * Bytes.t
     val cty_bytes : Bytes.t
     val cty_bytes_option : Bytes.t option
     val cty_bytes_list : Bytes.t list
     val cty_bytes_map : (string * Bytes.t) list
     val cty_bytes_tuple_with_unit : Bytes.t * unit
     val cty_bytes_tuple_with_bool : Bytes.t * bool
     val cty_bytes_tuple_with_int : Bytes.t * int
     val cty_bytes_tuple_with_int53p : Bytes.t * Kxclib.int53p
     val cty_bytes_tuple_with_float : Bytes.t * float
     val cty_bytes_tuple_with_string : Bytes.t * string
     val cty_bytes_tuple_with_uchar : Bytes.t * Uchar.t
     val cty_bytes_tuple_with_byte : Bytes.t * char
     val cty_bytes_tuple_with_bytes : Bytes.t * Bytes.t
     val cty_string_enum : [ `a | `b | `c ]

     val td_ex_coretype_various_prim_types :
       Bindoj_test_common_typedesc_generated_examples.Ex_coretype
       .ex_coretype_various_prim_types

     val td_ex_coretype_with_int53p :
       Bindoj_test_common_typedesc_generated_examples.Ex_coretype
       .ex_coretype_with_int53p

     val td_ex_coretype_various_complex_types :
       Bindoj_test_common_typedesc_generated_examples.Ex_coretype
       .ex_coretype_various_complex_types

     val td_ex_coretype_various_tuple_types :
       Bindoj_test_common_typedesc_generated_examples.Ex_coretype
       .ex_coretype_various_tuple_types

     val td_ex_coretype_named_json :
       Bindoj_test_common_typedesc_generated_examples.Ex_coretype
       .ex_coretype_named_json

     val td_ex_alias_unit :
       Bindoj_test_common_typedesc_generated_examples.Ex_alias.ex_alias_unit

     val td_ex_alias_int_opt :
       Bindoj_test_common_typedesc_generated_examples.Ex_alias.ex_alias_int_opt

     val td_ex_alias_objtuple :
       Bindoj_test_common_typedesc_generated_examples.Ex_alias.ex_alias_objtuple

     val td_ex_record_student :
       Bindoj_test_common_typedesc_generated_examples.Ex_record
       .ex_record_student

     val td_ex_record_teacher :
       Bindoj_test_common_typedesc_generated_examples.Ex_record
       .ex_record_teacher

     val td_ex_variant_person :
       Bindoj_test_common_typedesc_generated_examples.Ex_variant
       .ex_variant_person

     val td_ex_variant_person_reused :
       Bindoj_test_common_typedesc_generated_examples.Ex_variant
       .ex_variant_person_reused

     val td_ex_variant_int_list :
       Bindoj_test_common_typedesc_generated_examples.Ex_variant
       .ex_variant_int_list

     val td_ex_variant_int_list_objtuple :
       Bindoj_test_common_typedesc_generated_examples.Ex_variant
       .ex_variant_int_list_objtuple

     val td_ex_variant_foo :
       Bindoj_test_common_typedesc_generated_examples.Ex_variant.ex_variant_foo

     val td_ex_variant_customized_union :
       Bindoj_test_common_typedesc_generated_examples.Ex_variant
       .ex_variant_customized_union

     val td_ex_mangling_student_inherited :
       Bindoj_test_common_typedesc_generated_examples.Ex_mangling
       .ex_mangling_student_inherited

     val td_ex_mangling_person_no_mangling :
       Bindoj_test_common_typedesc_generated_examples.Ex_mangling
       .ex_mangling_person_no_mangling

     val td_ex_mangling_person_inherited :
       Bindoj_test_common_typedesc_generated_examples.Ex_mangling
       .ex_mangling_person_inherited

     val td_ex_mangling_enum :
       Bindoj_test_common_typedesc_generated_examples.Ex_mangling
       .ex_mangling_enum

     val td_ex_optional_xy_opt :
       Bindoj_test_common_typedesc_generated_examples.Ex_optional
       .ex_optional_xy_opt

     val td_ex_optional_variant :
       Bindoj_test_common_typedesc_generated_examples.Ex_optional
       .ex_optional_variant

     val td_ex_ident_student_pair :
       Bindoj_test_common_typedesc_generated_examples.Ex_ident
       .ex_ident_student_pair

     val td_ex_nested_point2 :
       Bindoj_test_common_typedesc_generated_examples.Ex_nested.ex_nested_point2

     val td_ex_nested_record :
       Bindoj_test_common_typedesc_generated_examples.Ex_nested.ex_nested_record

     val td_ex_nested_variant :
       Bindoj_test_common_typedesc_generated_examples.Ex_nested
       .ex_nested_variant

     val td_ex_nested_multiply_record :
       Bindoj_test_common_typedesc_generated_examples.Ex_nested_multiply
       .ex_nested_multiply_record

     val td_ex_nested_multiply_variant :
       Bindoj_test_common_typedesc_generated_examples.Ex_nested_multiply
       .ex_nested_multiply_variant

     val td_ex_version_substring_record_v3_2_1 :
       Bindoj_test_common_typedesc_generated_examples.Ex_version_substring
       .ex_version_substring_record_v3_2_1

     val td_ex_version_substring_variant_v1_0 :
       Bindoj_test_common_typedesc_generated_examples.Ex_version_substring
       .ex_version_substring_variant_v1_0
   end)
  ->
  struct
    include Concrete_bridge_interfaces

    let unwrap_endemic
        (Bindoj_objintf_shared.Endemic_object { bridge = Br; underlying }) =
      underlying
    [@@warning "-32"]

    and wrap_peer access raw_object =
      Bindoj_objintf_shared.Peer_object { bridge = Br; access; raw_object }
    [@@warning "-32"]

    let encode_unit_to_js () = Js_of_ocaml.Js.null [@@warning "-32"]

    and encode_uchar_to_js x =
      Js_of_ocaml.Js.string (String.of_seq (List.to_seq [ Uchar.to_char x ]))
    [@@warning "-32"]

    and encode_string_to_js = Js_of_ocaml.Js.string [@@warning "-32"]

    and encode_option_to_js js_of_t x =
      let open Js_of_ocaml.Js.Opt in
      map (option x) js_of_t
    [@@warning "-32"]

    and encode_map_to_js key_to_string js_of_v fields =
      fields
      |> List.map (fun (k, v) ->
             ( key_to_string k,
               (Obj.magic (js_of_v v) : Js_of_ocaml.Js.Unsafe.any) ))
      |> Array.of_list |> Js_of_ocaml.Js.Unsafe.obj
    [@@warning "-32"]

    and encode_list_to_js js_of_t xs =
      Array.of_list xs |> Array.map js_of_t |> Js_of_ocaml.Js.array
    [@@warning "-32"]

    and encode_int53p_to_js = Kxclib.Int53p.to_float [@@warning "-32"]
    and encode_int_to_js = float_of_int [@@warning "-32"]
    and encode_float_to_js (x : float) = x [@@warning "-32"]

    and encode_bytes_to_js =
      let open Js_of_ocaml in
      let encoder =
        Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "TextEncoder") [||]
      in
      fun bytes ->
        Js.Unsafe.meth_call encoder "encode"
          [| Js.Unsafe.inject (Js.string (Bytes.to_string bytes)) |]
    [@@warning "-32"]

    and encode_byte_to_js x = float_of_int (int_of_char x) [@@warning "-32"]
    and encode_bool_to_js = Js_of_ocaml.Js.bool [@@warning "-32"]

    let ref_setup_called = ref false

    module Endemic_object_registry = struct
      open Bindoj_runtime [@@warning "-33"]
    end

    let endemic_full_bridge =
      (let open Js_of_ocaml.Js in
       Unsafe.obj
         [|
           ( "setupCalled",
             Unsafe.inject (Unsafe.callback (fun () -> !ref_setup_called)) );
           ( "setup",
             Unsafe.inject
               (Unsafe.callback
                  (fun
                    (peer_full_bridge :
                      Bindoj_objintf_shared.endemic_full_bridge_reference)
                  ->
                    let () =
                      if !ref_setup_called then
                        raise Bindoj_objintf_shared.Full_Bridge_already_setup;
                      ref_setup_called := true
                    in
                    (let open Endemic_object_registry in
                     let open Js_of_ocaml in
                     Js.Unsafe.set
                       (Js.Unsafe.get peer_full_bridge "instance")
                       "peerObjectRegistry"
                       (let open Js_of_ocaml.Js in
                        Unsafe.obj [||])) [@warning "-33"];
                    let open Js_of_ocaml in
                    Js.Unsafe.set
                      (Js.Unsafe.get peer_full_bridge "instance")
                      "peerObjects"
                      (let open Js_of_ocaml.Js in
                       Unsafe.obj
                         [|
                           ( "ctyUnit",
                             Unsafe.inject (encode_unit_to_js M.cty_unit) );
                           ( "ctyUnitOption",
                             Unsafe.inject
                               ((encode_option_to_js encode_unit_to_js)
                                  M.cty_unit_option) );
                           ( "ctyUnitList",
                             Unsafe.inject
                               ((encode_list_to_js encode_unit_to_js)
                                  M.cty_unit_list) );
                           ( "ctyUnitMap",
                             Unsafe.inject
                               ((encode_map_to_js
                                   (fun k -> k)
                                   encode_unit_to_js)
                                  M.cty_unit_map) );
                           ( "ctyUnitTupleWithUnit",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_unit_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_unit_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_unit_tuple_with_unit) );
                           ( "ctyUnitTupleWithBool",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_unit_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_bool_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_unit_tuple_with_bool) );
                           ( "ctyUnitTupleWithInt",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_unit_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_int_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_unit_tuple_with_int) );
                           ( "ctyUnitTupleWithInt53p",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_unit_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_int53p_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_unit_tuple_with_int53p) );
                           ( "ctyUnitTupleWithFloat",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_unit_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_float_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_unit_tuple_with_float) );
                           ( "ctyUnitTupleWithString",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_unit_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_string_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_unit_tuple_with_string) );
                           ( "ctyUnitTupleWithUchar",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_unit_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_uchar_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_unit_tuple_with_uchar) );
                           ( "ctyUnitTupleWithByte",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_unit_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_byte_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_unit_tuple_with_byte) );
                           ( "ctyUnitTupleWithBytes",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_unit_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_bytes_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_unit_tuple_with_bytes) );
                           ( "ctyBool",
                             Unsafe.inject (encode_bool_to_js M.cty_bool) );
                           ( "ctyBoolOption",
                             Unsafe.inject
                               ((encode_option_to_js encode_bool_to_js)
                                  M.cty_bool_option) );
                           ( "ctyBoolList",
                             Unsafe.inject
                               ((encode_list_to_js encode_bool_to_js)
                                  M.cty_bool_list) );
                           ( "ctyBoolMap",
                             Unsafe.inject
                               ((encode_map_to_js
                                   (fun k -> k)
                                   encode_bool_to_js)
                                  M.cty_bool_map) );
                           ( "ctyBoolTupleWithUnit",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_bool_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_unit_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_bool_tuple_with_unit) );
                           ( "ctyBoolTupleWithBool",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_bool_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_bool_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_bool_tuple_with_bool) );
                           ( "ctyBoolTupleWithInt",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_bool_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_int_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_bool_tuple_with_int) );
                           ( "ctyBoolTupleWithInt53p",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_bool_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_int53p_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_bool_tuple_with_int53p) );
                           ( "ctyBoolTupleWithFloat",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_bool_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_float_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_bool_tuple_with_float) );
                           ( "ctyBoolTupleWithString",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_bool_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_string_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_bool_tuple_with_string) );
                           ( "ctyBoolTupleWithUchar",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_bool_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_uchar_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_bool_tuple_with_uchar) );
                           ( "ctyBoolTupleWithByte",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_bool_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_byte_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_bool_tuple_with_byte) );
                           ( "ctyBoolTupleWithBytes",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_bool_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_bytes_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_bool_tuple_with_bytes) );
                           ("ctyInt", Unsafe.inject (encode_int_to_js M.cty_int));
                           ( "ctyIntOption",
                             Unsafe.inject
                               ((encode_option_to_js encode_int_to_js)
                                  M.cty_int_option) );
                           ( "ctyIntList",
                             Unsafe.inject
                               ((encode_list_to_js encode_int_to_js)
                                  M.cty_int_list) );
                           ( "ctyIntMap",
                             Unsafe.inject
                               ((encode_map_to_js (fun k -> k) encode_int_to_js)
                                  M.cty_int_map) );
                           ( "ctyIntTupleWithUnit",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_int_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_unit_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_int_tuple_with_unit) );
                           ( "ctyIntTupleWithBool",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_int_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_bool_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_int_tuple_with_bool) );
                           ( "ctyIntTupleWithInt",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_int_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_int_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_int_tuple_with_int) );
                           ( "ctyIntTupleWithInt53p",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_int_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_int53p_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_int_tuple_with_int53p) );
                           ( "ctyIntTupleWithFloat",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_int_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_float_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_int_tuple_with_float) );
                           ( "ctyIntTupleWithString",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_int_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_string_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_int_tuple_with_string) );
                           ( "ctyIntTupleWithUchar",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_int_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_uchar_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_int_tuple_with_uchar) );
                           ( "ctyIntTupleWithByte",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_int_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_byte_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_int_tuple_with_byte) );
                           ( "ctyIntTupleWithBytes",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_int_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_bytes_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_int_tuple_with_bytes) );
                           ( "ctyInt53p",
                             Unsafe.inject (encode_int53p_to_js M.cty_int53p) );
                           ( "ctyInt53pOption",
                             Unsafe.inject
                               ((encode_option_to_js encode_int53p_to_js)
                                  M.cty_int53p_option) );
                           ( "ctyInt53pList",
                             Unsafe.inject
                               ((encode_list_to_js encode_int53p_to_js)
                                  M.cty_int53p_list) );
                           ( "ctyInt53pMap",
                             Unsafe.inject
                               ((encode_map_to_js
                                   (fun k -> k)
                                   encode_int53p_to_js)
                                  M.cty_int53p_map) );
                           ( "ctyInt53pTupleWithUnit",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_int53p_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_unit_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_int53p_tuple_with_unit) );
                           ( "ctyInt53pTupleWithBool",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_int53p_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_bool_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_int53p_tuple_with_bool) );
                           ( "ctyInt53pTupleWithInt",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_int53p_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_int_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_int53p_tuple_with_int) );
                           ( "ctyInt53pTupleWithInt53p",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_int53p_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_int53p_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_int53p_tuple_with_int53p) );
                           ( "ctyInt53pTupleWithFloat",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_int53p_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_float_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_int53p_tuple_with_float) );
                           ( "ctyInt53pTupleWithString",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_int53p_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_string_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_int53p_tuple_with_string) );
                           ( "ctyInt53pTupleWithUchar",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_int53p_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_uchar_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_int53p_tuple_with_uchar) );
                           ( "ctyInt53pTupleWithByte",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_int53p_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_byte_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_int53p_tuple_with_byte) );
                           ( "ctyInt53pTupleWithBytes",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_int53p_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_bytes_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_int53p_tuple_with_bytes) );
                           ( "ctyFloat",
                             Unsafe.inject (encode_float_to_js M.cty_float) );
                           ( "ctyFloatOption",
                             Unsafe.inject
                               ((encode_option_to_js encode_float_to_js)
                                  M.cty_float_option) );
                           ( "ctyFloatList",
                             Unsafe.inject
                               ((encode_list_to_js encode_float_to_js)
                                  M.cty_float_list) );
                           ( "ctyFloatMap",
                             Unsafe.inject
                               ((encode_map_to_js
                                   (fun k -> k)
                                   encode_float_to_js)
                                  M.cty_float_map) );
                           ( "ctyFloatTupleWithUnit",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_float_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_unit_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_float_tuple_with_unit) );
                           ( "ctyFloatTupleWithBool",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_float_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_bool_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_float_tuple_with_bool) );
                           ( "ctyFloatTupleWithInt",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_float_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_int_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_float_tuple_with_int) );
                           ( "ctyFloatTupleWithInt53p",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_float_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_int53p_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_float_tuple_with_int53p) );
                           ( "ctyFloatTupleWithFloat",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_float_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_float_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_float_tuple_with_float) );
                           ( "ctyFloatTupleWithString",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_float_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_string_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_float_tuple_with_string) );
                           ( "ctyFloatTupleWithUchar",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_float_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_uchar_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_float_tuple_with_uchar) );
                           ( "ctyFloatTupleWithByte",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_float_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_byte_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_float_tuple_with_byte) );
                           ( "ctyFloatTupleWithBytes",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_float_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_bytes_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_float_tuple_with_bytes) );
                           ( "ctyString",
                             Unsafe.inject (encode_string_to_js M.cty_string) );
                           ( "ctyStringOption",
                             Unsafe.inject
                               ((encode_option_to_js encode_string_to_js)
                                  M.cty_string_option) );
                           ( "ctyStringList",
                             Unsafe.inject
                               ((encode_list_to_js encode_string_to_js)
                                  M.cty_string_list) );
                           ( "ctyStringMap",
                             Unsafe.inject
                               ((encode_map_to_js
                                   (fun k -> k)
                                   encode_string_to_js)
                                  M.cty_string_map) );
                           ( "ctyStringTupleWithUnit",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_string_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_unit_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_string_tuple_with_unit) );
                           ( "ctyStringTupleWithBool",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_string_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_bool_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_string_tuple_with_bool) );
                           ( "ctyStringTupleWithInt",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_string_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_int_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_string_tuple_with_int) );
                           ( "ctyStringTupleWithInt53p",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_string_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_int53p_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_string_tuple_with_int53p) );
                           ( "ctyStringTupleWithFloat",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_string_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_float_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_string_tuple_with_float) );
                           ( "ctyStringTupleWithString",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_string_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_string_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_string_tuple_with_string) );
                           ( "ctyStringTupleWithUchar",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_string_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_uchar_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_string_tuple_with_uchar) );
                           ( "ctyStringTupleWithByte",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_string_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_byte_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_string_tuple_with_byte) );
                           ( "ctyStringTupleWithBytes",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_string_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_bytes_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_string_tuple_with_bytes) );
                           ( "ctyUchar",
                             Unsafe.inject (encode_uchar_to_js M.cty_uchar) );
                           ( "ctyUcharOption",
                             Unsafe.inject
                               ((encode_option_to_js encode_uchar_to_js)
                                  M.cty_uchar_option) );
                           ( "ctyUcharList",
                             Unsafe.inject
                               ((encode_list_to_js encode_uchar_to_js)
                                  M.cty_uchar_list) );
                           ( "ctyUcharMap",
                             Unsafe.inject
                               ((encode_map_to_js
                                   (fun k -> k)
                                   encode_uchar_to_js)
                                  M.cty_uchar_map) );
                           ( "ctyUcharTupleWithUnit",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_uchar_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_unit_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_uchar_tuple_with_unit) );
                           ( "ctyUcharTupleWithBool",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_uchar_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_bool_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_uchar_tuple_with_bool) );
                           ( "ctyUcharTupleWithInt",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_uchar_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_int_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_uchar_tuple_with_int) );
                           ( "ctyUcharTupleWithInt53p",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_uchar_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_int53p_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_uchar_tuple_with_int53p) );
                           ( "ctyUcharTupleWithFloat",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_uchar_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_float_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_uchar_tuple_with_float) );
                           ( "ctyUcharTupleWithString",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_uchar_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_string_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_uchar_tuple_with_string) );
                           ( "ctyUcharTupleWithUchar",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_uchar_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_uchar_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_uchar_tuple_with_uchar) );
                           ( "ctyUcharTupleWithByte",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_uchar_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_byte_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_uchar_tuple_with_byte) );
                           ( "ctyUcharTupleWithBytes",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_uchar_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_bytes_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_uchar_tuple_with_bytes) );
                           ( "ctyByte",
                             Unsafe.inject (encode_byte_to_js M.cty_byte) );
                           ( "ctyByteOption",
                             Unsafe.inject
                               ((encode_option_to_js encode_byte_to_js)
                                  M.cty_byte_option) );
                           ( "ctyByteList",
                             Unsafe.inject
                               ((encode_list_to_js encode_byte_to_js)
                                  M.cty_byte_list) );
                           ( "ctyByteMap",
                             Unsafe.inject
                               ((encode_map_to_js
                                   (fun k -> k)
                                   encode_byte_to_js)
                                  M.cty_byte_map) );
                           ( "ctyByteTupleWithUnit",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_byte_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_unit_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_byte_tuple_with_unit) );
                           ( "ctyByteTupleWithBool",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_byte_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_bool_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_byte_tuple_with_bool) );
                           ( "ctyByteTupleWithInt",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_byte_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_int_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_byte_tuple_with_int) );
                           ( "ctyByteTupleWithInt53p",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_byte_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_int53p_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_byte_tuple_with_int53p) );
                           ( "ctyByteTupleWithFloat",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_byte_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_float_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_byte_tuple_with_float) );
                           ( "ctyByteTupleWithString",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_byte_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_string_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_byte_tuple_with_string) );
                           ( "ctyByteTupleWithUchar",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_byte_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_uchar_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_byte_tuple_with_uchar) );
                           ( "ctyByteTupleWithByte",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_byte_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_byte_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_byte_tuple_with_byte) );
                           ( "ctyByteTupleWithBytes",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_byte_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_bytes_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_byte_tuple_with_bytes) );
                           ( "ctyBytes",
                             Unsafe.inject (encode_bytes_to_js M.cty_bytes) );
                           ( "ctyBytesOption",
                             Unsafe.inject
                               ((encode_option_to_js encode_bytes_to_js)
                                  M.cty_bytes_option) );
                           ( "ctyBytesList",
                             Unsafe.inject
                               ((encode_list_to_js encode_bytes_to_js)
                                  M.cty_bytes_list) );
                           ( "ctyBytesMap",
                             Unsafe.inject
                               ((encode_map_to_js
                                   (fun k -> k)
                                   encode_bytes_to_js)
                                  M.cty_bytes_map) );
                           ( "ctyBytesTupleWithUnit",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_bytes_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_unit_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_bytes_tuple_with_unit) );
                           ( "ctyBytesTupleWithBool",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_bytes_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_bool_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_bytes_tuple_with_bool) );
                           ( "ctyBytesTupleWithInt",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_bytes_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_int_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_bytes_tuple_with_int) );
                           ( "ctyBytesTupleWithInt53p",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_bytes_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_int53p_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_bytes_tuple_with_int53p) );
                           ( "ctyBytesTupleWithFloat",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_bytes_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_float_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_bytes_tuple_with_float) );
                           ( "ctyBytesTupleWithString",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_bytes_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_string_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_bytes_tuple_with_string) );
                           ( "ctyBytesTupleWithUchar",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_bytes_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_uchar_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_bytes_tuple_with_uchar) );
                           ( "ctyBytesTupleWithByte",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_bytes_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_byte_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_bytes_tuple_with_byte) );
                           ( "ctyBytesTupleWithBytes",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.array
                                    [|
                                      (Obj.magic (encode_bytes_to_js x0)
                                        : 'a Js_of_ocaml.Js.t);
                                      (Obj.magic (encode_bytes_to_js x1)
                                        : 'a Js_of_ocaml.Js.t);
                                    |])
                                  M.cty_bytes_tuple_with_bytes) );
                           ( "ctyStringEnum",
                             Unsafe.inject
                               ((let open Js_of_ocaml in
                                 function
                                 | `a -> Js.string "a"
                                 | `b -> Js.string "b"
                                 | `c -> Js.string "c")
                                  M.cty_string_enum) );
                           ( "tdExCoretypeVariousPrimTypes",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_coretype in
                                   ex_coretype_various_prim_types_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_coretype_various_prim_types) );
                           ( "tdExCoretypeWithInt53p",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_coretype in
                                   ex_coretype_with_int53p_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_coretype_with_int53p) );
                           ( "tdExCoretypeVariousComplexTypes",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_coretype in
                                   ex_coretype_various_complex_types_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_coretype_various_complex_types) );
                           ( "tdExCoretypeVariousTupleTypes",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_coretype in
                                   ex_coretype_various_tuple_types_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_coretype_various_tuple_types) );
                           ( "tdExCoretypeNamedJson",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_coretype in
                                   ex_coretype_named_json_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_coretype_named_json) );
                           ( "tdExAliasUnit",
                             Unsafe.inject
                               (encode_unit_to_js M.td_ex_alias_unit) );
                           ( "tdExAliasIntOpt",
                             Unsafe.inject
                               ((encode_option_to_js encode_int_to_js)
                                  M.td_ex_alias_int_opt) );
                           ( "tdExAliasObjtuple",
                             Unsafe.inject
                               ((fun (x0, x1) ->
                                  Js_of_ocaml.Js.Unsafe.obj
                                    [|
                                      ( "_0",
                                        (Obj.magic (encode_float_to_js x0)
                                          : Js_of_ocaml.Js.Unsafe.any) );
                                      ( "_1",
                                        (Obj.magic (encode_string_to_js x1)
                                          : Js_of_ocaml.Js.Unsafe.any) );
                                    |])
                                  M.td_ex_alias_objtuple) );
                           ( "tdExRecordStudent",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_record in
                                   ex_record_student_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_record_student) );
                           ( "tdExRecordTeacher",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_record in
                                   ex_record_teacher_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_record_teacher) );
                           ( "tdExVariantPerson",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_variant in
                                   ex_variant_person_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_variant_person) );
                           ( "tdExVariantPersonReused",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_variant in
                                   ex_variant_person_reused_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_variant_person_reused) );
                           ( "tdExVariantIntList",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_variant in
                                   ex_variant_int_list_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_variant_int_list) );
                           ( "tdExVariantIntListObjtuple",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_variant in
                                   ex_variant_int_list_objtuple_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_variant_int_list_objtuple) );
                           ( "tdExVariantFoo",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_variant in
                                   ex_variant_foo_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_variant_foo) );
                           ( "tdExVariantCustomizedUnion",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_variant in
                                   ex_variant_customized_union_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_variant_customized_union) );
                           ( "tdExManglingStudentInherited",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_mangling in
                                   ex_mangling_student_inherited_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_mangling_student_inherited) );
                           ( "tdExManglingPersonNoMangling",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_mangling in
                                   ex_mangling_person_no_mangling_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_mangling_person_no_mangling) );
                           ( "tdExManglingPersonInherited",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_mangling in
                                   ex_mangling_person_inherited_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_mangling_person_inherited) );
                           ( "tdExManglingEnum",
                             Unsafe.inject
                               ((let open Js_of_ocaml in
                                 function
                                 | `Case_at0 -> Js.string "Case-at0"
                                 | `case_at1 -> Js.string "case-at1"
                                 | `Case_at2 -> Js.string "Case-at2"
                                 | `Case_at3 -> Js.string "Case-third")
                                  M.td_ex_mangling_enum) );
                           ( "tdExOptionalXyOpt",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_optional in
                                   ex_optional_xy_opt_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_optional_xy_opt) );
                           ( "tdExOptionalVariant",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_optional in
                                   ex_optional_variant_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_optional_variant) );
                           ( "tdExIdentStudentPair",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_ident in
                                   ex_ident_student_pair_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_ident_student_pair) );
                           ( "tdExNestedPoint2",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_nested in
                                   ex_nested_point2_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_nested_point2) );
                           ( "tdExNestedRecord",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_nested in
                                   ex_nested_record_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_nested_record) );
                           ( "tdExNestedVariant",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_nested in
                                   ex_nested_variant_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_nested_variant) );
                           ( "tdExNestedMultiplyRecord",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_nested_multiply in
                                   ex_nested_multiply_record_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_nested_multiply_record) );
                           ( "tdExNestedMultiplyVariant",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_nested_multiply in
                                   ex_nested_multiply_variant_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_nested_multiply_variant) );
                           ( "tdExVersionSubstringRecordV3_2_1",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_version_substring in
                                   ex_version_substring_record_v3_2_1_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_version_substring_record_v3_2_1) );
                           ( "tdExVersionSubstringVariantV1_0",
                             Unsafe.inject
                               ((fun x ->
                                  (let open
                                     Bindoj_test_common_typedesc_generated_examples
                                     .Ex_version_substring in
                                   ex_version_substring_variant_v1_0_to_json)
                                    x
                                  |> Kxclib_js.Json_ext.to_xjv)
                                  M.td_ex_version_substring_variant_v1_0) );
                         |]))) );
         |])
      |> fun x ->
      (Obj.magic x : Bindoj_objintf_shared.endemic_full_bridge_reference)

    let () = ()
  end
