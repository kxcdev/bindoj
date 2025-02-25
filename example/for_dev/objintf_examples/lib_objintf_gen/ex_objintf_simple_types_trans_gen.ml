open Bindoj_objintf_gen_test_gen_utils [@@warning "-33"]
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
  open Interfaces [@@warning "-33"]

  module Peer_objects : sig
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
      Bindoj_test_common_typedesc_generated_examples.Ex_record.ex_record_student

    val td_ex_record_teacher :
      Bindoj_test_common_typedesc_generated_examples.Ex_record.ex_record_teacher

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
      Bindoj_test_common_typedesc_generated_examples.Ex_nested.ex_nested_variant

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
  end
end

open Concrete_bridge_interfaces.Interfaces [@@warning "-33"]

module type Peer_setup_only_full_bridge = sig
  module type Bridge = Concrete_bridge

  val get_bridge :
    unit -> [ `await_peer_setup | `bridge of (module Concrete_bridge) ]

  val get_bridge_async : ((module Concrete_bridge) -> unit) -> unit
  val endemic_full_bridge : Bindoj_objintf_shared.endemic_full_bridge_reference
end
