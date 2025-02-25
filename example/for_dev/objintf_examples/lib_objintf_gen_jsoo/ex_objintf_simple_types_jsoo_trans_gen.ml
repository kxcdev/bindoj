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

module Full_bridge_with_jsoo : Peer_setup_only_full_bridge = struct
  module type Bridge = Concrete_bridge

  let ref_setup_called = ref false
  let bridge_opt : (module Bridge) option ref = ref None
  let continuations = ref []

  let get_bridge () =
    match !bridge_opt with None -> `await_peer_setup | Some b -> `bridge b

  let get_bridge_async f =
    match !bridge_opt with
    | None -> continuations := f :: !continuations
    | Some b -> f b

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
                  (let open Js_of_ocaml in
                   if not (Js.Unsafe.get peer_full_bridge "setupCalled") then
                     Js.Unsafe.meth_call peer_full_bridge "setup"
                       [|
                         Js.Unsafe.inject
                           (let open Js_of_ocaml.Js in
                            Unsafe.obj
                              [|
                                ( "setupCalled",
                                  Unsafe.inject
                                    (Unsafe.callback (fun () -> true)) );
                              |]);
                       |]);
                  let concrete_bridge =
                    (module struct
                      include Concrete_bridge_interfaces
                      open Interfaces [@@warning "-33"]

                      let unwrap_endemic
                          (Bindoj_objintf_shared.Endemic_object
                             { bridge = Br; underlying }) =
                        underlying
                      [@@warning "-32"]

                      and wrap_peer access raw_object =
                        Bindoj_objintf_shared.Peer_object
                          { bridge = Br; access; raw_object }
                      [@@warning "-32"]

                      let decode_unit_of_js _ = () [@@warning "-32"]

                      and decode_uchar_of_js x =
                        let x = Js_of_ocaml.Js.to_string x in
                        if String.length x = 1 then Uchar.of_char x.[0]
                        else
                          Format.kasprintf failwith
                            "string '%s' is not a valid uchar value" x
                      [@@warning "-32"]

                      and decode_string_of_js = Js_of_ocaml.Js.to_string
                      [@@warning "-32"]

                      and decode_option_of_js js_to_t jv =
                        Js_of_ocaml.Js.Opt.to_option jv |> Option.map js_to_t
                      [@@warning "-32"]

                      and decode_map_of_js key_of_string js_to_v jv =
                        let open Js_of_ocaml in
                        let fields =
                          Js.Unsafe.fun_call "Object.entries"
                            [| Js.Unsafe.inject jv |]
                        in
                        fields |> Js.to_array
                        |> Array.map (fun elem ->
                               let [| k; v |] =
                                 Js.to_array elem
                                   [@@warning "-8"]
                               in
                               ( key_of_string
                                   (Js.to_string
                                      (Obj.magic (k : Js.Unsafe.any)
                                        : js_string Js.t)),
                                 js_to_v (Obj.magic (v : Js.Unsafe.any)) ))
                        |> Array.to_list
                      [@@warning "-32"]

                      and decode_list_of_js js_to_t jv =
                        jv |> Js_of_ocaml.Js.to_array |> Array.map js_to_t
                        |> Array.to_list
                      [@@warning "-32"]

                      and decode_int53p_of_js = Kxclib.Int53p.of_float
                      [@@warning "-32"]

                      and decode_int_of_js jv =
                        if Float.is_integer jv then int_of_float jv
                        else
                          Format.kasprintf failwith
                            "expecting an integer but the given is '%f'" jv
                      [@@warning "-32"]

                      and decode_float_of_js (x : float) = x [@@warning "-32"]

                      and decode_bytes_of_js =
                        let open Js_of_ocaml in
                        let decoder =
                          Js.Unsafe.new_obj
                            (Js.Unsafe.pure_js_expr "TextDecoder")
                            [||]
                        in
                        fun jv ->
                          Js.Unsafe.meth_call decoder "decode" [| jv |]
                          |> Js.to_string |> Bytes.of_string
                      [@@warning "-32"]

                      and decode_byte_of_js jv =
                        let x = int_of_float jv in
                        if 0 <= x && x <= 255 then char_of_int x
                        else
                          Format.kasprintf failwith
                            "number '%d' is not a valid byte value" x
                      [@@warning "-32"]

                      and decode_bool_of_js = Js_of_ocaml.Js.to_bool
                      [@@warning "-32"]

                      module Peer_object_registry = struct
                        open Bindoj_runtime [@@warning "-33"]

                        let () =
                          let open Js_of_ocaml in
                          let js_registry =
                            Js.Unsafe.get
                              (Js.Unsafe.get peer_full_bridge "instance")
                              "endemicObjectRegistry"
                              [@@warning "-26"]
                          in
                          Js.Unsafe.set
                            (Js.Unsafe.get peer_full_bridge "instance")
                            "endemicObjectRegistry"
                            (let open Js_of_ocaml.Js in
                             Unsafe.obj [||])
                      end

                      module Peer_objects = struct
                        let cty_unit =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUnit"
                          |> decode_unit_of_js

                        and cty_unit_option =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUnitOption"
                          |> decode_option_of_js decode_unit_of_js

                        and cty_unit_list =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUnitList"
                          |> decode_list_of_js decode_unit_of_js

                        and cty_unit_map =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUnitMap"
                          |> decode_map_of_js
                               (fun (s : string) -> s)
                               decode_unit_of_js

                        and cty_unit_tuple_with_unit =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUnitTupleWithUnit"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_unit_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_unit_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_unit_tuple_with_bool =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUnitTupleWithBool"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_unit_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_bool_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_unit_tuple_with_int =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUnitTupleWithInt"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_unit_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_int_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_unit_tuple_with_int53p =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUnitTupleWithInt53p"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_unit_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_int53p_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_unit_tuple_with_float =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUnitTupleWithFloat"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_unit_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_float_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_unit_tuple_with_string =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUnitTupleWithString"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_unit_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_string_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_unit_tuple_with_uchar =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUnitTupleWithUchar"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_unit_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_uchar_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_unit_tuple_with_byte =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUnitTupleWithByte"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_unit_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_byte_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_unit_tuple_with_bytes =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUnitTupleWithBytes"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_unit_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_bytes_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_bool =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBool"
                          |> decode_bool_of_js

                        and cty_bool_option =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBoolOption"
                          |> decode_option_of_js decode_bool_of_js

                        and cty_bool_list =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBoolList"
                          |> decode_list_of_js decode_bool_of_js

                        and cty_bool_map =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBoolMap"
                          |> decode_map_of_js
                               (fun (s : string) -> s)
                               decode_bool_of_js

                        and cty_bool_tuple_with_unit =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBoolTupleWithUnit"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_bool_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_unit_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_bool_tuple_with_bool =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBoolTupleWithBool"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_bool_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_bool_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_bool_tuple_with_int =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBoolTupleWithInt"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_bool_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_int_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_bool_tuple_with_int53p =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBoolTupleWithInt53p"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_bool_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_int53p_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_bool_tuple_with_float =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBoolTupleWithFloat"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_bool_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_float_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_bool_tuple_with_string =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBoolTupleWithString"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_bool_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_string_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_bool_tuple_with_uchar =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBoolTupleWithUchar"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_bool_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_uchar_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_bool_tuple_with_byte =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBoolTupleWithByte"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_bool_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_byte_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_bool_tuple_with_bytes =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBoolTupleWithBytes"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_bool_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_bytes_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_int =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyInt"
                          |> decode_int_of_js

                        and cty_int_option =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyIntOption"
                          |> decode_option_of_js decode_int_of_js

                        and cty_int_list =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyIntList"
                          |> decode_list_of_js decode_int_of_js

                        and cty_int_map =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyIntMap"
                          |> decode_map_of_js
                               (fun (s : string) -> s)
                               decode_int_of_js

                        and cty_int_tuple_with_unit =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyIntTupleWithUnit"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_int_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_unit_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_int_tuple_with_bool =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyIntTupleWithBool"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_int_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_bool_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_int_tuple_with_int =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyIntTupleWithInt"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_int_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_int_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_int_tuple_with_int53p =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyIntTupleWithInt53p"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_int_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_int53p_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_int_tuple_with_float =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyIntTupleWithFloat"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_int_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_float_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_int_tuple_with_string =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyIntTupleWithString"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_int_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_string_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_int_tuple_with_uchar =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyIntTupleWithUchar"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_int_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_uchar_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_int_tuple_with_byte =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyIntTupleWithByte"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_int_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_byte_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_int_tuple_with_bytes =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyIntTupleWithBytes"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_int_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_bytes_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_int53p =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyInt53p"
                          |> decode_int53p_of_js

                        and cty_int53p_option =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyInt53pOption"
                          |> decode_option_of_js decode_int53p_of_js

                        and cty_int53p_list =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyInt53pList"
                          |> decode_list_of_js decode_int53p_of_js

                        and cty_int53p_map =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyInt53pMap"
                          |> decode_map_of_js
                               (fun (s : string) -> s)
                               decode_int53p_of_js

                        and cty_int53p_tuple_with_unit =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyInt53pTupleWithUnit"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_int53p_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_unit_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_int53p_tuple_with_bool =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyInt53pTupleWithBool"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_int53p_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_bool_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_int53p_tuple_with_int =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyInt53pTupleWithInt"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_int53p_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_int_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_int53p_tuple_with_int53p =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyInt53pTupleWithInt53p"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_int53p_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_int53p_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_int53p_tuple_with_float =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyInt53pTupleWithFloat"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_int53p_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_float_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_int53p_tuple_with_string =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyInt53pTupleWithString"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_int53p_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_string_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_int53p_tuple_with_uchar =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyInt53pTupleWithUchar"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_int53p_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_uchar_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_int53p_tuple_with_byte =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyInt53pTupleWithByte"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_int53p_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_byte_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_int53p_tuple_with_bytes =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyInt53pTupleWithBytes"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_int53p_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_bytes_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_float =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyFloat"
                          |> decode_float_of_js

                        and cty_float_option =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyFloatOption"
                          |> decode_option_of_js decode_float_of_js

                        and cty_float_list =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyFloatList"
                          |> decode_list_of_js decode_float_of_js

                        and cty_float_map =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyFloatMap"
                          |> decode_map_of_js
                               (fun (s : string) -> s)
                               decode_float_of_js

                        and cty_float_tuple_with_unit =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyFloatTupleWithUnit"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_float_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_unit_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_float_tuple_with_bool =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyFloatTupleWithBool"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_float_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_bool_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_float_tuple_with_int =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyFloatTupleWithInt"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_float_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_int_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_float_tuple_with_int53p =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyFloatTupleWithInt53p"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_float_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_int53p_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_float_tuple_with_float =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyFloatTupleWithFloat"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_float_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_float_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_float_tuple_with_string =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyFloatTupleWithString"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_float_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_string_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_float_tuple_with_uchar =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyFloatTupleWithUchar"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_float_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_uchar_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_float_tuple_with_byte =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyFloatTupleWithByte"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_float_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_byte_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_float_tuple_with_bytes =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyFloatTupleWithBytes"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_float_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_bytes_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_string =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyString"
                          |> decode_string_of_js

                        and cty_string_option =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyStringOption"
                          |> decode_option_of_js decode_string_of_js

                        and cty_string_list =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyStringList"
                          |> decode_list_of_js decode_string_of_js

                        and cty_string_map =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyStringMap"
                          |> decode_map_of_js
                               (fun (s : string) -> s)
                               decode_string_of_js

                        and cty_string_tuple_with_unit =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyStringTupleWithUnit"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_string_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_unit_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_string_tuple_with_bool =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyStringTupleWithBool"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_string_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_bool_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_string_tuple_with_int =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyStringTupleWithInt"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_string_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_int_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_string_tuple_with_int53p =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyStringTupleWithInt53p"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_string_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_int53p_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_string_tuple_with_float =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyStringTupleWithFloat"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_string_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_float_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_string_tuple_with_string =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyStringTupleWithString"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_string_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_string_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_string_tuple_with_uchar =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyStringTupleWithUchar"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_string_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_uchar_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_string_tuple_with_byte =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyStringTupleWithByte"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_string_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_byte_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_string_tuple_with_bytes =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyStringTupleWithBytes"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_string_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_bytes_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_uchar =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUchar"
                          |> decode_uchar_of_js

                        and cty_uchar_option =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUcharOption"
                          |> decode_option_of_js decode_uchar_of_js

                        and cty_uchar_list =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUcharList"
                          |> decode_list_of_js decode_uchar_of_js

                        and cty_uchar_map =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUcharMap"
                          |> decode_map_of_js
                               (fun (s : string) -> s)
                               decode_uchar_of_js

                        and cty_uchar_tuple_with_unit =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUcharTupleWithUnit"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_uchar_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_unit_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_uchar_tuple_with_bool =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUcharTupleWithBool"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_uchar_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_bool_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_uchar_tuple_with_int =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUcharTupleWithInt"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_uchar_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_int_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_uchar_tuple_with_int53p =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUcharTupleWithInt53p"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_uchar_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_int53p_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_uchar_tuple_with_float =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUcharTupleWithFloat"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_uchar_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_float_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_uchar_tuple_with_string =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUcharTupleWithString"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_uchar_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_string_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_uchar_tuple_with_uchar =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUcharTupleWithUchar"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_uchar_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_uchar_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_uchar_tuple_with_byte =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUcharTupleWithByte"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_uchar_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_byte_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_uchar_tuple_with_bytes =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyUcharTupleWithBytes"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_uchar_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_bytes_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_byte =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyByte"
                          |> decode_byte_of_js

                        and cty_byte_option =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyByteOption"
                          |> decode_option_of_js decode_byte_of_js

                        and cty_byte_list =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyByteList"
                          |> decode_list_of_js decode_byte_of_js

                        and cty_byte_map =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyByteMap"
                          |> decode_map_of_js
                               (fun (s : string) -> s)
                               decode_byte_of_js

                        and cty_byte_tuple_with_unit =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyByteTupleWithUnit"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_byte_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_unit_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_byte_tuple_with_bool =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyByteTupleWithBool"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_byte_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_bool_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_byte_tuple_with_int =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyByteTupleWithInt"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_byte_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_int_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_byte_tuple_with_int53p =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyByteTupleWithInt53p"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_byte_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_int53p_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_byte_tuple_with_float =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyByteTupleWithFloat"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_byte_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_float_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_byte_tuple_with_string =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyByteTupleWithString"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_byte_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_string_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_byte_tuple_with_uchar =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyByteTupleWithUchar"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_byte_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_uchar_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_byte_tuple_with_byte =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyByteTupleWithByte"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_byte_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_byte_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_byte_tuple_with_bytes =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyByteTupleWithBytes"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_byte_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_bytes_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_bytes =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBytes"
                          |> decode_bytes_of_js

                        and cty_bytes_option =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBytesOption"
                          |> decode_option_of_js decode_bytes_of_js

                        and cty_bytes_list =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBytesList"
                          |> decode_list_of_js decode_bytes_of_js

                        and cty_bytes_map =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBytesMap"
                          |> decode_map_of_js
                               (fun (s : string) -> s)
                               decode_bytes_of_js

                        and cty_bytes_tuple_with_unit =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBytesTupleWithUnit"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_bytes_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_unit_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_bytes_tuple_with_bool =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBytesTupleWithBool"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_bytes_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_bool_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_bytes_tuple_with_int =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBytesTupleWithInt"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_bytes_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_int_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_bytes_tuple_with_int53p =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBytesTupleWithInt53p"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_bytes_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_int53p_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_bytes_tuple_with_float =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBytesTupleWithFloat"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_bytes_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_float_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_bytes_tuple_with_string =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBytesTupleWithString"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_bytes_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_string_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_bytes_tuple_with_uchar =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBytesTupleWithUchar"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_bytes_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_uchar_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_bytes_tuple_with_byte =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBytesTupleWithByte"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_bytes_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_byte_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_bytes_tuple_with_bytes =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyBytesTupleWithBytes"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_array jv with
                          | [| x0; x1 |] ->
                              ( decode_bytes_of_js
                                  (Obj.magic (x0 : 'a Js_of_ocaml.Js.t)),
                                decode_bytes_of_js
                                  (Obj.magic (x1 : 'a Js_of_ocaml.Js.t)) )
                          | xs ->
                              Format.kasprintf failwith
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (Array.length xs)

                        and cty_string_enum =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "ctyStringEnum"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_string jv with
                          | "a" -> `a
                          | "b" -> `b
                          | "c" -> `c
                          | s ->
                              Format.kasprintf failwith
                                "given string '%s' is not one of [ 'a', 'b', \
                                 'c' ]"
                                s

                        and td_ex_coretype_various_prim_types =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExCoretypeVariousPrimTypes"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_coretype in
                          ex_coretype_various_prim_types_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_coretype_with_int53p =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExCoretypeWithInt53p"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_coretype in
                          ex_coretype_with_int53p_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_coretype_various_complex_types =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExCoretypeVariousComplexTypes"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_coretype in
                          ex_coretype_various_complex_types_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_coretype_various_tuple_types =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExCoretypeVariousTupleTypes"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_coretype in
                          ex_coretype_various_tuple_types_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_coretype_named_json =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExCoretypeNamedJson"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_coretype in
                          ex_coretype_named_json_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_alias_unit =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExAliasUnit"
                          |> decode_unit_of_js

                        and td_ex_alias_int_opt =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExAliasIntOpt"
                          |> decode_option_of_js decode_int_of_js

                        and td_ex_alias_objtuple =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExAliasObjtuple"
                          |> fun jv ->
                          ( (let open Js_of_ocaml in
                             let x = Js.Unsafe.get jv "_0" in
                             Js.Optdef.case x
                               (fun () ->
                                 failwith "mandatory field '_0' does not exist")
                               decode_float_of_js),
                            let open Js_of_ocaml in
                            let x = Js.Unsafe.get jv "_1" in
                            Js.Optdef.case x
                              (fun () ->
                                failwith "mandatory field '_1' does not exist")
                              decode_string_of_js )

                        and td_ex_record_student =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExRecordStudent"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_record in
                          ex_record_student_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_record_teacher =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExRecordTeacher"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_record in
                          ex_record_teacher_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_variant_person =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExVariantPerson"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_variant in
                          ex_variant_person_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_variant_person_reused =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExVariantPersonReused"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_variant in
                          ex_variant_person_reused_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_variant_int_list =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExVariantIntList"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_variant in
                          ex_variant_int_list_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_variant_int_list_objtuple =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExVariantIntListObjtuple"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_variant in
                          ex_variant_int_list_objtuple_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_variant_foo =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExVariantFoo"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_variant in
                          ex_variant_foo_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_variant_customized_union =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExVariantCustomizedUnion"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_variant in
                          ex_variant_customized_union_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_mangling_student_inherited =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExManglingStudentInherited"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_mangling in
                          ex_mangling_student_inherited_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_mangling_person_no_mangling =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExManglingPersonNoMangling"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_mangling in
                          ex_mangling_person_no_mangling_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_mangling_person_inherited =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExManglingPersonInherited"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_mangling in
                          ex_mangling_person_inherited_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_mangling_enum =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExManglingEnum"
                          |> fun jv ->
                          match Js_of_ocaml.Js.to_string jv with
                          | "Case-at0" -> `Case_at0
                          | "case-at1" -> `case_at1
                          | "Case-at2" -> `Case_at2
                          | "Case-third" -> `Case_at3
                          | s ->
                              Format.kasprintf failwith
                                "given string '%s' is not one of [ 'Case-at0', \
                                 'case-at1', 'Case-at2', 'Case-third' ]"
                                s

                        and td_ex_optional_xy_opt =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExOptionalXyOpt"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_optional in
                          ex_optional_xy_opt_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_optional_variant =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExOptionalVariant"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_optional in
                          ex_optional_variant_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_ident_student_pair =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExIdentStudentPair"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_ident in
                          ex_ident_student_pair_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_nested_point2 =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExNestedPoint2"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_nested in
                          ex_nested_point2_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_nested_record =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExNestedRecord"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_nested in
                          ex_nested_record_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_nested_variant =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExNestedVariant"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_nested in
                          ex_nested_variant_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_nested_multiply_record =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExNestedMultiplyRecord"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_nested_multiply in
                          ex_nested_multiply_record_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_nested_multiply_variant =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExNestedMultiplyVariant"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_nested_multiply in
                          ex_nested_multiply_variant_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_version_substring_record_v3_2_1 =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExVersionSubstringRecordV3_2_1"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_version_substring in
                          ex_version_substring_record_v3_2_1_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result

                        and td_ex_version_substring_variant_v1_0 =
                          let open Js_of_ocaml in
                          Js.Unsafe.get
                            (Js.Unsafe.get
                               (Js.Unsafe.get peer_full_bridge "instance")
                               "endemicObjects")
                            "tdExVersionSubstringVariantV1_0"
                          |> fun x ->
                          (Kxclib_js.Json_ext.of_xjv x
                          |>
                          let open
                            Bindoj_test_common_typedesc_generated_examples
                            .Ex_version_substring in
                          ex_version_substring_variant_v1_0_of_json')
                          |> function
                          | Error e ->
                              failwith
                                (Bindoj_runtime.OfJsonResult.Err.to_string e)
                          | Ok result -> result
                      end
                    end : Bridge)
                  in
                  bridge_opt := Some concrete_bridge;
                  !continuations |> List.iter (fun f -> f concrete_bridge);
                  continuations := [])) );
       |])
    |> fun x ->
    (Obj.magic x : Bindoj_objintf_shared.endemic_full_bridge_reference)
end
