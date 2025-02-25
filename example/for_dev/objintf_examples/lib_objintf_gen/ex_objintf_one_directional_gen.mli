open Bindoj_objintf_gen_test_gen_utils [@@warning "-33"]

type ex_record_student = { admission_year : int; name : string }

val ex_record_student_reflect : ex_record_student Bindoj_runtime.Refl.t

val ex_record_student_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_record_student_to_json : ex_record_student -> Kxclib.Json.jv

val ex_record_student_of_json' :
  ex_record_student Bindoj_runtime.json_full_decoder

val ex_record_student_of_json : Kxclib.Json.jv -> ex_record_student option

module Simple_interfaces : sig
  type nonrec hello = string -> unit
  type nonrec unit_sole = string -> unit -> unit -> string

  class type unit_obj = object
    method name : string
    method unit_01 : string -> unit -> string
    method unit_02 : string -> unit -> unit -> string
    method unit_03 : string -> unit -> unit -> unit -> string
  end

  module type unit_mod = sig
    val name : unit -> string
    val unit_01 : string -> unit -> string
    val unit_02 : string -> unit -> unit -> string
    val unit_03 : string -> unit -> unit -> unit -> string
  end

  module type with_default_value = sig
    val get_default_string : ?str:string -> string
    val get_default_student : ?student:ex_record_student -> string
  end
end

open Simple_interfaces [@@warning "-33"]

module Concrete_bridge_interfaces : sig
  open Bindoj_objintf_shared

  (** marker type for this specific concrete bridge *)
  type nonrec br = Br

  type 'x peer = ('x, br) peer'
  type 'x endemic = ('x, br) endemic'

  val access : 'x peer -> 'x
  val bridge : 'x -> 'x endemic

  module Simple_interfaces = Simple_interfaces

  module Complex_interfaces : sig
    type nonrec sole_var = string

    class type rec_obj = object
      method name : string
      method get_self : rec_obj peer
    end
  end

  module Interfaces : sig
    module Simple_interfaces = Simple_interfaces
    module Complex_interfaces = Complex_interfaces
    include module type of Simple_interfaces
    include module type of Complex_interfaces
  end
end

module type Concrete_bridge = sig
  include module type of Concrete_bridge_interfaces
  open Interfaces [@@warning "-33"]

  module Peer_object_registry : sig
    val lookup_string : id0:string -> id1:Kxclib.int53p -> string option
    val lookup_hello : id:string -> hello peer option
  end

  module Peer_objects : sig
    val my_string : string
    val my_hello : hello peer
    val my_sole_var : sole_var peer
    val my_unit_sole : unit_sole peer
    val my_unit_obj : unit_obj peer
    val my_unit_mod : (module unit_mod) peer
    val my_rec_obj : rec_obj peer
    val my_non_json_values : Utils.non_json_values
    val with_default_value : (module with_default_value) peer
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
