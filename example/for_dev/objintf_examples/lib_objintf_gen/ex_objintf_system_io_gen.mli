open Bindoj_objintf_gen_test_gen_utils [@@warning "-33"]

module Simple_interfaces : sig
  type nonrec byte_source' =
    ?max:int ->
    unit ->
    [ `data of Bytes.t * [ `eof | `maybe_more ] | `wait | `eof ]
  (** byte_source' bridgeable
      @param max max argument.
      @param arg0 argument at 0. *)

  (** Logger bridgeable. *)
  module type Logger = sig
    val info : string -> unit
    (** info method.
        @param arg0 argument at 0. *)

    val error : ?exn:string -> string -> unit
    (** error method.
        @param exn exn argument.
        @param arg0 argument at 0. *)
  end

  (** byte_source bridgeable. *)
  class type byte_source = object
    method bytes_left : unit -> int
    (** bytes_left method.
        @param arg0 argument at 0. *)

    method next_block : ?max:int -> unit -> Bytes.t
    (** next_block method.
        @param max max argument.
        @param arg0 argument at 0. *)
  end
end

open Simple_interfaces [@@warning "-33"]

module type Endemic_object_registry_interface = sig
  val register_logger : id:string -> (module Logger) option -> unit
  (** logger registry *)
end

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
    (** output_channel bridgeable. *)
    class type output_channel = object
      method channel_name : string
      (** channel_name method. *)

      method write : Bytes.t -> unit
      (** write method. *)

      method write_bulk : byte_source endemic -> unit
      (** write_bulk method.
          @param arg0 argument at 0. *)

      method write_async : byte_source' endemic -> unit
      (** write_async method.
          @param arg0 argument at 0. *)
    end

    (** System_io bridgeable *)
    module type System_io = sig
      val stdout : unit -> output_channel peer
      (** stdout method *)

      val stderr : unit -> output_channel peer
      (** stderr method *)

      val open_file_wo : path:string -> output_channel peer
      (** open_file_wo method
          @param path path argument *)

      val open_file_ro : path:string -> byte_source peer
      (** open_file_ro method
          @param path path argument *)
    end
  end

  module Interfaces : sig
    module Simple_interfaces = Simple_interfaces
    module Complex_interfaces = Complex_interfaces
    include module type of Simple_interfaces
    include module type of Complex_interfaces
  end
end

(** Ex_objintf_system_io objintf *)
module type Concrete_bridge = sig
  include module type of Concrete_bridge_interfaces
  open Interfaces [@@warning "-33"]

  module Peer_object_registry : sig
    val lookup_logger :
      name:string ->
      variant:[ `persistent | `transient ] ->
      (module Logger) peer option
    (** logger registry *)
  end

  module Peer_objects : sig
    val system_io : (module System_io) peer
    (** system_io object *)
  end

  module Endemic_object_registry : Endemic_object_registry_interface
end

open Concrete_bridge_interfaces.Interfaces [@@warning "-33"]

module type Dual_setup_full_bridge = functor
  (M : sig
     val my_logger : (module Logger)
     (** my_logger object *)

     val initially_registry_objects :
       (module Endemic_object_registry_interface) -> unit
   end)
  -> sig
  module type Bridge = Concrete_bridge

  val get_bridge :
    unit -> [ `await_peer_setup | `bridge of (module Concrete_bridge) ]

  val get_bridge_async : ((module Concrete_bridge) -> unit) -> unit
  val endemic_full_bridge : Bindoj_objintf_shared.endemic_full_bridge_reference
end
