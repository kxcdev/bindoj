open Bindoj_objintf_gen_jsoo_test_gen_utils [@@warning "-33"]
module Simple_interfaces : sig end
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
  module Complex_interfaces : sig end

  module Interfaces : sig
    module Simple_interfaces = Simple_interfaces
    module Complex_interfaces = Complex_interfaces
    include module type of Simple_interfaces
    include module type of Complex_interfaces
  end
end

module type Concrete_bridge = sig
  include module type of Concrete_bridge_interfaces
end

open Concrete_bridge_interfaces.Interfaces [@@warning "-33"]

module type Setup_less_full_bridge = Concrete_bridge

module Full_bridge_with_jsoo : Setup_less_full_bridge
