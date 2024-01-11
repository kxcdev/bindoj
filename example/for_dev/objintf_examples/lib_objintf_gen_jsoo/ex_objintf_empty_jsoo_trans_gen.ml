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

module type Setup_less_full_bridge = Concrete_bridge

module Full_bridge_with_jsoo : Setup_less_full_bridge = struct
  include Concrete_bridge_interfaces
end
