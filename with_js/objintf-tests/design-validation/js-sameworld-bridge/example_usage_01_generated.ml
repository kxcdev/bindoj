open Bindoj_objintf_shared

module Simple_interfaces = struct

  (* generated interface types for "simple" bridgeables *)

  (* sole method bridgeable *)
  type byte_source' =
    (?max:int
          -> unit
          -> [ `data of bytes * [ `eof | `maybe_more ]
            | `wait | `eof ]
        )

  (* method bundle bridgeable - first-class module style (ocaml-only config) *)
  module type Logger = sig
    val info : string -> unit
    val error : ?exn:string -> string -> unit
  end

  (* method bundle bridgeable - object style (ocaml-only config) *)
  type byte_source =
    < bytes_left : int;
      next_block : ?max:int -> unit -> bytes;
    > (* type byte_source *)
end
open Simple_interfaces

module type Initial_registry_object_registers = sig
    val register_logger :
      id:string
      -> (module Logger) option
      -> unit
end

module type Concrete_bridge = sig
  type br (** opaque bridge marker type for this specific bridge *)

  (* aliases for convenience *)
  type 'x peer = ('x, br) peer'
  type 'x endemic = ('x, br) endemic'

  (* wrapper & unwrappers for bridgeables *)
  val access : 'intf peer -> 'intf
  val bridge : 'intf -> 'intf endemic

  module Simple_interfaces = Simple_interfaces
  module Complex_interfaces : sig

    (* generated interface types for "complex" bridgeables *)

    type output_channel =
      < channel_name : string;
        write : bytes -> unit;
        write_bulk : byte_source endemic -> unit;
        write_async : byte_source' endemic -> unit;
      > (* type output_channel *)

    module type System_io = sig
      val stdout : output_channel peer
      val stderr : output_channel peer
      val open_file_wo : path:string -> output_channel peer
      val open_file_ro : path:string -> byte_source peer
    end (* module type System_io *)
  end

  module Interfaces : sig
    module Simple_interfaces = Simple_interfaces
    module Complex_interfaces = Complex_interfaces
    include module type of Simple_interfaces (* also: type equality *)
    include module type of Complex_interfaces (* also: type equality *)
  end

  open Interfaces

  module Peer_object_registry : sig
    val lookup_logger :
      name:string
      -> variant:[ `persistent | `transient ]
      -> (module Logger) peer option
  end

  (* named peer objects *)
  module Peer_objects : sig
    val system_io : (module System_io) peer
  end

  module Endemic_object_registry : Initial_registry_object_registers
end


(* there are four modes of setup
    - endemic setup action only
    - peer setup action only
    - dual (both peer & endemic) setup action necessary
    - setup-less

    for setup modes that require setup action,
    the action can be generative / applicative and singleton / multi-instance.
    we only consider singleton generative ones for now.
  *)

(* setup-less bridge can be used right away *)
module type Setup_less_full_bridge = Concrete_bridge

(* endemic setup action only *)
module type Endemic_setup_only_full_bridge =
  functor (_ : sig
    val my_logger : (module Logger)
    val initially_registry_objects : (module Initial_registry_object_registers) -> unit
  end)
  -> sig
    include Concrete_bridge
    val endemic_full_bridge : endemic_full_bridge_reference
  end

(* peer setup action only *)
module type Peer_setup_only_full_bridge = sig
  module type Bridge = Concrete_bridge

  val get_bridge : unit -> [ `await_peer_setup | `bridge of (module Concrete_bridge) ]

  (* register the callback so when peer setup is performed the callback is called *)
  val get_bridge_async : ((module Concrete_bridge) -> unit) -> unit

  val endemic_full_bridge : endemic_full_bridge_reference
end

(* dual setup action *)
module type Dual_setup_full_bridge =
  functor (_ : sig
    val my_logger : (module Logger)
    val initially_registry_objects : (module Initial_registry_object_registers) -> unit
  end) -> sig
    module type Bridge = Concrete_bridge
    val endemic_full_bridge : endemic_full_bridge_reference
    val get_bridge : unit -> [ `await_peer_setup | `bridge of (module Concrete_bridge) ]
    val get_bridge_async : ((module Concrete_bridge) -> unit) -> unit
  end

module Full_bridge_with_jsoo : Dual_setup_full_bridge =
  functor(M : sig
      val my_logger : (module Logger)
      val initially_registry_objects : (module Initial_registry_object_registers) -> unit
  end) -> struct
    module type Bridge = Concrete_bridge
    let ref_setup_called = ref false
    let bridge: (module Bridge) option ref = ref none
    let continuations = ref []

    let get_bridge () =
      match !bridge with
      | None -> `await_peer_setup
      | Some b -> `bridge b

    let get_bridge_async f =
      match !bridge with
      | None -> refappend continuations f
      | Some b -> f b

    type br = Br

    let endemic_full_bridge : endemic_full_bridge_reference =
      (object%js
        method setupCalled = !ref_setup_called
        method setup (peer_full_bridge) =
          if !ref_setup_called then
            raise Full_Bridge_already_setup;
          refset ref_setup_called true;

          let open Js_of_ocaml.Js in

          let encode_bytes =
            let encoder = Unsafe.new_obj (Unsafe.pure_js_expr "TextEncoder") [||] in
            fun bytes ->
              Unsafe.meth_call encoder "encode" [| Bytes.to_string bytes |> string |> Unsafe.inject |]
          in

          let decode_bytes =
            let decoder = Unsafe.new_obj (Unsafe.pure_js_expr "TextDecoder") [||] in
            fun bytes ->
              Unsafe.meth_call decoder "decode" [| bytes |]
              |> to_string
              |> Bytes.of_string
          in

          let undefined_to_opt: 'x. 'x -> 'x option = function
            | x when Obj.magic x == undefined -> None
            | x -> Some x
          in

          let rec js_of_Logger: (module Logger) -> _ =
            fun (module M : Logger) ->
            object%js
              method info message = M.info (to_string message)
              method error message labeledArgs =
                let exn =
                  Optdef.(
                    bind labeledArgs (fun la ->
                      map (Unsafe.get la "exn") to_string
                    ) |> to_option)
                in
                M.error
                  ?exn
                  (message |> Obj.magic |> Prr.Jstr.to_string)
            end
            [@@warning "-39"]
          
          and js_to_Logger: _ -> (module Logger) =
            fun js_obj ->
            (module struct
              let info message = Unsafe.meth_call js_obj "info" [| Unsafe.inject (string message) |]
              let error ?exn message =
                Unsafe.meth_call js_obj "error" [|
                  Unsafe.inject (string message);
                  Unsafe.inject (object%js
                    val exn = match exn with
                      | None -> Unsafe.inject undefined
                      | Some e -> Unsafe.inject (string e)
                  end)
                |]
            end : Logger)
            [@@warning "-39"]
          and js_to_byte_source =
            fun js_obj -> object
              method bytes_left = Unsafe.meth_call js_obj "bytesLeft" [||]
              method next_block ?max () =
                decode_bytes (Unsafe.meth_call js_obj "nextBlock" [|
                  Unsafe.inject (object%js
                    val max = match max with
                      | None -> Unsafe.inject undefined
                      | Some m -> Unsafe.inject m
                  end)
                |])
            end
            [@@warning "-26-39"]
          and js_of_byte_source' src =
            Unsafe.callback (fun labeledArgs ->
              let max =
                undefined_to_opt labeledArgs
                |> Fn.flip Option.bind (Fn.flip Unsafe.get "max")
                |> Fn.flip Option.bind undefined_to_opt
              in
              match src ?max () with
              | `data (bytes, e) ->
                Unsafe.inject (object%js
                  val kind = string "data"
                  val value = array [|
                      (* Direct encoding/decoding between type_decl and js objects is not currently implemented, so pass values in json compatible format. *)
                      (* encode_bytes bytes; *)
                      Kxclib.Base64.encode bytes |> string;
                      string (match e with `eof -> "eof" | `maybe_more -> "maybe-more")
                    |]
                end)
              | `wait -> Unsafe.inject (object%js val kind = string "wait" end)
              | `eof -> Unsafe.inject (object%js val kind = string "eof" end)
            )
          in

          if not (Unsafe.get peer_full_bridge "setupCalled") then
            Unsafe.meth_call peer_full_bridge "setup" [|
              Unsafe.inject (object%js
                method setup_called_js = true
              end)
            |];

          Unsafe.set (Unsafe.get peer_full_bridge "instance") "peerObjects" (object%js
            val myLogger = js_of_Logger M.my_logger
          end);

          let concrete_bridge =
            (module struct
              type nonrec br = br
    
              type 'x peer = ('x, br) peer'
              type 'x endemic = ('x, br) endemic'
    
              let access x = access x
              let bridge x = bridge_generic ~bridge:Br x
    
              module Simple_interfaces = Simple_interfaces
              module Complex_interfaces = struct
                type output_channel =
                  < channel_name : string;
                    write : bytes -> unit;
                    write_bulk : byte_source endemic -> unit;
                    write_async : byte_source' endemic -> unit;
                  >
    
                module type System_io = sig
                  val stdout : output_channel peer
                  val stderr : output_channel peer
                  val open_file_wo : path:string -> output_channel peer
                  val open_file_ro : path:string -> byte_source peer
                end
              end
    
              module Interfaces = struct
                module Simple_interfaces = Simple_interfaces
                module Complex_interfaces = Complex_interfaces
                include Simple_interfaces
                include Complex_interfaces
              end
    
              open Interfaces
    
              let unwrap_endemic (Endemic_object { bridge = Br; underlying }) = underlying
              let wrap_peer access raw_object = Peer_object { bridge = Br; access; raw_object }
    
              let rec js_to_output_channel js_obj =
                (object
                  method channel_name =
                    Unsafe.meth_call js_obj "channelName" [| |] |> to_string
                  method write bytes =
                    Unsafe.meth_call js_obj "write" [| encode_bytes bytes |]
                  method write_bulk (src: byte_source endemic) =
                    Unsafe.meth_call js_obj "writeBulk" [|
                      Unsafe.inject (object%js
                        method bytesLeft = (unwrap_endemic src)#bytes_left
                        method nextBlock labeledArgs =
                          let max =
                            undefined_to_opt labeledArgs
                            |> Fn.flip Option.bind (Fn.flip Unsafe.get "max")
                            |> Fn.flip Option.bind undefined_to_opt
                          in
                          (unwrap_endemic src)#next_block ?max () |> encode_bytes
                      end)
                    |]
                  method write_async (src: byte_source' endemic) =
                    Unsafe.meth_call js_obj "writeAsync" [|
                      Unsafe.inject & js_of_byte_source' (unwrap_endemic src)
                    |]
                end : output_channel)
              [@@warning "-39"]
              and js_to_System_io =
                fun js_obj ->
                      (module struct
                        let stdout =
                          Unsafe.get js_obj "stdout"
                          |> wrap_peer js_to_output_channel
                        let stderr =
                          Unsafe.get js_obj "stderr"
                          |> wrap_peer js_to_output_channel
                        let open_file_wo ~path =
                          Unsafe.meth_call js_obj "openFileWo" [|
                            Unsafe.inject (object%js
                              val path = string path
                            end)
                          |]
                          |> wrap_peer js_to_output_channel
                        let open_file_ro ~path =
                          Unsafe.meth_call js_obj "openFileRo" [|
                            Unsafe.inject (object%js
                              val path = string path
                            end)
                          |]
                          |> wrap_peer js_to_byte_source
                      end : System_io)
              [@@warning "-39"]

              module Peer_object_registry = struct
                open Bindoj_runtime
                let registry_of_logger = ref StringMap.empty
                let lookup_logger ~name  ~variant  =
                  !registry_of_logger
                  |> StringMap.find_opt
                    Map_key.(String.concat "" [
                      encode_map_key
                        ~check_type:`string
                        (Mk_string name);
                      encode_map_key
                        ~check_type:(`StringEnum ["persistent"; "transient"])
                        (Mk_string_enum
                          (match variant with
                            | `persistent -> "persistent"
                            | `transient -> "transient"))
                    ])
                and register_logger ~name  ~variant  value =
                  registry_of_logger :=
                    !registry_of_logger
                    |> StringMap.update
                      Map_key.(
                        String.concat "" [
                          encode_map_key
                            ~check_type:`string
                            (Mk_string name);
                          encode_map_key
                            ~check_type:(`StringEnum ["persistent"; "transient"])
                            (Mk_string_enum
                              (match variant with
                              | `persistent -> "persistent"
                              | `transient -> "transient"))
                      ])
                      (fun _ -> value)
                
                let () =
                  let js_registry =
                    peer_full_bridge
                    |> Fn.flip Unsafe.get "instance"
                    |> Fn.flip Unsafe.get "endemicObjectRegistry" in
                  Unsafe.get js_registry "logger"
                  |> Fn.flip Unsafe.get "initialObjects"
                  |> to_array
                  |> Array.iter (fun x ->
                    let coordinate = Unsafe.get x "coordinate" in
                    match to_string (Unsafe.get x "kind") with
                    | "register" ->
                        register_logger
                          ~name:(to_string (Unsafe.get coordinate "name"))
                          ~variant:(
                                      match to_string (Unsafe.get coordinate "variant") with
                                      | "persistent" -> `persistent
                                      | "transient" -> `transient
                                      | s -> failwith' "given string '%s' is not one of [ 'persistent', 'transient' ]" s)
                          (Some (wrap_peer js_to_Logger (Unsafe.get x "value")))
                    | "deregister" ->
                        register_logger
                          ~name:(to_string (Unsafe.get coordinate "name"))
                          ~variant:((fun jv ->
                                        match to_string jv
                                        with
                                        | "persistent" -> `persistent
                                        | "transient" -> `transient
                                        | s -> failwith' "given string '%s' is not one of [ 'persistent', 'transient' ]" s)
                                      (Unsafe.get coordinate "variant"))
                          None
                    | kind -> failwith ("unexpected kind: " ^ kind));
                  ();
                  Unsafe.set (Unsafe.get peer_full_bridge "instance") "endemicObjectRegistry" (object%js
                    val logger_js = (object%js
                      method register coordinate value =
                        register_logger
                          ~name:(to_string (Unsafe.get coordinate "name"))
                          ~variant:(
                            match to_string (Unsafe.get coordinate "variant") with
                            | "persistent" -> `persistent
                            | "transient" -> `transient
                            | s -> failwith' "given string '%s' is not one of [ 'persistent', 'transient' ]" s)
                          (Some (wrap_peer js_to_Logger value))
                      method deregister coordinate =
                        register_logger
                          ~name:(to_string (Unsafe.get coordinate "name"))
                          ~variant:(
                            match to_string (Unsafe.get coordinate "variant") with
                            | "persistent" -> `persistent
                            | "transient" -> `transient
                            | s -> failwith' "given string '%s' is not one of [ 'persistent', 'transient' ]" s)
                          None
                    end)
                  end)
              end
    
              module Peer_objects = struct
                let system_io : (module System_io) peer =
                  (Unsafe.get peer_full_bridge "instance")
                  |> Fn.flip Unsafe.get "endemicObjects"
                  |> Fn.flip Unsafe.get "systemIo"
                  |> wrap_peer js_to_System_io
              end
    
              module Endemic_object_registry : Initial_registry_object_registers = struct
                open Bindoj_runtime
                let registry_of_logger : (module Logger) StringMap.t ref = ref StringMap.empty
    
                let lookup_logger ~id  =
                  !registry_of_logger
                  |> StringMap.find_opt Map_key.(encode_map_key ~check_type:`string (Mk_string id))
                and register_logger  ~id logger =
                  registry_of_logger :=
                  !registry_of_logger
                  |> StringMap.update
                    Map_key.(encode_map_key ~check_type:`string (Mk_string id))
                    (constant logger)
    
                let () =
                  (* Override peerObjectRegistry of ConcreteBridge in JS. *)
                  Unsafe.set (Unsafe.get peer_full_bridge "instance") "peerObjectRegistry" (object%js
                    method lookupLogger coordinate =
                      lookup_logger ~id:(to_string (Unsafe.get coordinate "id"))
                      |> function
                      | None -> Unsafe.inject null
                      | Some logger -> Unsafe.inject (js_of_Logger logger)
                  end)
              end
            end : Bridge)
          in
          concrete_bridge |> fun (module B: Bridge) ->
            M.initially_registry_objects((module B.Endemic_object_registry));
          refset bridge & Some concrete_bridge;
          !continuations |!> (fun f -> f concrete_bridge);
          continuations := []
      end)
      |> Obj.magic
  end
