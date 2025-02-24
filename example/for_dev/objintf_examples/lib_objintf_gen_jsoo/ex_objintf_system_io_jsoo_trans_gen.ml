open Bindoj_objintf_gen_jsoo_test_gen_utils [@@warning "-33"]

module Simple_interfaces = struct
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
  val register_logger :
    name:string ->
    variant:[ `persistent | `transient ] ->
    (module Logger) option ->
    unit
  (** logger registry *)
end

module Concrete_bridge_interfaces = struct
  open Bindoj_objintf_shared

  (** marker type for this specific concrete bridge *)
  type nonrec br = Br

  type 'x peer = ('x, br) peer'
  type 'x endemic = ('x, br) endemic'

  let access : 'x peer -> 'x = access
  let bridge x = bridge_generic ~bridge:Br x

  module Simple_interfaces = Simple_interfaces

  module Complex_interfaces = struct
    (** output_channel bridgeable. *)
    class type output_channel = object
      method channel_name : string
      (** channel_name method. *)

      method write : Bytes.t -> unit
      (** write method. *)

      method write_bulk : byte_source peer -> unit
      (** write_bulk method.
          @param arg0 argument at 0. *)

      method write_async : byte_source' peer -> unit
      (** write_async method.
          @param arg0 argument at 0. *)
    end

    (** System_io bridgeable *)
    module type System_io = sig
      val stdout : unit -> output_channel endemic
      (** stdout method *)

      val stderr : unit -> output_channel endemic
      (** stderr method *)

      val open_file_wo : path:string -> output_channel endemic
      (** open_file_wo method
          @param path path argument *)

      val open_file_ro : path:string -> byte_source endemic
      (** open_file_ro method
          @param path path argument *)
    end
  end

  module Interfaces = struct
    module Simple_interfaces = Simple_interfaces
    module Complex_interfaces = Complex_interfaces
    include Simple_interfaces
    include Complex_interfaces
  end
end

(** Ex_objintf_system_io objintf *)
module type Concrete_bridge = sig
  include module type of Concrete_bridge_interfaces
  open Interfaces

  module Peer_object_registry : sig
    val lookup_logger : id:string -> (module Logger) peer option
    (** logger registry *)
  end

  module Peer_objects : sig
    val my_logger : (module Logger) peer
    (** my_logger object *)
  end

  module Endemic_object_registry : Endemic_object_registry_interface
end

open Concrete_bridge_interfaces.Interfaces [@@warning "-33"]

module type Dual_setup_full_bridge = functor
  (M : sig
     val system_io : (module System_io)
     (** system_io object *)

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

module Full_bridge_with_jsoo : Dual_setup_full_bridge =
functor
  (M : sig
     val system_io : (module System_io)
     (** system_io object *)

     val initially_registry_objects :
       (module Endemic_object_registry_interface) -> unit
   end)
  ->
  struct
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

                        let encode_unit_to_js () = Js_of_ocaml.Js.null
                        [@@warning "-32"]

                        and encode_string_to_js = Js_of_ocaml.Js.string
                        [@@warning "-32"]

                        and encode_int_to_js = float_of_int [@@warning "-32"]

                        and encode_bytes_to_js =
                          let open Js_of_ocaml in
                          let encoder =
                            Js.Unsafe.new_obj
                              (Js.Unsafe.pure_js_expr "TextEncoder")
                              [||]
                          in
                          fun bytes ->
                            Js.Unsafe.meth_call encoder "encode"
                              [|
                                Js.Unsafe.inject
                                  (Js.string (Bytes.to_string bytes));
                              |]
                        [@@warning "-32"]

                        and decode_unit_of_js _ = () [@@warning "-32"]

                        and decode_string_of_js = Js_of_ocaml.Js.to_string
                        [@@warning "-32"]

                        and decode_int_of_js jv =
                          if Float.is_integer jv then int_of_float jv
                          else
                            Format.kasprintf failwith
                              "expecting an integer but the given is '%f'" jv
                        [@@warning "-32"]

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

                        let rec byte_source_state_of_json' =
                          (fun ?(path = []) ->
                             fun x ->
                              (let rec of_json_impl =
                                 let bytes_of_json' path = function
                                   | (`str x : Kxclib.Json.jv) -> (
                                       try Ok (Kxclib.Base64.decode x)
                                       with Invalid_argument msg ->
                                         Error (msg, path))
                                   | jv ->
                                       Error
                                         ( Printf.sprintf
                                             "expecting type 'bytes' but the \
                                              given is of type '%s'"
                                             (let open Kxclib.Json in
                                              string_of_jv_kind (classify_jv jv)),
                                           path )
                                 in
                                 fun path ->
                                   fun __bindoj_orig ->
                                    match
                                      Kxclib.Jv.pump_field "kind" __bindoj_orig
                                    with
                                    | `obj (("kind", `str "data") :: param) -> (
                                        match List.assoc_opt "value" param with
                                        | Some (`arr [ x0; x1 ]) ->
                                            let ( >>= ) = Result.bind in
                                            bytes_of_json'
                                              (`i 0 :: `f "value" :: path)
                                              x0
                                            >>= fun x0 ->
                                            (fun path -> function
                                              | `str s ->
                                                  (function
                                                    | "eof" -> Ok `eof
                                                    | "maybe-more" ->
                                                        Ok `maybe_more
                                                    | s ->
                                                        Error
                                                          ( Printf.sprintf
                                                              "given string \
                                                               '%s' is not one \
                                                               of [ 'eof', \
                                                               'maybe-more' ]"
                                                              s,
                                                            path ))
                                                    s
                                              | jv ->
                                                  Error
                                                    ( Printf.sprintf
                                                        "expecting type \
                                                         'string' but the \
                                                         given is of type '%s'"
                                                        (let open Kxclib.Json in
                                                         string_of_jv_kind
                                                           (classify_jv jv)),
                                                      path ))
                                              (`i 1 :: `f "value" :: path)
                                              x1
                                            >>= fun x1 -> Ok (`data (x0, x1))
                                        | Some (`arr xs) ->
                                            Error
                                              ( Printf.sprintf
                                                  "expecting an array of \
                                                   length 2, but the given has \
                                                   a length of %d"
                                                  (List.length xs),
                                                `f "value" :: path )
                                        | Some jv ->
                                            Error
                                              ( Printf.sprintf
                                                  "an array is expected for a \
                                                   tuple value, but the given \
                                                   is of type '%s'"
                                                  (let open Kxclib.Json in
                                                   string_of_jv_kind
                                                     (classify_jv jv)),
                                                `f "value" :: path )
                                        | None ->
                                            Error
                                              ( "mandatory field 'value' does \
                                                 not exist",
                                                path ))
                                    | `obj (("kind", `str "wait") :: _) ->
                                        Ok `wait
                                    | `obj (("kind", `str "eof") :: _) ->
                                        Ok `eof
                                    | `obj
                                        (("kind", `str discriminator_value) :: _)
                                      ->
                                        Error
                                          ( Printf.sprintf
                                              "given discriminator field value \
                                               '%s' is not one of [ 'data', \
                                               'wait', 'eof' ]"
                                              discriminator_value,
                                            `f "kind" :: path )
                                    | `obj (("kind", jv) :: _) ->
                                        Error
                                          ( Printf.sprintf
                                              "a string is expected for a \
                                               variant discriminator, but the \
                                               given is of type '%s'"
                                              (let open Kxclib.Json in
                                               string_of_jv_kind
                                                 (classify_jv jv)),
                                            `f "kind" :: path )
                                    | `obj _ ->
                                        Error
                                          ( "discriminator field 'kind' does \
                                             not exist",
                                            path )
                                    | jv ->
                                        Error
                                          ( Printf.sprintf
                                              "an object is expected for a \
                                               variant value, but the given is \
                                               of type '%s'"
                                              (let open Kxclib.Json in
                                               string_of_jv_kind
                                                 (classify_jv jv)),
                                            path )
                               in
                               of_json_impl)
                                path x
                              |> Result.map_error (fun (msg, path) ->
                                     ( msg,
                                       path,
                                       `with_warning
                                         ( "not considering any config if exists",
                                           `named
                                             ( "ByteSourceState",
                                               `anyone_of
                                                 [
                                                   `object_of
                                                     [
                                                       `mandatory_field
                                                         ( "kind",
                                                           `exactly
                                                             (`str "data") );
                                                       `mandatory_field
                                                         ( "value",
                                                           `tuple_of
                                                             [
                                                               `base64str;
                                                               `string_enum
                                                                 [
                                                                   "eof";
                                                                   "maybe-more";
                                                                 ];
                                                             ] );
                                                     ];
                                                   `object_of
                                                     [
                                                       `mandatory_field
                                                         ( "kind",
                                                           `exactly
                                                             (`str "wait") );
                                                     ];
                                                   `object_of
                                                     [
                                                       `mandatory_field
                                                         ( "kind",
                                                           `exactly (`str "eof")
                                                         );
                                                     ];
                                                 ] ) ) ))
                            : [ `data of Bytes.t * [ `eof | `maybe_more ]
                              | `wait
                              | `eof ]
                              Bindoj_runtime.json_full_decoder)
                        [@@warning "-39"]

                        let rec decode_byte_source'_of_js __js_obj ?max () =
                          let open Js_of_ocaml in
                          (fun x ->
                            Kxclib_js.Json_ext.of_xjv x
                            |> byte_source_state_of_json'
                            |> function
                            | Error e ->
                                failwith
                                  (Bindoj_runtime.OfJsonResult.Err.to_string e)
                            | Ok result -> result)
                            (Js.Unsafe.fun_call __js_obj
                               [|
                                 Js.Unsafe.inject
                                   (let open Js_of_ocaml.Js in
                                    Unsafe.obj
                                      [|
                                        ( "max",
                                          Unsafe.inject
                                            (let open Js.Opt in
                                             map (option max) encode_int_to_js)
                                        );
                                      |]);
                               |])
                        [@@warning "-39"]

                        and decode_byte_source_of_js __js_obj =
                          object
                            method bytes_left () =
                              let open Js_of_ocaml in
                              decode_int_of_js
                                (Js.Unsafe.meth_call __js_obj "bytesLeft" [||])

                            method next_block ?max () =
                              let open Js_of_ocaml in
                              decode_bytes_of_js
                                (Js.Unsafe.meth_call __js_obj "nextBlock"
                                   [|
                                     Js.Unsafe.inject
                                       (let open Js_of_ocaml.Js in
                                        Unsafe.obj
                                          [|
                                            ( "max",
                                              Unsafe.inject
                                                (let open Js.Opt in
                                                 map (option max)
                                                   encode_int_to_js) );
                                          |]);
                                   |])
                          end
                        [@@warning "-39"]

                        and encode_byte_source_to_js (__caml_obj : byte_source)
                            =
                          let open Js_of_ocaml.Js in
                          Unsafe.obj
                            [|
                              ( "bytesLeft",
                                Unsafe.inject
                                  (Unsafe.callback (fun () ->
                                       encode_int_to_js
                                         (__caml_obj#bytes_left ()))) );
                              ( "nextBlock",
                                Unsafe.inject
                                  (Unsafe.callback (fun labeledArgs ->
                                       encode_bytes_to_js
                                         (__caml_obj#next_block
                                            ?max:
                                              (let open Js_of_ocaml.Js in
                                               Optdef.bind labeledArgs
                                                 (fun la ->
                                                   Optdef.map
                                                     (Unsafe.get la "max")
                                                     decode_int_of_js)
                                               |> Optdef.to_option)
                                            ()))) );
                            |]
                        [@@warning "-39"]

                        and encode_Logger_to_js (module M : Logger) =
                          let open Js_of_ocaml.Js in
                          Unsafe.obj
                            [|
                              ( "info",
                                Unsafe.inject
                                  (Unsafe.callback (fun __arg0 ->
                                       encode_unit_to_js
                                         (M.info (decode_string_of_js __arg0))))
                              );
                              ( "error",
                                Unsafe.inject
                                  (Unsafe.callback_with_arity 2 (fun __arg0 ->
                                       fun labeledArgs ->
                                        encode_unit_to_js
                                          (M.error
                                             ?exn:
                                               (let open Js_of_ocaml.Js in
                                                Optdef.bind labeledArgs
                                                  (fun la ->
                                                    Optdef.map
                                                      (Unsafe.get la "exn")
                                                      decode_string_of_js)
                                                |> Optdef.to_option)
                                             (decode_string_of_js __arg0)))) );
                            |]
                        [@@warning "-39"]

                        and decode_Logger_of_js __js_obj =
                          (module struct
                            let info __arg0 =
                              let open Js_of_ocaml in
                              decode_unit_of_js
                                (Js.Unsafe.meth_call __js_obj "info"
                                   [|
                                     Js.Unsafe.inject
                                       (encode_string_to_js __arg0);
                                   |])

                            and error ?exn __arg0 =
                              let open Js_of_ocaml in
                              decode_unit_of_js
                                (Js.Unsafe.meth_call __js_obj "error"
                                   [|
                                     Js.Unsafe.inject
                                       (encode_string_to_js __arg0);
                                     Js.Unsafe.inject
                                       (let open Js_of_ocaml.Js in
                                        Unsafe.obj
                                          [|
                                            ( "exn",
                                              Unsafe.inject
                                                (let open Js.Opt in
                                                 map (option exn)
                                                   encode_string_to_js) );
                                          |]);
                                   |])
                          end : Logger)
                        [@@warning "-39"]

                        let rec encode_output_channel_to_js
                            (__caml_obj : output_channel) =
                          let open Js_of_ocaml.Js in
                          Unsafe.obj
                            [|
                              ( "channelName",
                                Unsafe.inject
                                  (Unsafe.callback (fun () ->
                                       encode_string_to_js
                                         __caml_obj#channel_name)) );
                              ( "write",
                                Unsafe.inject
                                  (Unsafe.callback (fun __arg0 ->
                                       encode_unit_to_js
                                         (__caml_obj#write
                                            (decode_bytes_of_js __arg0)))) );
                              ( "writeBulk",
                                Unsafe.inject
                                  (Unsafe.callback (fun __arg0 ->
                                       encode_unit_to_js
                                         (__caml_obj#write_bulk
                                            ((wrap_peer decode_byte_source_of_js)
                                               __arg0)))) );
                              ( "writeAsync",
                                Unsafe.inject
                                  (Unsafe.callback (fun __arg0 ->
                                       encode_unit_to_js
                                         (__caml_obj#write_async
                                            ((wrap_peer
                                                decode_byte_source'_of_js)
                                               __arg0)))) );
                            |]
                        [@@warning "-39"]

                        and encode_System_io_to_js (module M : System_io) =
                          let open Js_of_ocaml.Js in
                          Unsafe.obj
                            [|
                              ( "stdout",
                                Unsafe.inject
                                  (Unsafe.callback (fun () ->
                                       (fun x ->
                                         encode_output_channel_to_js
                                           (unwrap_endemic x))
                                         (M.stdout ()))) );
                              ( "stderr",
                                Unsafe.inject
                                  (Unsafe.callback (fun () ->
                                       (fun x ->
                                         encode_output_channel_to_js
                                           (unwrap_endemic x))
                                         (M.stderr ()))) );
                              ( "openFileWo",
                                Unsafe.inject
                                  (Unsafe.callback (fun labeledArgs ->
                                       (fun x ->
                                         encode_output_channel_to_js
                                           (unwrap_endemic x))
                                         (M.open_file_wo
                                            ~path:
                                              (let open Js_of_ocaml.Js in
                                               decode_string_of_js
                                                 (Unsafe.get labeledArgs "path")))))
                              );
                              ( "openFileRo",
                                Unsafe.inject
                                  (Unsafe.callback (fun labeledArgs ->
                                       (fun x ->
                                         encode_byte_source_to_js
                                           (unwrap_endemic x))
                                         (M.open_file_ro
                                            ~path:
                                              (let open Js_of_ocaml.Js in
                                               decode_string_of_js
                                                 (Unsafe.get labeledArgs "path")))))
                              );
                            |]
                        [@@warning "-39"]

                        module Peer_object_registry = struct
                          open Bindoj_runtime

                          let registry_of_logger = ref StringMap.empty

                          let lookup_logger ~id =
                            !registry_of_logger
                            |> StringMap.find_opt
                                 (let open Map_key in
                                  encode_map_key ~check_type:`string
                                    (Mk_string id))

                          and register_logger ~id value =
                            registry_of_logger :=
                              !registry_of_logger
                              |> StringMap.update
                                   (let open Map_key in
                                    encode_map_key ~check_type:`string
                                      (Mk_string id))
                                   (fun _ -> value)

                          let () =
                            let open Js_of_ocaml in
                            let js_registry =
                              Js.Unsafe.get
                                (Js.Unsafe.get peer_full_bridge "instance")
                                "endemicObjectRegistry"
                            in
                            Js.Unsafe.get
                              (Js.Unsafe.get js_registry "logger")
                              "initialObjects"
                            |> Js.to_array
                            |> Array.iter (fun x ->
                                   let coordinate =
                                     Js.Unsafe.get x "coordinate"
                                   in
                                   match
                                     Js.to_string (Js.Unsafe.get x "kind")
                                   with
                                   | "register" ->
                                       register_logger
                                         ~id:
                                           (decode_string_of_js
                                              (Js.Unsafe.get coordinate "id"))
                                         (Some
                                            ((wrap_peer decode_Logger_of_js)
                                               (Js.Unsafe.get x "value")))
                                   | "deregister" ->
                                       register_logger
                                         ~id:
                                           (decode_string_of_js
                                              (Js.Unsafe.get coordinate "id"))
                                         None
                                   | kind ->
                                       failwith ("unexpected kind: " ^ kind));
                            Js.Unsafe.set
                              (Js.Unsafe.get peer_full_bridge "instance")
                              "endemicObjectRegistry"
                              (let open Js_of_ocaml.Js in
                               Unsafe.obj
                                 [|
                                   ( "logger",
                                     Unsafe.inject
                                       (let open Js_of_ocaml.Js in
                                        Unsafe.obj
                                          [|
                                            ( "register",
                                              Unsafe.inject
                                                (Unsafe.callback_with_arity 2
                                                   (fun coordinate ->
                                                     fun value ->
                                                      register_logger
                                                        ~id:
                                                          (decode_string_of_js
                                                             (Js.Unsafe.get
                                                                coordinate "id"))
                                                        (Some
                                                           ((wrap_peer
                                                               decode_Logger_of_js)
                                                              value)))) );
                                            ( "deregister",
                                              Unsafe.inject
                                                (Unsafe.callback
                                                   (fun coordinate ->
                                                     register_logger
                                                       ~id:
                                                         (decode_string_of_js
                                                            (Js.Unsafe.get
                                                               coordinate "id"))
                                                       None)) );
                                          |]) );
                                 |])
                        end

                        module Peer_objects = struct
                          let my_logger =
                            let open Js_of_ocaml in
                            Js.Unsafe.get
                              (Js.Unsafe.get
                                 (Js.Unsafe.get peer_full_bridge "instance")
                                 "endemicObjects")
                              "myLogger"
                            |> wrap_peer decode_Logger_of_js
                        end

                        module Endemic_object_registry = struct
                          open Bindoj_runtime

                          let registry_of_logger = ref StringMap.empty

                          let lookup_logger ~name ~variant =
                            !registry_of_logger
                            |> StringMap.find_opt
                                 (let open Map_key in
                                  String.concat ""
                                    [
                                      encode_map_key ~check_type:`string
                                        (Mk_string name);
                                      encode_map_key
                                        ~check_type:
                                          (`StringEnum
                                             [ "persistent"; "transient" ])
                                        (Mk_string_enum
                                           (match variant with
                                           | `persistent -> "persistent"
                                           | `transient -> "transient"));
                                    ])

                          and register_logger ~name ~variant value =
                            registry_of_logger :=
                              !registry_of_logger
                              |> StringMap.update
                                   (let open Map_key in
                                    String.concat ""
                                      [
                                        encode_map_key ~check_type:`string
                                          (Mk_string name);
                                        encode_map_key
                                          ~check_type:
                                            (`StringEnum
                                               [ "persistent"; "transient" ])
                                          (Mk_string_enum
                                             (match variant with
                                             | `persistent -> "persistent"
                                             | `transient -> "transient"));
                                      ])
                                   (fun _ -> value)
                        end

                        let () =
                          (let open Endemic_object_registry in
                           let open Js_of_ocaml in
                           Js.Unsafe.set
                             (Js.Unsafe.get peer_full_bridge "instance")
                             "peerObjectRegistry"
                             (let open Js_of_ocaml.Js in
                              Unsafe.obj
                                [|
                                  ( "lookupLogger",
                                    Unsafe.inject
                                      (Unsafe.callback (fun coordinate ->
                                           lookup_logger
                                             ~name:
                                               (decode_string_of_js
                                                  (Js.Unsafe.get coordinate
                                                     "name"))
                                             ~variant:
                                               ((fun jv ->
                                                  match
                                                    Js_of_ocaml.Js.to_string jv
                                                  with
                                                  | "persistent" -> `persistent
                                                  | "transient" -> `transient
                                                  | s ->
                                                      Format.kasprintf failwith
                                                        "given string '%s' is \
                                                         not one of [ \
                                                         'persistent', \
                                                         'transient' ]"
                                                        s)
                                                  (Js.Unsafe.get coordinate
                                                     "variant"))
                                           |> function
                                           | None -> Js.Unsafe.inject Js.null
                                           | Some obj ->
                                               Js.Unsafe.inject
                                                 (encode_Logger_to_js obj))) );
                                |]));
                          let open Js_of_ocaml in
                          Js.Unsafe.set
                            (Js.Unsafe.get peer_full_bridge "instance")
                            "peerObjects"
                            (let open Js_of_ocaml.Js in
                             Unsafe.obj
                               [|
                                 ( "systemIo",
                                   Unsafe.inject
                                     (encode_System_io_to_js M.system_io) );
                               |])
                      end : Bridge)
                    in
                    ( concrete_bridge |> fun (module B) ->
                      M.initially_registry_objects
                        (module B.Endemic_object_registry) );
                    bridge_opt := Some concrete_bridge;
                    !continuations |> List.iter (fun f -> f concrete_bridge);
                    continuations := [])) );
         |])
      |> fun x ->
      (Obj.magic x : Bindoj_objintf_shared.endemic_full_bridge_reference)
  end
