open Bindoj_objintf_gen_jsoo_test_gen_output.Ex_objintf_system_io_jsoo_gen

module Usage(Bridge : Concrete_bridge) = struct
  open Bridge

  let example_remote_logging () =
    let (module Remote_logger) =
      (Peer_object_registry.lookup_logger
          ~name:"remote" ~variant:`persistent
        |?! (fun () -> failwith "failed to lookup logger")
      |> access)
    in
    Remote_logger.info "hello from Usage1"

  module System_io = (val (access Peer_objects.system_io))

  let example_stdout () =
    (access & System_io.stdout ())#write (Bytes.of_string "hello")

  let example_pass_endemic_object () =
    let rest = ref "hello?" in
    (access & System_io.stdout ())#write_bulk (object (self)
      method bytes_left () = String.length !rest
      method next_block ?max () =
        match max with
        | None ->
            !rest |-> (fun _ -> rest := "")
            |> Bytes.of_string
        | Some n when n >= self#bytes_left () -> self#next_block ()
        | Some n ->
            let len = self#bytes_left () in
            let contents = String.sub !rest 0 n in
            rest := String.sub !rest n (len - n);
            Bytes.of_string contents
      end |> bridge)

  let example_file_write () =
    let rest = ref "hello?" in
    (access & System_io.open_file_wo ~path:"text.txt")#write_async ((
        fun ?max () ->
        let len = String.length !rest in
        let output size =
          !rest |-> (fun s -> rest := String.sub s size (len - size))
          |> (fun s -> String.sub s 0 size)
          |> Bytes.of_string
        in
        match len, max with
        | 0, _ -> `eof
        | l, Some m when m < l -> `data (output m, `maybe_more)
        | _ -> `data (output len, `eof))
        |> bridge)
end

let () =
  let open Js_of_ocaml in
  let usage : 'a ref = ref Js.null in

  let module Bridge = Full_bridge_with_jsoo(struct
      let my_logger = (module struct
        let info : string -> unit =
          fun s -> printf "%s" s
        let error : ?exn:string -> string -> unit =
          fun ?exn s ->
            match exn with
            | None -> printf "%s" s
            | Some e -> printf "[%s] %s" e s
      end : Simple_interfaces.Logger)
      let initially_registry_objects: (module Endemic_object_registry_interface) -> unit =
        ignore
    end)
  in

  Bridge.get_bridge_async(fun (module C : Concrete_bridge) ->
    let module Usage = Usage(C) in
    refset usage & Obj.magic(object%js
      method example_remote_logging_js = Usage.example_remote_logging ()
      method example_stdout_js = Usage.example_stdout ()
      method example_pass_endemic_object_js = Usage.example_pass_endemic_object ()
      method example_file_write_js = Usage.example_file_write ()
    end)
  );

  Js.export "usage_system_io" (object%js
    val full_bridge_js = Bridge.endemic_full_bridge
    method usage_js = !usage
  end)
