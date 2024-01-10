open Bindoj_objintf_gen_jsoo_test_gen_output.Ex_objintf_system_io_jsoo_trans_gen

open Concrete_bridge_interfaces

let create_output_channel =
  fun name ->
    let buf = ref Bytes.empty in
    buf, (object (self)
      method channel_name = name
      method write bytes =
        refset buf (Bytes.cat !buf bytes)

      method write_bulk (src: Interfaces.byte_source peer) =
        let src = access src in
        while src#bytes_left () > 0 do
          self#write(src#next_block ~max:1 ())
        done

      method write_async (src: Interfaces.byte_source' peer) =
        let rec loop () = 
          match (access src) () with
          | `eof -> ()
          | `data (b, s) ->
            self#write b;
            begin match s with
            | `eof -> ()
            | `maybe_more -> loop ()
            end
          | `wait -> loop ()
        in
        loop ()
    end :> Interfaces.output_channel)

let mock_stdout = create_output_channel "stdout"
let mock_stderr = create_output_channel "stderr"
let mock_files = ref StringMap.empty

let get_file path =
  refget mock_files |> (fun m ->
    match StringMap.find_opt path m with
    | Some f -> f
    | None ->
      let f = create_output_channel path in
      refset mock_files (StringMap.add path f m);
      f
  )

module Usage(Bridge : Concrete_bridge) = struct
  let register_remote_logger () =
    let output = ref "" in
    Bridge.Endemic_object_registry.register_logger
      ~name:"remote"
      ~variant:`persistent
      & Some (module struct
        let info msg = refset output msg
        let error ?exn msg =
          refset output (match exn with
          | None -> msg
          | Some e -> sprintf "[%s] %s" e msg)
      end);
      output

  let clear_stdout () = refset (fst mock_stdout) Bytes.empty
  
  let get_stdout_content () =
    fst mock_stdout |> refget |> Bytes.to_string
  
  let get_file_content path =
    get_file path |> fst |> refget |> Bytes.to_string
end

let () =
  let open Js_of_ocaml in
  let usage_js : 'a ref = ref Js.Opt.empty in

  let module Bridge = Full_bridge_with_jsoo(struct
      let system_io = (module struct
        let stdout () = bridge & snd mock_stdout
        let stderr () = bridge & snd mock_stderr
        let open_file_wo ~path =
          let _, channel = get_file path in
          bridge channel

        let open_file_ro ~path =
          let buf, _ = get_file path in
          bridge (object (self)
            val mutable index = 0
            method bytes_left () = (Bytes.length & refget buf) - index |> max 0
            method next_block ?max () =
              match max with
              | None -> refget buf
              | Some max when max >= self#bytes_left () -> self#next_block ()
              | Some max ->
                let res = (Bytes.sub & refget buf) index & self#bytes_left () in
                index <- index + max;
                res
          end)
      end : Interfaces.System_io)

      let initially_registry_objects _ = ()
    end)
  in

  Bridge.get_bridge_async(fun (module C : Concrete_bridge) ->
    let module Usage = Usage(C) in
    refset usage_js & Js.Opt.return (object%js
      method register_remote_logger_js =
        let output = Usage.register_remote_logger () in
        object%js
          method output = output |> refget |> Js.string
        end
      method clear_stdout_js = Usage.clear_stdout ()
      method get_stdout_content_js = Usage.get_stdout_content () |> Js.string
      method get_file_content_js path = Usage.get_file_content path |> Js.string
    end)
  );

  Js.export "usage_system_io_trans" (object%js
    val full_bridge_js = Bridge.endemic_full_bridge
    method usage_js = !usage_js
  end)
