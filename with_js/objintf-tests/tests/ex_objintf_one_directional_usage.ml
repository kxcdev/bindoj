open Bindoj_objintf_gen_jsoo_test_gen_output.Ex_objintf_one_directional_jsoo_gen

module Usage(Bridge : Concrete_bridge) = struct
  open struct    
    let from_ocaml s = s ^ " from OCaml"

    let my_hello name =
      Bridge.(access Peer_objects.my_hello (from_ocaml name))
  end
  
  let hello_my_string () =
    let name = Bridge.Peer_objects.my_string in
    my_hello name
  
  let hello_sole_var () =
    my_hello Bridge.(access Peer_objects.my_sole_var)
  
  let hello_my_string_unit_sole () =
    let name = Bridge.Peer_objects.my_string in
    my_hello Bridge.(access Peer_objects.my_unit_sole name () ())
  
  let hello_my_string_unit_obj () =
    let hello f =
      my_hello Bridge.(access Peer_objects.my_unit_obj |> f)
    in
    hello (fun o -> o#name);
    hello (fun o -> o#unit_01 o#name ());
    hello (fun o -> o#unit_02 o#name () ());
    hello (fun o -> o#unit_03 o#name () () ())

  let hello_my_string_unit_mod () =
    let hello f =
      my_hello Bridge.(access Peer_objects.my_unit_mod |> f)
    in
    hello (fun (module M) -> M.name ());
    hello (fun (module M) -> M.unit_01 (M.name ()) ());
    hello (fun (module M) -> M.unit_02 (M.name ()) () ());
    hello (fun (module M) -> M.unit_03 (M.name ()) () () ())

  let hello_registry () =
    let open Bridge.Peer_object_registry in
    let name = 
      lookup_string
        ~cdn_id0:"1"
        ~cdn_id1:(Int53p.of_float 1.2)
      |?! (fun () -> failwith "string not found")
      |> from_ocaml
    in
    let hello =
      lookup_hello
        ~cdn_id:"foo"
      |?! (fun () -> failwith "hello not found")
    in
    Bridge.access hello name
  
  let hello_rec_obj () =
    let open Bridge in
    let name =
      Peer_objects.my_rec_obj
      |> (access &> fun x -> x#get_self)
      |> (access &> fun x -> x#name)
    in
    my_hello name

  let hello_non_json_values () =
    let v = Bridge.Peer_objects.my_non_json_values in
    my_hello (Bytes.to_string v.bytes)

  let hello_with_default_value () =
    let module V = (val Bridge.(access Peer_objects.with_default_value)) in
    my_hello (V.get_default_string ?larg_str:None);
    my_hello (V.get_default_student ?larg_student:None);
end

let () =
  let open Js_of_ocaml.Js in
  let usage : 'a ref = ref null in
  let module Bridge = Full_bridge_with_jsoo in
  Bridge.get_bridge_async(fun (module C : Concrete_bridge) ->
    let module Usage = Usage(C) in
    refset usage & Obj.magic(object%js
      method hello_my_string_js = Usage.hello_my_string ()
      method hello_sole_var_js = Usage.hello_sole_var ()
      method hello_my_string_unit_sole_js = Usage.hello_my_string_unit_sole ()
      method hello_my_string_unit_obj_js = Usage.hello_my_string_unit_obj ()
      method hello_my_string_unit_mod_js = Usage.hello_my_string_unit_mod ()
      method hello_registry_js = Usage.hello_registry ()
      method hello_rec_obj_js = Usage.hello_rec_obj ()
      method hello_non_json_values_js = Usage.hello_non_json_values ()
      method hello_with_default_value_js = Usage.hello_with_default_value ()
    end));
  
  export "usage_one_directional" (object%js
    val full_bridge_js = Bridge.endemic_full_bridge
    method usage_js = !usage
  end)
