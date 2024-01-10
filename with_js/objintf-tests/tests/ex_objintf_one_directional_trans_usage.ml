open Bindoj_objintf_gen_jsoo_test_gen_output.Ex_objintf_one_directional_jsoo_trans_gen

open Concrete_bridge_interfaces

let buffer: string list ref = ref []

module Bridge = Full_bridge_with_jsoo(struct
  let my_string = "ABC"
  let my_hello : Concrete_bridge_interfaces.Interfaces.hello =
    fun name -> refappend buffer ("[my_hello] "^name)
  let my_sole_var : string = "my_sole_var"
  let my_unit_sole : Concrete_bridge_interfaces.Interfaces.unit_sole =
    fun s () () -> s^"!"
  
  let make_unit_mod name prefix suffix =
    (module struct
      let name () = name
      let unit_01 s () = prefix^prefix^prefix^"_"^s^"_"^suffix
      let unit_02 s () () = prefix^prefix^"_"^s^"_"^suffix^suffix
      let unit_03 s () () () = prefix^"_"^s^"_"^suffix^suffix^suffix
    end : Simple_interfaces.unit_mod)

  let make_unit_obj name prefix suffix =
    let open (val (make_unit_mod name prefix suffix)) in
    object
      method name = name ()
      method unit_01 = unit_01
      method unit_02 = unit_02
      method unit_03 = unit_03
    end

  let my_unit_obj : Interfaces.unit_obj =
    make_unit_obj "my_unit_obj" "p" "o"
  let my_unit_mod : (module Simple_interfaces.unit_mod) =
    make_unit_mod "my_unit_mod" "p" "m"
  let make_rec_obj name =
    let x = ref None in
    x :=
      Some(object
        method name = name
        method get_self = bridge (Option.get !x)
      end);
    Option.get !x
  let my_rec_obj : Interfaces.rec_obj =
    make_rec_obj "my_rec_obj"

  let my_non_json_values =
    let open Bindoj_objintf_gen_test_gen_utils.Utils in
    { bytes = Bytes.of_string "Hello, world!" }

  let with_default_value = (module (struct
    let get_default_string ?str : string = Option.get str
    [@@warning "-16"]
    let get_default_student ?student = (Option.get student).name
    [@@warning "-16"]
  end) : Interfaces.with_default_value)
  
  let initially_registry_objects (module E: Endemic_object_registry_interface) =
    E.register_hello ~id:"foo" (some & fun name -> refappend buffer ("hello (foo): "^name));
    E.register_hello ~id:"bar" (some & fun name -> refappend buffer ("hello (bar): "^name));
    ()
end)

let () =
  let open Js_of_ocaml in
  Js.export "usage_one_directional_trans" (object%js
    val full_bridge_js = Bridge.endemic_full_bridge

    method register_js =
      Bridge.Endemic_object_registry.(
        register_string
          ~id0:"1" ~id1:(Int53p.of_float 1.2)
          (Some "registered string");
      )

    method buffer_js = !buffer |&> Js.string |> List.rev |> Array.of_list |> Js.array
    method clear_buffer_js = refset buffer []
  end)
