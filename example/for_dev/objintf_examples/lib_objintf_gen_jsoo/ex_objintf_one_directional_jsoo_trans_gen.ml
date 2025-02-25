open Bindoj_objintf_gen_jsoo_test_gen_utils [@@warning "-33"]

type ex_record_student = { admission_year : int; name : string }

let rec (ex_record_student_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { admission_year; name } ->
             StringMap.of_list
               [
                 ("admission_year", Expr.of_int admission_year);
                 ("name", Expr.of_string name);
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "admission_year" >>= Expr.to_int
             >>= fun admission_year ->
             xs |> StringMap.find_opt "name" >>= Expr.to_string >>= fun name ->
             Some { admission_year; name });
       })
[@@warning "-33-39"]

let ex_record_student_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExRecordStudent",
           `object_of
             [
               `mandatory_field ("admissionYear", `integral);
               `mandatory_field ("name", `string);
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_record_student_to_json =
  (let string_to_json (x : string) = (`str x : Kxclib.Json.jv)
   and int_to_json (x : int) = (`num (float_of_int x) : Kxclib.Json.jv) in
   fun { admission_year = x0; name = x1 } ->
     `obj [ ("admissionYear", int_to_json x0); ("name", string_to_json x1) ]
    : ex_record_student -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_record_student_of_json' =
  (fun ?(path = []) ->
     fun x ->
      (let rec of_json_impl =
         let string_of_json' path = function
           | (`str x : Kxclib.Json.jv) -> Ok x
           | jv ->
               Error
                 ( Printf.sprintf
                     "expecting type 'string' but the given is of type '%s'"
                     (let open Kxclib.Json in
                      string_of_jv_kind (classify_jv jv)),
                   path )
         and int_of_json' path = function
           | (`num x : Kxclib.Json.jv) ->
               if Float.is_integer x then Ok (int_of_float x)
               else
                 Error
                   ( Printf.sprintf "expecting an integer but the given is '%f'"
                       x,
                     path )
           | jv ->
               Error
                 ( Printf.sprintf
                     "expecting type 'int' but the given is of type '%s'"
                     (let open Kxclib.Json in
                      string_of_jv_kind (classify_jv jv)),
                   path )
         in
         fun path ->
           fun __bindoj_orig ->
            match __bindoj_orig with
            | `obj param ->
                let ( >>= ) = Result.bind in
                List.assoc_opt "admissionYear" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'admissionYear' does not exist", path)
                >>= int_of_json' (`f "admissionYear" :: path)
                >>= fun x0 ->
                List.assoc_opt "name" param
                |> Option.to_result
                     ~none:("mandatory field 'name' does not exist", path)
                >>= string_of_json' (`f "name" :: path)
                >>= fun x1 -> Ok { admission_year = x0; name = x1 }
            | jv ->
                Error
                  ( Printf.sprintf
                      "an object is expected for a record value, but the given \
                       is of type '%s'"
                      (let open Kxclib.Json in
                       string_of_jv_kind (classify_jv jv)),
                    path )
       in
       of_json_impl)
        path x
      |> Result.map_error (fun (msg, path) ->
             (msg, path, ex_record_student_json_shape_explanation))
    : ex_record_student Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_record_student_of_json =
  (fun x -> ex_record_student_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_record_student option)
[@@warning "-39"]

module Simple_interfaces = struct
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

module type Endemic_object_registry_interface = sig
  val register_string : id0:string -> id1:Kxclib.int53p -> string option -> unit
  val register_hello : id:string -> hello option -> unit
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
    type nonrec sole_var = string

    class type rec_obj = object
      method name : string
      method get_self : rec_obj endemic
    end
  end

  module Interfaces = struct
    module Simple_interfaces = Simple_interfaces
    module Complex_interfaces = Complex_interfaces
    include Simple_interfaces
    include Complex_interfaces
  end
end

module type Concrete_bridge = sig
  include module type of Concrete_bridge_interfaces
  module Endemic_object_registry : Endemic_object_registry_interface
end

open Concrete_bridge_interfaces.Interfaces [@@warning "-33"]

module type Endemic_setup_only_full_bridge = functor
  (M : sig
     val my_string : string
     val my_hello : hello
     val my_sole_var : sole_var
     val my_unit_sole : unit_sole
     val my_unit_obj : unit_obj
     val my_unit_mod : (module unit_mod)
     val my_rec_obj : rec_obj
     val my_non_json_values : Utils.non_json_values
     val with_default_value : (module with_default_value)

     val initially_registry_objects :
       (module Endemic_object_registry_interface) -> unit
   end)
  -> sig
  include Concrete_bridge

  val endemic_full_bridge : Bindoj_objintf_shared.endemic_full_bridge_reference
end

module Full_bridge_with_jsoo : Endemic_setup_only_full_bridge =
functor
  (M : sig
     val my_string : string
     val my_hello : hello
     val my_sole_var : sole_var
     val my_unit_sole : unit_sole
     val my_unit_obj : unit_obj
     val my_unit_mod : (module unit_mod)
     val my_rec_obj : rec_obj
     val my_non_json_values : Utils.non_json_values
     val with_default_value : (module with_default_value)

     val initially_registry_objects :
       (module Endemic_object_registry_interface) -> unit
   end)
  ->
  struct
    include Concrete_bridge_interfaces

    let unwrap_endemic
        (Bindoj_objintf_shared.Endemic_object { bridge = Br; underlying }) =
      underlying
    [@@warning "-32"]

    and wrap_peer access raw_object =
      Bindoj_objintf_shared.Peer_object { bridge = Br; access; raw_object }
    [@@warning "-32"]

    let encode_unit_to_js () = Js_of_ocaml.Js.null [@@warning "-32"]
    and encode_string_to_js = Js_of_ocaml.Js.string [@@warning "-32"]
    and decode_unit_of_js _ = () [@@warning "-32"]
    and decode_string_of_js = Js_of_ocaml.Js.to_string [@@warning "-32"]
    and decode_int53p_of_js = Kxclib.Int53p.of_float [@@warning "-32"]

    let rec encode_hello_to_js (__caml_obj : hello) =
      Js_of_ocaml.Js.Unsafe.callback (fun name ->
          encode_unit_to_js (__caml_obj (decode_string_of_js name)))
    [@@warning "-39"]

    and encode_unit_sole_to_js (__caml_obj : unit_sole) =
      Js_of_ocaml.Js.Unsafe.callback (fun __arg0 ->
          encode_string_to_js (__caml_obj (decode_string_of_js __arg0) () ()))
    [@@warning "-39"]

    and encode_unit_obj_to_js (__caml_obj : unit_obj) =
      let open Js_of_ocaml.Js in
      Unsafe.obj
        [|
          ( "name",
            Unsafe.inject
              (Unsafe.callback (fun () -> encode_string_to_js __caml_obj#name))
          );
          ( "unit01",
            Unsafe.inject
              (Unsafe.callback (fun __arg0 ->
                   encode_string_to_js
                     (__caml_obj#unit_01 (decode_string_of_js __arg0) ()))) );
          ( "unit02",
            Unsafe.inject
              (Unsafe.callback (fun __arg0 ->
                   encode_string_to_js
                     (__caml_obj#unit_02 (decode_string_of_js __arg0) () ())))
          );
          ( "unit03",
            Unsafe.inject
              (Unsafe.callback (fun __arg0 ->
                   encode_string_to_js
                     (__caml_obj#unit_03 (decode_string_of_js __arg0) () () ())))
          );
        |]
    [@@warning "-39"]

    and encode_unit_mod_to_js (module M : unit_mod) =
      let open Js_of_ocaml.Js in
      Unsafe.obj
        [|
          ( "name",
            Unsafe.inject
              (Unsafe.callback (fun () -> encode_string_to_js (M.name ()))) );
          ( "unit01",
            Unsafe.inject
              (Unsafe.callback (fun __arg0 ->
                   encode_string_to_js
                     (M.unit_01 (decode_string_of_js __arg0) ()))) );
          ( "unit02",
            Unsafe.inject
              (Unsafe.callback (fun __arg0 ->
                   encode_string_to_js
                     (M.unit_02 (decode_string_of_js __arg0) () ()))) );
          ( "unit03",
            Unsafe.inject
              (Unsafe.callback (fun __arg0 ->
                   encode_string_to_js
                     (M.unit_03 (decode_string_of_js __arg0) () () ()))) );
        |]
    [@@warning "-39"]

    and encode_with_default_value_to_js (module M : with_default_value) =
      let open Js_of_ocaml.Js in
      Unsafe.obj
        [|
          ( "getDefaultString",
            Unsafe.inject
              (Unsafe.callback (fun labeledArgs ->
                   encode_string_to_js
                     (M.get_default_string
                        ~str:
                          ((let open Js_of_ocaml.Js in
                            Optdef.bind labeledArgs (fun la ->
                                Optdef.map (Unsafe.get la "str")
                                  decode_string_of_js)
                            |> Optdef.to_option)
                          |> Option.value ~default:"Hello")))) );
          ( "getDefaultStudent",
            Unsafe.inject
              (Unsafe.callback (fun labeledArgs ->
                   encode_string_to_js
                     (M.get_default_student
                        ~student:
                          ((let open Js_of_ocaml.Js in
                            Optdef.bind labeledArgs (fun la ->
                                Optdef.map (Unsafe.get la "student") (fun x ->
                                    Kxclib_js.Json_ext.of_xjv x
                                    |> ex_record_student_of_json'
                                    |> function
                                    | Error e ->
                                        failwith
                                          (Bindoj_runtime.OfJsonResult.Err
                                           .to_string e)
                                    | Ok result -> result))
                            |> Optdef.to_option)
                          |> Option.value
                               ~default:
                                 {
                                   name = "William Gibson";
                                   admission_year = 1984;
                                 })))) );
        |]
    [@@warning "-39"]

    let rec encode_sole_var_to_js (__caml_obj : sole_var) =
      Js_of_ocaml.Js.Unsafe.callback (fun () -> encode_string_to_js __caml_obj)
    [@@warning "-39"]

    and encode_rec_obj_to_js (__caml_obj : rec_obj) =
      let open Js_of_ocaml.Js in
      Unsafe.obj
        [|
          ( "name",
            Unsafe.inject
              (Unsafe.callback (fun () -> encode_string_to_js __caml_obj#name))
          );
          ( "getSelf",
            Unsafe.inject
              (Unsafe.callback (fun () ->
                   (fun x -> encode_rec_obj_to_js (unwrap_endemic x))
                     __caml_obj#get_self)) );
        |]
    [@@warning "-39"]

    let ref_setup_called = ref false

    module Endemic_object_registry = struct
      open Bindoj_runtime [@@warning "-33"]

      let registry_of_string = ref StringMap.empty

      let lookup_string ~id0 ~id1 =
        !registry_of_string
        |> StringMap.find_opt
             (let open Map_key in
              String.concat ""
                [
                  encode_map_key ~check_type:`string (Mk_string id0);
                  encode_map_key ~check_type:`int53p (Mk_int53 id1);
                ])

      and register_string ~id0 ~id1 value =
        registry_of_string :=
          !registry_of_string
          |> StringMap.update
               (let open Map_key in
                String.concat ""
                  [
                    encode_map_key ~check_type:`string (Mk_string id0);
                    encode_map_key ~check_type:`int53p (Mk_int53 id1);
                  ])
               (fun _ -> value)

      let registry_of_hello = ref StringMap.empty

      let lookup_hello ~id =
        !registry_of_hello
        |> StringMap.find_opt
             (let open Map_key in
              encode_map_key ~check_type:`string (Mk_string id))

      and register_hello ~id value =
        registry_of_hello :=
          !registry_of_hello
          |> StringMap.update
               (let open Map_key in
                encode_map_key ~check_type:`string (Mk_string id))
               (fun _ -> value)
    end

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
                    (let open Endemic_object_registry in
                     let open Js_of_ocaml in
                     Js.Unsafe.set
                       (Js.Unsafe.get peer_full_bridge "instance")
                       "peerObjectRegistry"
                       (let open Js_of_ocaml.Js in
                        Unsafe.obj
                          [|
                            ( "lookupString",
                              Unsafe.inject
                                (Unsafe.callback (fun coordinate ->
                                     lookup_string
                                       ~id0:
                                         (decode_string_of_js
                                            (Js.Unsafe.get coordinate "id0"))
                                       ~id1:
                                         (decode_int53p_of_js
                                            (Js.Unsafe.get coordinate "id1"))
                                     |> function
                                     | None -> Js.Unsafe.inject Js.null
                                     | Some obj ->
                                         Js.Unsafe.inject
                                           (encode_string_to_js obj))) );
                            ( "lookupHello",
                              Unsafe.inject
                                (Unsafe.callback (fun coordinate ->
                                     lookup_hello
                                       ~id:
                                         (decode_string_of_js
                                            (Js.Unsafe.get coordinate "id"))
                                     |> function
                                     | None -> Js.Unsafe.inject Js.null
                                     | Some obj ->
                                         Js.Unsafe.inject
                                           (encode_hello_to_js obj))) );
                          |])) [@warning "-33"];
                    let open Js_of_ocaml in
                    Js.Unsafe.set
                      (Js.Unsafe.get peer_full_bridge "instance")
                      "peerObjects"
                      (let open Js_of_ocaml.Js in
                       Unsafe.obj
                         [|
                           ( "myString",
                             Unsafe.inject (encode_string_to_js M.my_string) );
                           ( "myHello",
                             Unsafe.inject (encode_hello_to_js M.my_hello) );
                           ( "mySoleVar",
                             Unsafe.inject (encode_sole_var_to_js M.my_sole_var)
                           );
                           ( "myUnitSole",
                             Unsafe.inject
                               (encode_unit_sole_to_js M.my_unit_sole) );
                           ( "myUnitObj",
                             Unsafe.inject (encode_unit_obj_to_js M.my_unit_obj)
                           );
                           ( "myUnitMod",
                             Unsafe.inject (encode_unit_mod_to_js M.my_unit_mod)
                           );
                           ( "myRecObj",
                             Unsafe.inject (encode_rec_obj_to_js M.my_rec_obj)
                           );
                           ( "myNonJsonValues",
                             Unsafe.inject
                               ((let open Utils in
                                 encode_non_json_values_to_js)
                                  M.my_non_json_values) );
                           ( "withDefaultValue",
                             Unsafe.inject
                               (encode_with_default_value_to_js
                                  M.with_default_value) );
                         |]))) );
         |])
      |> fun x ->
      (Obj.magic x : Bindoj_objintf_shared.endemic_full_bridge_reference)

    let () = M.initially_registry_objects (module Endemic_object_registry)
  end
