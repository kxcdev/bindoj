open Bindoj_objintf_gen_jsoo_test_gen_utils[@@warning "-33"]
type ex_record_student = {
  admission_year: int ;
  name: string }
let rec (ex_record_student_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
       let open Kxclib.Option.Ops_monad in
         Refl.Record
           {
             get =
               (fun { admission_year; name } ->
                  StringMap.of_list
                    [("admission_year", (Expr.of_int admission_year));
                    ("name", (Expr.of_string name))]);
             mk =
               (fun xs ->
                  ((xs |> (StringMap.find_opt "admission_year")) >>=
                     Expr.to_int)
                    >>=
                    (fun admission_year ->
                       ((xs |> (StringMap.find_opt "name")) >>=
                          Expr.to_string)
                         >>= (fun name -> Some { admission_year; name })))
           })[@@warning "-33-39"]
let ex_record_student_json_shape_explanation =
  (`with_warning
     ("not considering any config if exists",
       (`named
          ("ExRecordStudent",
            (`object_of
               [`mandatory_field ("admissionYear", `integral);
               `mandatory_field ("name", `string)])))) : Bindoj_runtime.json_shape_explanation)
  [@@warning "-39"]
let rec ex_record_student_to_json =
  (let string_to_json (x : string) = (`str x : Kxclib.Json.jv)
   and int_to_json (x : int) = (`num (float_of_int x) : Kxclib.Json.jv) in
   fun { admission_year = x0; name = x1 } ->
     `obj
       [("admissionYear", (int_to_json x0)); ("name", (string_to_json x1))] : 
  ex_record_student -> Kxclib.Json.jv)[@@warning "-39"]
and ex_record_student_of_json' =
  (fun ?(path= []) ->
     fun x ->
       ((let rec of_json_impl =
           let string_of_json' path =
             function
             | (`str x : Kxclib.Json.jv) -> Ok x
             | jv ->
                 Error
                   ((Printf.sprintf
                       "expecting type 'string' but the given is of type '%s'"
                       (let open Kxclib.Json in
                          string_of_jv_kind (classify_jv jv))), path)
           and int_of_json' path =
             function
             | (`num x : Kxclib.Json.jv) ->
                 if Float.is_integer x
                 then Ok (int_of_float x)
                 else
                   Error
                     ((Printf.sprintf
                         "expecting an integer but the given is '%f'" x),
                       path)
             | jv ->
                 Error
                   ((Printf.sprintf
                       "expecting type 'int' but the given is of type '%s'"
                       (let open Kxclib.Json in
                          string_of_jv_kind (classify_jv jv))), path) in
           fun path ->
             fun __bindoj_orig ->
               match __bindoj_orig with
               | `obj param ->
                   let (>>=) = Result.bind in
                   (((List.assoc_opt "admissionYear" param) |>
                       (Option.to_result
                          ~none:("mandatory field 'admissionYear' does not exist",
                                  path)))
                      >>= (int_of_json' ((`f "admissionYear") :: path)))
                     >>=
                     ((fun x0 ->
                         (((List.assoc_opt "name" param) |>
                             (Option.to_result
                                ~none:("mandatory field 'name' does not exist",
                                        path)))
                            >>= (string_of_json' ((`f "name") :: path)))
                           >>=
                           (fun x1 -> Ok { admission_year = x0; name = x1 })))
               | jv ->
                   Error
                     ((Printf.sprintf
                         "an object is expected for a record value, but the given is of type '%s'"
                         (let open Kxclib.Json in
                            string_of_jv_kind (classify_jv jv))), path) in
         of_json_impl) path x) |>
         (Result.map_error
            (fun (msg, path) ->
               (msg, path, ex_record_student_json_shape_explanation))) : 
  ex_record_student Bindoj_runtime.json_full_decoder)[@@warning "-39"]
and ex_record_student_of_json =
  (fun x -> (ex_record_student_of_json' x) |> Result.to_option : Kxclib.Json.jv
                                                                   ->
                                                                   ex_record_student
                                                                    option)
[@@warning "-39"]
module Simple_interfaces =
  struct
    type nonrec hello = string -> unit
    type nonrec unit_sole = string -> unit -> unit -> string
    class type unit_obj =
      object
        method  name : string
        method  unit_01 : string -> unit -> string
        method  unit_02 : string -> unit -> unit -> string
        method  unit_03 : string -> unit -> unit -> unit -> string
      end
    module type unit_mod  =
      sig
        val name : unit -> string
        val unit_01 : string -> unit -> string
        val unit_02 : string -> unit -> unit -> string
        val unit_03 : string -> unit -> unit -> unit -> string
      end
    module type with_default_value  =
      sig
        val get_default_string : ?str:string -> string
        val get_default_student : ?student:ex_record_student -> string
      end
  end
open Simple_interfaces[@@warning "-33"]
module Concrete_bridge_interfaces =
  struct
    open Bindoj_objintf_shared
    
    (** marker type for this specific concrete bridge *)
    type nonrec br =
      | Br 
    type 'x peer = ('x, br) peer'
    type 'x endemic = ('x, br) endemic'
    let access : 'x peer -> 'x = access
    let bridge x = bridge_generic ~bridge:Br x
    module Simple_interfaces = Simple_interfaces
    module Complex_interfaces =
      struct
        type nonrec sole_var = string
        class type rec_obj =
          object method  name : string method  get_self : rec_obj peer end
      end
    module Interfaces =
      struct
        module Simple_interfaces = Simple_interfaces
        module Complex_interfaces = Complex_interfaces
        include Simple_interfaces
        include Complex_interfaces
      end
  end
module type Concrete_bridge  =
  sig
    include module type of Concrete_bridge_interfaces
    open Interfaces
    module Peer_object_registry :
    sig
      val lookup_string : id0:string -> id1:Kxclib.int53p -> string option
      val lookup_hello : id:string -> hello peer option
    end
    module Peer_objects :
    sig
      val my_string : string
      val my_hello : hello peer
      val my_sole_var : sole_var peer
      val my_unit_sole : unit_sole peer
      val my_unit_obj : unit_obj peer
      val my_unit_mod : (module unit_mod) peer
      val my_rec_obj : rec_obj peer
      val my_non_json_values : Utils.non_json_values
      val with_default_value : (module with_default_value) peer
    end
  end
open Concrete_bridge_interfaces.Interfaces[@@warning "-33"]
module type Peer_setup_only_full_bridge  =
  sig
    module type Bridge  = Concrete_bridge
    val get_bridge :
      unit -> [ `await_peer_setup  | `bridge of (module Concrete_bridge) ]
    val get_bridge_async : ((module Concrete_bridge) -> unit) -> unit
    val endemic_full_bridge :
      Bindoj_objintf_shared.endemic_full_bridge_reference
  end
module Full_bridge_with_jsoo : Peer_setup_only_full_bridge =
  struct
    module type Bridge  = Concrete_bridge
    let ref_setup_called = ref false
    let bridge_opt : (module Bridge) option ref = ref None
    let continuations = ref []
    let get_bridge () =
      match !bridge_opt with
      | None -> `await_peer_setup
      | Some b -> `bridge b
    let get_bridge_async f =
      match !bridge_opt with
      | None -> continuations := (f :: (!continuations))
      | Some b -> f b
    let endemic_full_bridge =
      (let open Js_of_ocaml.Js in
         Unsafe.obj
           [|("setupCalled",
               (Unsafe.inject (Unsafe.callback (fun () -> !ref_setup_called))));
             ("setup",
               (Unsafe.inject
                  (Unsafe.callback
                     (fun
                        (peer_full_bridge :
                          Bindoj_objintf_shared.endemic_full_bridge_reference)
                        ->
                        let () =
                          if !ref_setup_called
                          then
                            raise
                              Bindoj_objintf_shared.Full_Bridge_already_setup;
                          ref_setup_called := true in
                        (let open Js_of_ocaml in
                           if
                             not
                               (Js.Unsafe.get peer_full_bridge "setupCalled")
                           then
                             Js.Unsafe.meth_call peer_full_bridge "setup"
                               [|(Js.Unsafe.inject
                                    (let open Js_of_ocaml.Js in
                                       Unsafe.obj
                                         [|("setupCalled",
                                             (Unsafe.inject
                                                (Unsafe.callback
                                                   (fun () -> true))))|]))|]);
                        (let concrete_bridge = ((module
                           struct
                             include Concrete_bridge_interfaces
                             open Interfaces[@@warning "-33"]
                             let unwrap_endemic
                               (Bindoj_objintf_shared.Endemic_object
                               { bridge = Br; underlying }) = underlying
                             [@@warning "-32"]
                             and wrap_peer access raw_object =
                               Bindoj_objintf_shared.Peer_object
                                 { bridge = Br; access; raw_object }[@@warning
                                                                    "-32"]
                             let encode_unit_to_js () = Js_of_ocaml.Js.null
                             [@@warning "-32"]
                             and encode_string_to_js = Js_of_ocaml.Js.string
                             [@@warning "-32"]
                             and decode_unit_of_js _ = ()[@@warning "-32"]
                             and decode_string_of_js =
                               Js_of_ocaml.Js.to_string[@@warning "-32"]
                             and decode_int53p_of_js = Kxclib.Int53p.of_float
                             [@@warning "-32"]
                             let rec decode_hello_of_js __js_obj name =
                               let open Js_of_ocaml in
                                 decode_unit_of_js
                                   (Js.Unsafe.fun_call __js_obj
                                      [|(Js.Unsafe.inject
                                           (encode_string_to_js name))|])
                             [@@warning "-39"]
                             and decode_unit_sole_of_js __js_obj __arg0 () ()
                               =
                               let open Js_of_ocaml in
                                 decode_string_of_js
                                   (Js.Unsafe.fun_call __js_obj
                                      [|(Js.Unsafe.inject
                                           (encode_string_to_js __arg0))|])
                             [@@warning "-39"]
                             and decode_unit_obj_of_js __js_obj =
                               object
                                 method name =
                                   let open Js_of_ocaml in
                                     decode_string_of_js
                                       (Js.Unsafe.meth_call __js_obj "name"
                                          [||])
                                 method unit_01 __arg0 () =
                                   let open Js_of_ocaml in
                                     decode_string_of_js
                                       (Js.Unsafe.meth_call __js_obj "unit01"
                                          [|(Js.Unsafe.inject
                                               (encode_string_to_js __arg0))|])
                                 method unit_02 __arg0 () () =
                                   let open Js_of_ocaml in
                                     decode_string_of_js
                                       (Js.Unsafe.meth_call __js_obj "unit02"
                                          [|(Js.Unsafe.inject
                                               (encode_string_to_js __arg0))|])
                                 method unit_03 __arg0 () () () =
                                   let open Js_of_ocaml in
                                     decode_string_of_js
                                       (Js.Unsafe.meth_call __js_obj "unit03"
                                          [|(Js.Unsafe.inject
                                               (encode_string_to_js __arg0))|])
                               end[@@warning "-39"]
                             and decode_unit_mod_of_js __js_obj = ((module
                               struct
                                 let name () =
                                   let open Js_of_ocaml in
                                     decode_string_of_js
                                       (Js.Unsafe.meth_call __js_obj "name"
                                          [||])
                                 and unit_01 __arg0 () =
                                   let open Js_of_ocaml in
                                     decode_string_of_js
                                       (Js.Unsafe.meth_call __js_obj "unit01"
                                          [|(Js.Unsafe.inject
                                               (encode_string_to_js __arg0))|])
                                 and unit_02 __arg0 () () =
                                   let open Js_of_ocaml in
                                     decode_string_of_js
                                       (Js.Unsafe.meth_call __js_obj "unit02"
                                          [|(Js.Unsafe.inject
                                               (encode_string_to_js __arg0))|])
                                 and unit_03 __arg0 () () () =
                                   let open Js_of_ocaml in
                                     decode_string_of_js
                                       (Js.Unsafe.meth_call __js_obj "unit03"
                                          [|(Js.Unsafe.inject
                                               (encode_string_to_js __arg0))|])
                               end) : (module unit_mod))[@@warning "-39"]
                             and decode_with_default_value_of_js __js_obj =
                               ((module
                               struct
                                 let get_default_string ?str  =
                                   let open Js_of_ocaml in
                                     decode_string_of_js
                                       (Js.Unsafe.meth_call __js_obj
                                          "getDefaultString"
                                          [|(Js.Unsafe.inject
                                               (let open Js_of_ocaml.Js in
                                                  Unsafe.obj
                                                    [|("str",
                                                        (Unsafe.inject
                                                           (encode_string_to_js
                                                              (str |>
                                                                 (Option.value
                                                                    ~default:"Hello")))))|]))|])
                                 [@@warning "-16"]
                                 and get_default_student ?student  =
                                   let open Js_of_ocaml in
                                     decode_string_of_js
                                       (Js.Unsafe.meth_call __js_obj
                                          "getDefaultStudent"
                                          [|(Js.Unsafe.inject
                                               (let open Js_of_ocaml.Js in
                                                  Unsafe.obj
                                                    [|("student",
                                                        (Unsafe.inject
                                                           ((let open Js_of_ocaml in
                                                               fun x ->
                                                                 ((((ex_record_student_to_json
                                                                    x) |>
                                                                    Kxclib.Json.to_yojson)
                                                                    |>
                                                                    Yojson.Safe.to_string)
                                                                    |>
                                                                    Js.string)
                                                                   |>
                                                                   (fun x ->
                                                                    (Js._JSON
                                                                    ## parse)
                                                                    x))
                                                              (student |>
                                                                 (Option.value
                                                                    ~default:
                                                                    {
                                                                    name =
                                                                    "William Gibson";
                                                                    admission_year
                                                                    = 1984
                                                                    })))))|]))|])
                                 [@@warning "-16"]
                               end) : (module with_default_value))[@@warning
                                                                    "-39"]
                             let rec decode_sole_var_of_js __js_obj =
                               let open Js_of_ocaml in
                                 decode_string_of_js
                                   (Js.Unsafe.fun_call __js_obj [||])
                             [@@warning "-39"]
                             and decode_rec_obj_of_js __js_obj =
                               object
                                 method name =
                                   let open Js_of_ocaml in
                                     decode_string_of_js
                                       (Js.Unsafe.meth_call __js_obj "name"
                                          [||])
                                 method get_self =
                                   let open Js_of_ocaml in
                                     (wrap_peer decode_rec_obj_of_js)
                                       (Js.Unsafe.meth_call __js_obj
                                          "getSelf" [||])
                               end[@@warning "-39"]
                             module Peer_object_registry =
                               struct
                                 open Bindoj_runtime
                                 let registry_of_string = ref StringMap.empty
                                 let lookup_string ~id0  ~id1  =
                                   (!registry_of_string) |>
                                     (StringMap.find_opt
                                        (let open Map_key in
                                           String.concat ""
                                             [encode_map_key
                                                ~check_type:`string
                                                (Mk_string id0);
                                             encode_map_key
                                               ~check_type:`int53p
                                               (Mk_int53 id1)]))
                                 and register_string ~id0  ~id1  value =
                                   registry_of_string :=
                                     ((!registry_of_string) |>
                                        (StringMap.update
                                           (let open Map_key in
                                              String.concat ""
                                                [encode_map_key
                                                   ~check_type:`string
                                                   (Mk_string id0);
                                                encode_map_key
                                                  ~check_type:`int53p
                                                  (Mk_int53 id1)])
                                           (fun _ -> value)))
                                 let registry_of_hello = ref StringMap.empty
                                 let lookup_hello ~id  =
                                   (!registry_of_hello) |>
                                     (StringMap.find_opt
                                        (let open Map_key in
                                           encode_map_key ~check_type:`string
                                             (Mk_string id)))
                                 and register_hello ~id  value =
                                   registry_of_hello :=
                                     ((!registry_of_hello) |>
                                        (StringMap.update
                                           (let open Map_key in
                                              encode_map_key
                                                ~check_type:`string
                                                (Mk_string id))
                                           (fun _ -> value)))
                                 let () =
                                   let open Js_of_ocaml in
                                     let js_registry =
                                       Js.Unsafe.get
                                         (Js.Unsafe.get peer_full_bridge
                                            "instance")
                                         "endemicObjectRegistry" in
                                     ((Js.Unsafe.get
                                         (Js.Unsafe.get js_registry "string")
                                         "initialObjects")
                                        |> Js.to_array)
                                       |>
                                       (Array.iter
                                          (fun x ->
                                             let coordinate =
                                               Js.Unsafe.get x "coordinate" in
                                             match Js.to_string
                                                     (Js.Unsafe.get x "kind")
                                             with
                                             | "register" ->
                                                 register_string
                                                   ~id0:(decode_string_of_js
                                                           (Js.Unsafe.get
                                                              coordinate
                                                              "id0"))
                                                   ~id1:(decode_int53p_of_js
                                                           (Js.Unsafe.get
                                                              coordinate
                                                              "id1"))
                                                   (Some
                                                      (decode_string_of_js
                                                         (Js.Unsafe.get x
                                                            "value")))
                                             | "deregister" ->
                                                 register_string
                                                   ~id0:(decode_string_of_js
                                                           (Js.Unsafe.get
                                                              coordinate
                                                              "id0"))
                                                   ~id1:(decode_int53p_of_js
                                                           (Js.Unsafe.get
                                                              coordinate
                                                              "id1")) None
                                             | kind ->
                                                 failwith
                                                   ("unexpected kind: " ^
                                                      kind)));
                                     ((Js.Unsafe.get
                                         (Js.Unsafe.get js_registry "hello")
                                         "initialObjects")
                                        |> Js.to_array)
                                       |>
                                       (Array.iter
                                          (fun x ->
                                             let coordinate =
                                               Js.Unsafe.get x "coordinate" in
                                             match Js.to_string
                                                     (Js.Unsafe.get x "kind")
                                             with
                                             | "register" ->
                                                 register_hello
                                                   ~id:(decode_string_of_js
                                                          (Js.Unsafe.get
                                                             coordinate "id"))
                                                   (Some
                                                      ((wrap_peer
                                                          decode_hello_of_js)
                                                         (Js.Unsafe.get x
                                                            "value")))
                                             | "deregister" ->
                                                 register_hello
                                                   ~id:(decode_string_of_js
                                                          (Js.Unsafe.get
                                                             coordinate "id"))
                                                   None
                                             | kind ->
                                                 failwith
                                                   ("unexpected kind: " ^
                                                      kind)));
                                     Js.Unsafe.set
                                       (Js.Unsafe.get peer_full_bridge
                                          "instance") "endemicObjectRegistry"
                                       (let open Js_of_ocaml.Js in
                                          Unsafe.obj
                                            [|("string",
                                                (Unsafe.inject
                                                   (let open Js_of_ocaml.Js in
                                                      Unsafe.obj
                                                        [|("register",
                                                            (Unsafe.inject
                                                               (Unsafe.callback_with_arity
                                                                  2
                                                                  (fun
                                                                    coordinate
                                                                    ->
                                                                    fun value
                                                                    ->
                                                                    register_string
                                                                    ~id0:(
                                                                    decode_string_of_js
                                                                    (Js.Unsafe.get
                                                                    coordinate
                                                                    "id0"))
                                                                    ~id1:(
                                                                    decode_int53p_of_js
                                                                    (Js.Unsafe.get
                                                                    coordinate
                                                                    "id1"))
                                                                    (Some
                                                                    (decode_string_of_js
                                                                    value))))));
                                                          ("deregister",
                                                            (Unsafe.inject
                                                               (Unsafe.callback
                                                                  (fun
                                                                    coordinate
                                                                    ->
                                                                    register_string
                                                                    ~id0:(
                                                                    decode_string_of_js
                                                                    (Js.Unsafe.get
                                                                    coordinate
                                                                    "id0"))
                                                                    ~id1:(
                                                                    decode_int53p_of_js
                                                                    (Js.Unsafe.get
                                                                    coordinate
                                                                    "id1"))
                                                                    None))))|])));
                                              ("hello",
                                                (Unsafe.inject
                                                   (let open Js_of_ocaml.Js in
                                                      Unsafe.obj
                                                        [|("register",
                                                            (Unsafe.inject
                                                               (Unsafe.callback_with_arity
                                                                  2
                                                                  (fun
                                                                    coordinate
                                                                    ->
                                                                    fun value
                                                                    ->
                                                                    register_hello
                                                                    ~id:(
                                                                    decode_string_of_js
                                                                    (Js.Unsafe.get
                                                                    coordinate
                                                                    "id"))
                                                                    (Some
                                                                    ((wrap_peer
                                                                    decode_hello_of_js)
                                                                    value))))));
                                                          ("deregister",
                                                            (Unsafe.inject
                                                               (Unsafe.callback
                                                                  (fun
                                                                    coordinate
                                                                    ->
                                                                    register_hello
                                                                    ~id:(
                                                                    decode_string_of_js
                                                                    (Js.Unsafe.get
                                                                    coordinate
                                                                    "id"))
                                                                    None))))|])))|])
                               end
                             module Peer_objects =
                               struct
                                 let my_string =
                                   let open Js_of_ocaml in
                                     (Js.Unsafe.get
                                        (Js.Unsafe.get
                                           (Js.Unsafe.get peer_full_bridge
                                              "instance") "endemicObjects")
                                        "myString")
                                       |> decode_string_of_js
                                 and my_hello =
                                   let open Js_of_ocaml in
                                     (Js.Unsafe.get
                                        (Js.Unsafe.get
                                           (Js.Unsafe.get peer_full_bridge
                                              "instance") "endemicObjects")
                                        "myHello")
                                       |> (wrap_peer decode_hello_of_js)
                                 and my_sole_var =
                                   let open Js_of_ocaml in
                                     (Js.Unsafe.get
                                        (Js.Unsafe.get
                                           (Js.Unsafe.get peer_full_bridge
                                              "instance") "endemicObjects")
                                        "mySoleVar")
                                       |> (wrap_peer decode_sole_var_of_js)
                                 and my_unit_sole =
                                   let open Js_of_ocaml in
                                     (Js.Unsafe.get
                                        (Js.Unsafe.get
                                           (Js.Unsafe.get peer_full_bridge
                                              "instance") "endemicObjects")
                                        "myUnitSole")
                                       |> (wrap_peer decode_unit_sole_of_js)
                                 and my_unit_obj =
                                   let open Js_of_ocaml in
                                     (Js.Unsafe.get
                                        (Js.Unsafe.get
                                           (Js.Unsafe.get peer_full_bridge
                                              "instance") "endemicObjects")
                                        "myUnitObj")
                                       |> (wrap_peer decode_unit_obj_of_js)
                                 and my_unit_mod =
                                   let open Js_of_ocaml in
                                     (Js.Unsafe.get
                                        (Js.Unsafe.get
                                           (Js.Unsafe.get peer_full_bridge
                                              "instance") "endemicObjects")
                                        "myUnitMod")
                                       |> (wrap_peer decode_unit_mod_of_js)
                                 and my_rec_obj =
                                   let open Js_of_ocaml in
                                     (Js.Unsafe.get
                                        (Js.Unsafe.get
                                           (Js.Unsafe.get peer_full_bridge
                                              "instance") "endemicObjects")
                                        "myRecObj")
                                       |> (wrap_peer decode_rec_obj_of_js)
                                 and my_non_json_values =
                                   let open Js_of_ocaml in
                                     (Js.Unsafe.get
                                        (Js.Unsafe.get
                                           (Js.Unsafe.get peer_full_bridge
                                              "instance") "endemicObjects")
                                        "myNonJsonValues")
                                       |>
                                       (let open Utils in
                                          decode_non_json_values_of_js)
                                 and with_default_value =
                                   let open Js_of_ocaml in
                                     (Js.Unsafe.get
                                        (Js.Unsafe.get
                                           (Js.Unsafe.get peer_full_bridge
                                              "instance") "endemicObjects")
                                        "withDefaultValue")
                                       |>
                                       (wrap_peer
                                          decode_with_default_value_of_js)
                               end
                           end) : (module Bridge)) in
                         bridge_opt := (Some concrete_bridge);
                         (!continuations) |>
                           (List.iter (fun f -> f concrete_bridge));
                         continuations := [])))))|])
        |>
        (fun x ->
           (Obj.magic x : Bindoj_objintf_shared.endemic_full_bridge_reference))
  end 