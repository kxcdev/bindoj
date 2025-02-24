type ex_coretype_various_prim_types = {
  unit : unit;
  bool : bool;
  int : int;
  float : float;
  string : string;
  uchar : Uchar.t;
  byte : char;
  bytes : Bytes.t;
}

let rec (ex_coretype_various_prim_types_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { unit; bool; int; float; string; uchar; byte; bytes } ->
             StringMap.of_list
               [
                 ("unit", Expr.of_unit unit);
                 ("bool", Expr.of_bool bool);
                 ("int", Expr.of_int int);
                 ("float", Expr.of_float float);
                 ("string", Expr.of_string string);
                 ("uchar", Expr.of_uchar uchar);
                 ("byte", Expr.of_byte byte);
                 ("bytes", Expr.of_bytes bytes);
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "unit" >>= Expr.to_unit >>= fun unit ->
             xs |> StringMap.find_opt "bool" >>= Expr.to_bool >>= fun bool ->
             xs |> StringMap.find_opt "int" >>= Expr.to_int >>= fun int ->
             xs |> StringMap.find_opt "float" >>= Expr.to_float >>= fun float ->
             xs |> StringMap.find_opt "string" >>= Expr.to_string
             >>= fun string ->
             xs |> StringMap.find_opt "uchar" >>= Expr.to_uchar >>= fun uchar ->
             xs |> StringMap.find_opt "byte" >>= Expr.to_byte >>= fun byte ->
             xs |> StringMap.find_opt "bytes" >>= Expr.to_bytes >>= fun bytes ->
             Some { unit; bool; int; float; string; uchar; byte; bytes });
       })
[@@warning "-33-39"]

let ex_coretype_various_prim_types_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExCoretypeVariousPrimTypes",
           `object_of
             [
               `mandatory_field ("unit", `special ("unit", `exactly `null));
               `mandatory_field ("bool", `boolean);
               `mandatory_field ("int", `integral);
               `mandatory_field ("float", `proper_float);
               `mandatory_field ("string", `string);
               `mandatory_field ("uchar", `special ("uchar", `string));
               `mandatory_field ("byte", `special ("byte", `string));
               `mandatory_field ("bytes", `base64str);
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_coretype_various_prim_types_to_json =
  (let unit_to_json () = (`num 1. : Kxclib.Json.jv)
   and uchar_to_json (x : Uchar.t) =
     (`str (String.of_seq (List.to_seq [ Uchar.to_char x ])) : Kxclib.Json.jv)
   and string_to_json (x : string) = (`str x : Kxclib.Json.jv)
   and int_to_json (x : int) = (`num (float_of_int x) : Kxclib.Json.jv)
   and float_to_json (x : float) = (`num x : Kxclib.Json.jv)
   and bytes_to_json (x : Bytes.t) =
     (`str (Kxclib.Base64.encode x) : Kxclib.Json.jv)
   and byte_to_json (x : char) =
     (`num (float_of_int (int_of_char x)) : Kxclib.Json.jv)
   and bool_to_json (x : bool) = (`bool x : Kxclib.Json.jv) in
   fun {
         unit = x0;
         bool = x1;
         int = x2;
         float = x3;
         string = x4;
         uchar = x5;
         byte = x6;
         bytes = x7;
       } ->
     `obj
       [
         ("unit", unit_to_json x0);
         ("bool", bool_to_json x1);
         ("int", int_to_json x2);
         ("float", float_to_json x3);
         ("string", string_to_json x4);
         ("uchar", uchar_to_json x5);
         ("byte", byte_to_json x6);
         ("bytes", bytes_to_json x7);
       ]
    : ex_coretype_various_prim_types -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_coretype_various_prim_types_of_json' =
  (fun ?(path = []) ->
     fun x ->
      (let rec of_json_impl =
         let unit_of_json' path = function
           | `bool _ | `num _ | `str _ | `arr [] | `obj [] -> Ok ()
           | jv ->
               Error
                 ( Printf.sprintf
                     "expecting type 'unit' but the given is of type '%s'"
                     (let open Kxclib.Json in
                      string_of_jv_kind (classify_jv jv)),
                   path )
         and uchar_of_json' path = function
           | (`str x : Kxclib.Json.jv) ->
               if String.length x = 1 then Ok (Uchar.of_char x.[0])
               else
                 Error
                   ( Printf.sprintf "string '%s' is not a valid uchar value" x,
                     path )
           | jv ->
               Error
                 ( Printf.sprintf
                     "expecting type 'uchar' but the given is of type '%s'"
                     (let open Kxclib.Json in
                      string_of_jv_kind (classify_jv jv)),
                   path )
         and string_of_json' path = function
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
         and float_of_json' path = function
           | (`num x : Kxclib.Json.jv) -> Ok x
           | jv ->
               Error
                 ( Printf.sprintf
                     "expecting type 'float' but the given is of type '%s'"
                     (let open Kxclib.Json in
                      string_of_jv_kind (classify_jv jv)),
                   path )
         and bytes_of_json' path = function
           | (`str x : Kxclib.Json.jv) -> (
               try Ok (Kxclib.Base64.decode x)
               with Invalid_argument msg -> Error (msg, path))
           | jv ->
               Error
                 ( Printf.sprintf
                     "expecting type 'bytes' but the given is of type '%s'"
                     (let open Kxclib.Json in
                      string_of_jv_kind (classify_jv jv)),
                   path )
         and byte_of_json' path = function
           | (`num x : Kxclib.Json.jv) ->
               let x = int_of_float x in
               if 0 <= x && x <= 255 then Ok (char_of_int x)
               else
                 Error
                   ( Printf.sprintf "number '%d' is not a valid byte value" x,
                     path )
           | jv ->
               Error
                 ( Printf.sprintf
                     "expecting type 'byte' but the given is of type '%s'"
                     (let open Kxclib.Json in
                      string_of_jv_kind (classify_jv jv)),
                   path )
         and bool_of_json' path = function
           | (`bool x : Kxclib.Json.jv) -> Ok x
           | jv ->
               Error
                 ( Printf.sprintf
                     "expecting type 'bool' but the given is of type '%s'"
                     (let open Kxclib.Json in
                      string_of_jv_kind (classify_jv jv)),
                   path )
         in
         fun path ->
           fun __bindoj_orig ->
            match __bindoj_orig with
            | `obj param ->
                let ( >>= ) = Result.bind in
                List.assoc_opt "unit" param
                |> Option.to_result
                     ~none:("mandatory field 'unit' does not exist", path)
                >>= unit_of_json' (`f "unit" :: path)
                >>= fun x0 ->
                List.assoc_opt "bool" param
                |> Option.to_result
                     ~none:("mandatory field 'bool' does not exist", path)
                >>= bool_of_json' (`f "bool" :: path)
                >>= fun x1 ->
                List.assoc_opt "int" param
                |> Option.to_result
                     ~none:("mandatory field 'int' does not exist", path)
                >>= int_of_json' (`f "int" :: path)
                >>= fun x2 ->
                List.assoc_opt "float" param
                |> Option.to_result
                     ~none:("mandatory field 'float' does not exist", path)
                >>= float_of_json' (`f "float" :: path)
                >>= fun x3 ->
                List.assoc_opt "string" param
                |> Option.to_result
                     ~none:("mandatory field 'string' does not exist", path)
                >>= string_of_json' (`f "string" :: path)
                >>= fun x4 ->
                List.assoc_opt "uchar" param
                |> Option.to_result
                     ~none:("mandatory field 'uchar' does not exist", path)
                >>= uchar_of_json' (`f "uchar" :: path)
                >>= fun x5 ->
                List.assoc_opt "byte" param
                |> Option.to_result
                     ~none:("mandatory field 'byte' does not exist", path)
                >>= byte_of_json' (`f "byte" :: path)
                >>= fun x6 ->
                List.assoc_opt "bytes" param
                |> Option.to_result
                     ~none:("mandatory field 'bytes' does not exist", path)
                >>= bytes_of_json' (`f "bytes" :: path)
                >>= fun x7 ->
                Ok
                  {
                    unit = x0;
                    bool = x1;
                    int = x2;
                    float = x3;
                    string = x4;
                    uchar = x5;
                    byte = x6;
                    bytes = x7;
                  }
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
             (msg, path, ex_coretype_various_prim_types_json_shape_explanation))
    : ex_coretype_various_prim_types Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_coretype_various_prim_types_of_json =
  (fun x -> ex_coretype_various_prim_types_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_coretype_various_prim_types option)
[@@warning "-39"]

let ex_coretype_various_prim_types_decl =
  Bindoj_test_common_typedesc_examples.Ex_coretype.Various_prim_types.decl

let ex_coretype_various_prim_types_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_coretype.Various_prim_types.decl
    ex_coretype_various_prim_types_reflect

type ex_coretype_with_int53p = { value : Kxclib.int53p }

let rec (ex_coretype_with_int53p_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { value } ->
             StringMap.of_list [ ("value", Expr.of_int53p value) ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "value" >>= Expr.to_int53p
             >>= fun value -> Some { value });
       })
[@@warning "-33-39"]

let ex_coretype_with_int53p_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExCoretypeWithInt53p",
           `object_of [ `mandatory_field ("value", `proper_int53p) ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_coretype_with_int53p_to_json =
  (let int53p_to_json (x : Kxclib.int53p) =
     (`num (Kxclib.Int53p.to_float x) : Kxclib.Json.jv)
   in
   fun { value = x0 } -> `obj [ ("value", int53p_to_json x0) ]
    : ex_coretype_with_int53p -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_coretype_with_int53p_of_json' =
  (fun ?(path = []) ->
     fun x ->
      (let rec of_json_impl =
         let int53p_of_json' path = function
           | (`num x : Kxclib.Json.jv) -> Ok (Kxclib.Int53p.of_float x)
           | jv ->
               Error
                 ( Printf.sprintf
                     "expecting type 'int53p' but the given is of type '%s'"
                     (let open Kxclib.Json in
                      string_of_jv_kind (classify_jv jv)),
                   path )
         in
         fun path ->
           fun __bindoj_orig ->
            match __bindoj_orig with
            | `obj param ->
                let ( >>= ) = Result.bind in
                List.assoc_opt "value" param
                |> Option.to_result
                     ~none:("mandatory field 'value' does not exist", path)
                >>= int53p_of_json' (`f "value" :: path)
                >>= fun x0 -> Ok { value = x0 }
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
             (msg, path, ex_coretype_with_int53p_json_shape_explanation))
    : ex_coretype_with_int53p Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_coretype_with_int53p_of_json =
  (fun x -> ex_coretype_with_int53p_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_coretype_with_int53p option)
[@@warning "-39"]

let ex_coretype_with_int53p_decl =
  Bindoj_test_common_typedesc_examples.Ex_coretype.With_int53p.decl

let ex_coretype_with_int53p_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_coretype.With_int53p.decl
    ex_coretype_with_int53p_reflect

type ex_coretype_various_complex_types = {
  option : int option;
  list : int list;
  map : (string * int) list;
}

let rec (ex_coretype_various_complex_types_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { option; list; map } ->
             StringMap.of_list
               [
                 ("option", (Expr.of_option Expr.of_int) option);
                 ("list", (Expr.of_list Expr.of_int) list);
                 ("map", (Expr.of_map Expr.of_int) map);
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "option" >>= Expr.to_option Expr.to_int
             >>= fun option ->
             xs |> StringMap.find_opt "list" >>= Expr.to_list Expr.to_int
             >>= fun list ->
             xs |> StringMap.find_opt "map" >>= Expr.to_map Expr.to_int
             >>= fun map -> Some { option; list; map });
       })
[@@warning "-33-39"]

let ex_coretype_various_complex_types_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExCoretypeVariousComplexTypes",
           `object_of
             [
               `optional_field ("option", `integral);
               `mandatory_field ("list", `array_of `integral);
               `mandatory_field ("map", `record_of `integral);
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_coretype_various_complex_types_to_json =
  (let map_to_json key_to_string v_to_json fields =
     let fields =
       fields |> List.map (fun (k, v) -> (key_to_string k, v_to_json v))
     in
     (`obj fields : Kxclib.Json.jv)
   and list_to_json t_to_json xs =
     (`arr (List.map t_to_json xs) : Kxclib.Json.jv)
   and int_to_json (x : int) = (`num (float_of_int x) : Kxclib.Json.jv) in
   fun { option = x0; list = x1; map = x2 } ->
     `obj
       (List.filter_map
          (fun x -> x)
          [ Option.map (fun x0 -> ("option", int_to_json x0)) x0 ]
       @ [
           ("list", (list_to_json int_to_json) x1);
           ("map", (map_to_json (fun (k : string) -> k) int_to_json) x2);
         ])
    : ex_coretype_various_complex_types -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_coretype_various_complex_types_of_json' =
  (fun ?(path = []) ->
     fun x ->
      (let rec of_json_impl =
         let option_of_json' t_of_json path = function
           | `null -> Ok None
           | x -> (
               match t_of_json path x with
               | Ok x -> Ok (Some x)
               | Error msg -> Error msg)
         and map_of_json' key_of_string v_of_json path = function
           | `obj fields ->
               let open Kxclib.MonadOps (Kxclib.ResultOf (struct
                 type err = string * Kxclib.Json.jvpath
               end)) in
               fields
               |> List.map (fun (k, v) ->
                      match (key_of_string k, v_of_json (`f k :: path) v) with
                      | Some k, Ok v -> Ok (k, v)
                      | None, _ ->
                          Error
                            (Printf.sprintf "'key_of_string %s' failed" k, path)
                      | _, Error x -> Error x)
               |> sequence_list
           | jv ->
               Error
                 ( Printf.sprintf
                     "expecting type 'map' but the given is of type '%s'"
                     (let open Kxclib.Json in
                      string_of_jv_kind (classify_jv jv)),
                   path )
         and list_of_json' t_of_json path = function
           | (`arr xs : Kxclib.Json.jv) ->
               let open Kxclib.MonadOps (Kxclib.ResultOf (struct
                 type err = string * Kxclib.Json.jvpath
               end)) in
               xs
               |> List.mapi (fun i -> t_of_json (`i i :: path))
               |> sequence_list
           | jv ->
               Error
                 ( Printf.sprintf
                     "expecting type 'list' but the given is of type '%s'"
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
                List.assoc_opt "option" param
                |> Option.value ~default:`null
                |> (option_of_json' int_of_json') (`f "option" :: path)
                >>= fun x0 ->
                List.assoc_opt "list" param
                |> Option.to_result
                     ~none:("mandatory field 'list' does not exist", path)
                >>= (list_of_json' int_of_json') (`f "list" :: path)
                >>= fun x1 ->
                List.assoc_opt "map" param
                |> Option.to_result
                     ~none:("mandatory field 'map' does not exist", path)
                >>= (map_of_json' (fun (s : string) -> Some s) int_of_json')
                      (`f "map" :: path)
                >>= fun x2 -> Ok { option = x0; list = x1; map = x2 }
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
             ( msg,
               path,
               ex_coretype_various_complex_types_json_shape_explanation ))
    : ex_coretype_various_complex_types Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_coretype_various_complex_types_of_json =
  (fun x -> ex_coretype_various_complex_types_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_coretype_various_complex_types option)
[@@warning "-39"]

let ex_coretype_various_complex_types_decl =
  Bindoj_test_common_typedesc_examples.Ex_coretype.Various_complex_types.decl

let ex_coretype_various_complex_types_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_coretype.Various_complex_types.decl
    ex_coretype_various_complex_types_reflect

type ex_coretype_various_tuple_types = {
  tuple : int * int;
  objtuple : int * int;
  nested : int option * int list * (int * int);
}

let rec (ex_coretype_various_tuple_types_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { tuple; objtuple; nested } ->
             StringMap.of_list
               [
                 ( "tuple",
                   (fun (x0, x1) ->
                     Expr.Tuple [ Expr.of_int x0; Expr.of_int x1 ])
                     tuple );
                 ( "objtuple",
                   (fun (x0, x1) ->
                     Expr.Tuple [ Expr.of_int x0; Expr.of_int x1 ])
                     objtuple );
                 ( "nested",
                   (fun (x0, x1, x2) ->
                     Expr.Tuple
                       [
                         (Expr.of_option Expr.of_int) x0;
                         (Expr.of_list Expr.of_int) x1;
                         (fun (x0, x1) ->
                           Expr.Tuple [ Expr.of_int x0; Expr.of_int x1 ])
                           x2;
                       ])
                     nested );
               ]);
         mk =
           (fun xs ->
             ( xs |> StringMap.find_opt "tuple" >>= function
               | Expr.Tuple [ x0; x1 ] ->
                   Expr.to_int x0 >>= fun x0 ->
                   Expr.to_int x1 >>= fun x1 -> Some (x0, x1)
               | _ -> None )
             >>= fun tuple ->
             ( xs |> StringMap.find_opt "objtuple" >>= function
               | Expr.Tuple [ x0; x1 ] ->
                   Expr.to_int x0 >>= fun x0 ->
                   Expr.to_int x1 >>= fun x1 -> Some (x0, x1)
               | _ -> None )
             >>= fun objtuple ->
             ( xs |> StringMap.find_opt "nested" >>= function
               | Expr.Tuple [ x0; x1; x2 ] ->
                   (Expr.to_option Expr.to_int) x0 >>= fun x0 ->
                   (Expr.to_list Expr.to_int) x1 >>= fun x1 ->
                   (function
                     | Expr.Tuple [ x0; x1 ] ->
                         Expr.to_int x0 >>= fun x0 ->
                         Expr.to_int x1 >>= fun x1 -> Some (x0, x1)
                     | _ -> None)
                     x2
                   >>= fun x2 -> Some (x0, x1, x2)
               | _ -> None )
             >>= fun nested -> Some { tuple; objtuple; nested });
       })
[@@warning "-33-39"]

let ex_coretype_various_tuple_types_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExCoretypeVariousTupleTypes",
           `object_of
             [
               `mandatory_field ("tuple", `tuple_of [ `integral; `integral ]);
               `mandatory_field
                 ( "objtuple",
                   `object_of
                     [
                       `mandatory_field ("_0", `integral);
                       `mandatory_field ("_1", `integral);
                     ] );
               `mandatory_field
                 ( "nested",
                   `tuple_of
                     [
                       `nullable `integral;
                       `array_of `integral;
                       `tuple_of [ `integral; `integral ];
                     ] );
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_coretype_various_tuple_types_to_json =
  (let option_to_json t_to_json = function
     | Some x -> t_to_json x
     | None -> (`null : Kxclib.Json.jv)
   and list_to_json t_to_json xs =
     (`arr (List.map t_to_json xs) : Kxclib.Json.jv)
   and int_to_json (x : int) = (`num (float_of_int x) : Kxclib.Json.jv) in
   fun { tuple = x0; objtuple = x1; nested = x2 } ->
     `obj
       [
         ( "tuple",
           (fun (x0, x1) ->
             (`arr [ int_to_json x0; int_to_json x1 ] : Kxclib.Json.jv))
             x0 );
         ( "objtuple",
           (fun (x0, x1) ->
             (`obj [ ("_0", int_to_json x0); ("_1", int_to_json x1) ]
               : Kxclib.Json.jv))
             x1 );
         ( "nested",
           (fun (x0, x1, x2) ->
             (`arr
                [
                  (option_to_json int_to_json) x0;
                  (list_to_json int_to_json) x1;
                  (fun (x0, x1) ->
                    (`arr [ int_to_json x0; int_to_json x1 ] : Kxclib.Json.jv))
                    x2;
                ]
               : Kxclib.Json.jv))
             x2 );
       ]
    : ex_coretype_various_tuple_types -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_coretype_various_tuple_types_of_json' =
  (fun ?(path = []) ->
     fun x ->
      (let rec of_json_impl =
         let option_of_json' t_of_json path = function
           | `null -> Ok None
           | x -> (
               match t_of_json path x with
               | Ok x -> Ok (Some x)
               | Error msg -> Error msg)
         and list_of_json' t_of_json path = function
           | (`arr xs : Kxclib.Json.jv) ->
               let open Kxclib.MonadOps (Kxclib.ResultOf (struct
                 type err = string * Kxclib.Json.jvpath
               end)) in
               xs
               |> List.mapi (fun i -> t_of_json (`i i :: path))
               |> sequence_list
           | jv ->
               Error
                 ( Printf.sprintf
                     "expecting type 'list' but the given is of type '%s'"
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
                List.assoc_opt "tuple" param
                |> Option.to_result
                     ~none:("mandatory field 'tuple' does not exist", path)
                >>= (fun path -> function
                      | (`arr [ x0; x1 ] : Kxclib.Json.jv) ->
                          let ( >>= ) = Result.bind in
                          int_of_json' (`i 0 :: path) x0 >>= fun x0 ->
                          int_of_json' (`i 1 :: path) x1 >>= fun x1 ->
                          Ok (x0, x1)
                      | `arr xs ->
                          Error
                            ( Printf.sprintf
                                "expecting a tuple of length 2, but the given \
                                 has a length of %d"
                                (List.length xs),
                              path )
                      | jv ->
                          Error
                            ( Printf.sprintf
                                "an array is expected for a tuple value, but \
                                 the given is of type '%s'"
                                (let open Kxclib.Json in
                                 string_of_jv_kind (classify_jv jv)),
                              path ))
                      (`f "tuple" :: path)
                >>= fun x0 ->
                List.assoc_opt "objtuple" param
                |> Option.to_result
                     ~none:("mandatory field 'objtuple' does not exist", path)
                >>= (fun path -> function
                      | (`obj fields : Kxclib.Json.jv) ->
                          let ( >>= ) = Result.bind in
                          List.assoc_opt "_0" fields
                          |> Option.to_result
                               ~none:
                                 ("mandatory field '_0' does not exist", path)
                          >>= int_of_json' (`f "_0" :: path)
                          >>= fun x0 ->
                          List.assoc_opt "_1" fields
                          |> Option.to_result
                               ~none:
                                 ("mandatory field '_1' does not exist", path)
                          >>= int_of_json' (`f "_1" :: path)
                          >>= fun x1 -> Ok (x0, x1)
                      | jv ->
                          Error
                            ( Printf.sprintf
                                "an object is expected for a tuple value, but \
                                 the given is of type '%s'"
                                (let open Kxclib.Json in
                                 string_of_jv_kind (classify_jv jv)),
                              path ))
                      (`f "objtuple" :: path)
                >>= fun x1 ->
                List.assoc_opt "nested" param
                |> Option.to_result
                     ~none:("mandatory field 'nested' does not exist", path)
                >>= (fun path -> function
                      | (`arr [ x0; x1; x2 ] : Kxclib.Json.jv) ->
                          let ( >>= ) = Result.bind in
                          (option_of_json' int_of_json') (`i 0 :: path) x0
                          >>= fun x0 ->
                          (list_of_json' int_of_json') (`i 1 :: path) x1
                          >>= fun x1 ->
                          (fun path -> function
                            | (`arr [ x0; x1 ] : Kxclib.Json.jv) ->
                                let ( >>= ) = Result.bind in
                                int_of_json' (`i 0 :: path) x0 >>= fun x0 ->
                                int_of_json' (`i 1 :: path) x1 >>= fun x1 ->
                                Ok (x0, x1)
                            | `arr xs ->
                                Error
                                  ( Printf.sprintf
                                      "expecting a tuple of length 2, but the \
                                       given has a length of %d"
                                      (List.length xs),
                                    path )
                            | jv ->
                                Error
                                  ( Printf.sprintf
                                      "an array is expected for a tuple value, \
                                       but the given is of type '%s'"
                                      (let open Kxclib.Json in
                                       string_of_jv_kind (classify_jv jv)),
                                    path ))
                            (`i 2 :: path) x2
                          >>= fun x2 -> Ok (x0, x1, x2)
                      | `arr xs ->
                          Error
                            ( Printf.sprintf
                                "expecting a tuple of length 3, but the given \
                                 has a length of %d"
                                (List.length xs),
                              path )
                      | jv ->
                          Error
                            ( Printf.sprintf
                                "an array is expected for a tuple value, but \
                                 the given is of type '%s'"
                                (let open Kxclib.Json in
                                 string_of_jv_kind (classify_jv jv)),
                              path ))
                      (`f "nested" :: path)
                >>= fun x2 -> Ok { tuple = x0; objtuple = x1; nested = x2 }
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
             (msg, path, ex_coretype_various_tuple_types_json_shape_explanation))
    : ex_coretype_various_tuple_types Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_coretype_various_tuple_types_of_json =
  (fun x -> ex_coretype_various_tuple_types_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_coretype_various_tuple_types option)
[@@warning "-39"]

let ex_coretype_various_tuple_types_decl =
  Bindoj_test_common_typedesc_examples.Ex_coretype.Various_tuple_types.decl

let ex_coretype_various_tuple_types_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_coretype.Various_tuple_types.decl
    ex_coretype_various_tuple_types_reflect

type ex_coretype_named_json = {
  name : string;
  json : Bindoj_std_runtime.Json_value.t;
}

let rec (ex_coretype_named_json_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { name; json } ->
             StringMap.of_list
               [
                 ("name", Expr.of_string name);
                 ( "json",
                   (Expr.of_refl Bindoj_std_runtime.Json_value.reflect) json );
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "name" >>= Expr.to_string >>= fun name ->
             xs |> StringMap.find_opt "json"
             >>= Expr.to_refl Bindoj_std_runtime.Json_value.reflect
             >>= fun json -> Some { name; json });
       })
[@@warning "-33-39"]

let ex_coretype_named_json_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExCoretypeNamedJson",
           `object_of
             [
               `mandatory_field ("name", `string);
               `mandatory_field ("json", `named ("json_value", `any_json_value));
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_coretype_named_json_to_json =
  (let string_to_json (x : string) = (`str x : Kxclib.Json.jv) in
   fun { name = x0; json = x1 } ->
     `obj
       [
         ("name", string_to_json x0);
         ("json", Bindoj_std_runtime.Json_value.to_json x1);
       ]
    : ex_coretype_named_json -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_coretype_named_json_of_json' =
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
         in
         fun path ->
           fun __bindoj_orig ->
            match __bindoj_orig with
            | `obj param ->
                let ( >>= ) = Result.bind in
                List.assoc_opt "name" param
                |> Option.to_result
                     ~none:("mandatory field 'name' does not exist", path)
                >>= string_of_json' (`f "name" :: path)
                >>= fun x0 ->
                List.assoc_opt "json" param
                |> Option.to_result
                     ~none:("mandatory field 'json' does not exist", path)
                >>= (fun path ->
                      fun x ->
                       Bindoj_std_runtime.Json_value.of_json' ~path x
                       |> Result.map_error (fun (msg, path, _) -> (msg, path)))
                      (`f "json" :: path)
                >>= fun x1 -> Ok { name = x0; json = x1 }
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
             (msg, path, ex_coretype_named_json_json_shape_explanation))
    : ex_coretype_named_json Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_coretype_named_json_of_json =
  (fun x -> ex_coretype_named_json_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_coretype_named_json option)
[@@warning "-39"]

let ex_coretype_named_json_decl =
  Bindoj_test_common_typedesc_examples.Ex_coretype.Named_json.decl

let ex_coretype_named_json_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_coretype.Named_json.decl
    ex_coretype_named_json_reflect
