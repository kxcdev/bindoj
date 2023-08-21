type various_prim_types = {
  unit : unit;
  bool : bool;
  int : int;
  float : float;
  string : string;
  uchar : Uchar.t;
  byte : char;
  bytes : Bytes.t;
}

let rec (various_prim_types_reflect : _ Bindoj_runtime.Refl.t) =
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

let various_prim_types_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "VariousPrimTypes",
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

let rec various_prim_types_to_json =
  (let unit_to_json () : Kxclib.Json.jv = `num 1.
   and uchar_to_json (x : Uchar.t) : Kxclib.Json.jv =
     `str (String.of_seq (List.to_seq [ Uchar.to_char x ]))
   and string_to_json (x : string) : Kxclib.Json.jv = `str x
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x)
   and float_to_json (x : float) : Kxclib.Json.jv = `num x
   and bytes_to_json (x : Bytes.t) : Kxclib.Json.jv =
     `str (Kxclib.Base64.encode x)
   and byte_to_json (x : char) : Kxclib.Json.jv =
     `num (float_of_int (int_of_char x))
   and bool_to_json (x : bool) : Kxclib.Json.jv = `bool x in
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
    : various_prim_types -> Kxclib.Json.jv)
[@@warning "-39"]

and various_prim_types_of_json' =
  (fun ?(path = []) x ->
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
                  ( Printf.sprintf "expecting an integer but the given is '%f'" x,
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
        fun path __bindoj_orig ->
          match __bindoj_orig with
          | `obj param ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "unit" param
              |> (function
                   | Some a -> Ok a
                   | None ->
                       Error ("mandatory field 'unit' does not exist", path))
              >>= unit_of_json' (`f "unit" :: path)
              >>= fun x0 ->
              List.assoc_opt "bool" param
              |> (function
                   | Some a -> Ok a
                   | None ->
                       Error ("mandatory field 'bool' does not exist", path))
              >>= bool_of_json' (`f "bool" :: path)
              >>= fun x1 ->
              List.assoc_opt "int" param
              |> (function
                   | Some a -> Ok a
                   | None -> Error ("mandatory field 'int' does not exist", path))
              >>= int_of_json' (`f "int" :: path)
              >>= fun x2 ->
              List.assoc_opt "float" param
              |> (function
                   | Some a -> Ok a
                   | None ->
                       Error ("mandatory field 'float' does not exist", path))
              >>= float_of_json' (`f "float" :: path)
              >>= fun x3 ->
              List.assoc_opt "string" param
              |> (function
                   | Some a -> Ok a
                   | None ->
                       Error ("mandatory field 'string' does not exist", path))
              >>= string_of_json' (`f "string" :: path)
              >>= fun x4 ->
              List.assoc_opt "uchar" param
              |> (function
                   | Some a -> Ok a
                   | None ->
                       Error ("mandatory field 'uchar' does not exist", path))
              >>= uchar_of_json' (`f "uchar" :: path)
              >>= fun x5 ->
              List.assoc_opt "byte" param
              |> (function
                   | Some a -> Ok a
                   | None ->
                       Error ("mandatory field 'byte' does not exist", path))
              >>= byte_of_json' (`f "byte" :: path)
              >>= fun x6 ->
              List.assoc_opt "bytes" param
              |> (function
                   | Some a -> Ok a
                   | None ->
                       Error ("mandatory field 'bytes' does not exist", path))
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
            (msg, path, various_prim_types_json_shape_explanation))
    : various_prim_types Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and various_prim_types_of_json =
  (fun x -> various_prim_types_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> various_prim_types option)
[@@warning "-39"]

let various_prim_types_decl = Bindoj_test_common_typedesc_examples.Ex06.decl

let various_prim_types_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex06.decl various_prim_types_reflect
