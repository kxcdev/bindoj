type complex_types = {
  option : int option;
  list : int list;
  tuple : int * int;
  objtuple : int * int;
  nested : int option * int list * (int * int);
  map : (string * int) list;
}

let rec (complex_types_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { option; list; tuple; objtuple; nested; map } ->
             StringMap.of_list
               [
                 ("option", (Expr.of_option Expr.of_int) option);
                 ("list", (Expr.of_list Expr.of_int) list);
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
                 ("map", (Expr.of_map Expr.of_int) map);
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "option" >>= Expr.to_option Expr.to_int
             >>= fun option ->
             xs |> StringMap.find_opt "list" >>= Expr.to_list Expr.to_int
             >>= fun list ->
             (xs |> StringMap.find_opt "tuple" >>= function
              | Expr.Tuple [ x0; x1 ] ->
                  Expr.to_int x0 >>= fun x0 ->
                  Expr.to_int x1 >>= fun x1 -> Some (x0, x1)
              | _ -> None)
             >>= fun tuple ->
             (xs |> StringMap.find_opt "objtuple" >>= function
              | Expr.Tuple [ x0; x1 ] ->
                  Expr.to_int x0 >>= fun x0 ->
                  Expr.to_int x1 >>= fun x1 -> Some (x0, x1)
              | _ -> None)
             >>= fun objtuple ->
             (xs |> StringMap.find_opt "nested" >>= function
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
              | _ -> None)
             >>= fun nested ->
             xs |> StringMap.find_opt "map" >>= Expr.to_map Expr.to_int
             >>= fun map -> Some { option; list; tuple; objtuple; nested; map });
       })
[@@warning "-33-39"]

let complex_types_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ComplexTypes",
           `object_of
             [
               `optional_field ("option", `integral);
               `mandatory_field ("list", `array_of `integral);
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
               `mandatory_field ("map", `record_of `integral);
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec complex_types_to_json =
  (let option_to_json t_to_json = function
     | Some x -> t_to_json x
     | None -> (`null : Kxclib.Json.jv)
   and map_to_json key_to_string v_to_json fields =
     let fields =
       fields |> List.map (fun (k, v) -> (key_to_string k, v_to_json v))
     in
     (`obj fields : Kxclib.Json.jv)
   and list_to_json t_to_json xs : Kxclib.Json.jv = `arr (List.map t_to_json xs)
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   fun {
         option = x0;
         list = x1;
         tuple = x2;
         objtuple = x3;
         nested = x4;
         map = x5;
       } ->
     `obj
       (List.filter_map
          (fun x -> x)
          [ Option.map (fun x0 -> ("option", int_to_json x0)) x0 ]
       @ [
           ("list", (list_to_json int_to_json) x1);
           ( "tuple",
             (fun (x0, x1) : Kxclib.Json.jv ->
               `arr [ int_to_json x0; int_to_json x1 ])
               x2 );
           ( "objtuple",
             (fun (x0, x1) : Kxclib.Json.jv ->
               `obj [ ("_0", int_to_json x0); ("_1", int_to_json x1) ])
               x3 );
           ( "nested",
             (fun (x0, x1, x2) : Kxclib.Json.jv ->
               `arr
                 [
                   (option_to_json int_to_json) x0;
                   (list_to_json int_to_json) x1;
                   (fun (x0, x1) : Kxclib.Json.jv ->
                     `arr [ int_to_json x0; int_to_json x1 ])
                     x2;
                 ])
               x4 );
           ("map", (map_to_json (fun (k : string) -> k) int_to_json) x5);
         ])
    : complex_types -> Kxclib.Json.jv)
[@@warning "-39"]

and complex_types_of_json' =
  (fun ?(path = []) x ->
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
                  ( Printf.sprintf "expecting an integer but the given is '%f'" x,
                    path )
          | jv ->
              Error
                ( Printf.sprintf
                    "expecting type 'int' but the given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        in
        fun path __bindoj_orig ->
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
              List.assoc_opt "tuple" param
              |> Option.to_result
                   ~none:("mandatory field 'tuple' does not exist", path)
              >>= (fun path -> function
                    | (`arr [ x0; x1 ] : Kxclib.Json.jv) ->
                        let ( >>= ) = Result.bind in
                        int_of_json' (`i 0 :: path) x0 >>= fun x0 ->
                        int_of_json' (`i 1 :: path) x1 >>= fun x1 -> Ok (x0, x1)
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
                              "an array is expected for a tuple value, but the \
                               given is of type '%s'"
                              (let open Kxclib.Json in
                               string_of_jv_kind (classify_jv jv)),
                            path ))
                    (`f "tuple" :: path)
              >>= fun x2 ->
              List.assoc_opt "objtuple" param
              |> Option.to_result
                   ~none:("mandatory field 'objtuple' does not exist", path)
              >>= (fun path -> function
                    | (`obj fields : Kxclib.Json.jv) ->
                        let ( >>= ) = Result.bind in
                        List.assoc_opt "_0" fields
                        |> Option.to_result
                             ~none:("mandatory field '_0' does not exist", path)
                        >>= int_of_json' (`f "_0" :: path)
                        >>= fun x0 ->
                        List.assoc_opt "_1" fields
                        |> Option.to_result
                             ~none:("mandatory field '_1' does not exist", path)
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
              >>= fun x3 ->
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
                              "an array is expected for a tuple value, but the \
                               given is of type '%s'"
                              (let open Kxclib.Json in
                               string_of_jv_kind (classify_jv jv)),
                            path ))
                    (`f "nested" :: path)
              >>= fun x4 ->
              List.assoc_opt "map" param
              |> Option.to_result
                   ~none:("mandatory field 'map' does not exist", path)
              >>= (map_of_json' (fun (s : string) -> Some s) int_of_json')
                    (`f "map" :: path)
              >>= fun x5 ->
              Ok
                {
                  option = x0;
                  list = x1;
                  tuple = x2;
                  objtuple = x3;
                  nested = x4;
                  map = x5;
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
            (msg, path, complex_types_json_shape_explanation))
    : complex_types Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and complex_types_of_json =
  (fun x -> complex_types_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> complex_types option)
[@@warning "-39"]

let complex_types_decl = Bindoj_test_common_typedesc_examples.Ex05.decl

let complex_types_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex05.decl complex_types_reflect
