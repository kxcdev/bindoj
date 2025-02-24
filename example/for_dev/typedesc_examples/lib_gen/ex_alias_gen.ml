type nonrec ex_alias_unit = unit

let (ex_alias_unit_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Alias { get = Expr.of_unit; mk = Expr.to_unit })
[@@warning "-33-39"]

let ex_alias_unit_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named ("ExAliasUnit", `special ("unit", `exactly `null)) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_alias_unit_to_json =
  (let unit_to_json () = (`num 1. : Kxclib.Json.jv) in
   unit_to_json
    : ex_alias_unit -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_alias_unit_of_json' =
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
         in
         unit_of_json'
       in
       of_json_impl)
        path x
      |> Result.map_error (fun (msg, path) ->
             (msg, path, ex_alias_unit_json_shape_explanation))
    : ex_alias_unit Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_alias_unit_of_json =
  (fun x -> ex_alias_unit_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_alias_unit option)
[@@warning "-39"]

let ex_alias_unit_decl = Bindoj_test_common_typedesc_examples.Ex_alias.Unit.decl

let ex_alias_unit_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_alias.Unit.decl
    ex_alias_unit_reflect

type nonrec ex_alias_int_opt = int option

let (ex_alias_int_opt_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Alias
       { get = Expr.of_option Expr.of_int; mk = Expr.to_option Expr.to_int })
[@@warning "-33-39"]

let ex_alias_int_opt_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named ("ExAliasIntOpt", `nullable `integral) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_alias_int_opt_to_json =
  (let option_to_json t_to_json = function
     | Some x -> t_to_json x
     | None -> (`null : Kxclib.Json.jv)
   and int_to_json (x : int) = (`num (float_of_int x) : Kxclib.Json.jv) in
   option_to_json int_to_json
    : ex_alias_int_opt -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_alias_int_opt_of_json' =
  (fun ?(path = []) ->
     fun x ->
      (let rec of_json_impl =
         let option_of_json' t_of_json path = function
           | `null -> Ok None
           | x -> (
               match t_of_json path x with
               | Ok x -> Ok (Some x)
               | Error msg -> Error msg)
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
         option_of_json' int_of_json'
       in
       of_json_impl)
        path x
      |> Result.map_error (fun (msg, path) ->
             (msg, path, ex_alias_int_opt_json_shape_explanation))
    : ex_alias_int_opt Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_alias_int_opt_of_json =
  (fun x -> ex_alias_int_opt_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_alias_int_opt option)
[@@warning "-39"]

let ex_alias_int_opt_decl =
  Bindoj_test_common_typedesc_examples.Ex_alias.Int_opt.decl

let ex_alias_int_opt_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_alias.Int_opt.decl
    ex_alias_int_opt_reflect

type nonrec ex_alias_objtuple = float * string

let (ex_alias_objtuple_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Alias
       {
         get =
           (fun (x0, x1) -> Expr.Tuple [ Expr.of_float x0; Expr.of_string x1 ]);
         mk =
           (function
           | Expr.Tuple [ x0; x1 ] ->
               Expr.to_float x0 >>= fun x0 ->
               Expr.to_string x1 >>= fun x1 -> Some (x0, x1)
           | _ -> None);
       })
[@@warning "-33-39"]

let ex_alias_objtuple_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExAliasObjtuple",
           `object_of
             [
               `mandatory_field ("_0", `proper_float);
               `mandatory_field ("_1", `string);
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_alias_objtuple_to_json =
  (let string_to_json (x : string) = (`str x : Kxclib.Json.jv)
   and float_to_json (x : float) = (`num x : Kxclib.Json.jv) in
   fun (x0, x1) ->
     (`obj [ ("_0", float_to_json x0); ("_1", string_to_json x1) ]
       : Kxclib.Json.jv)
    : ex_alias_objtuple -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_alias_objtuple_of_json' =
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
         and float_of_json' path = function
           | (`num x : Kxclib.Json.jv) -> Ok x
           | jv ->
               Error
                 ( Printf.sprintf
                     "expecting type 'float' but the given is of type '%s'"
                     (let open Kxclib.Json in
                      string_of_jv_kind (classify_jv jv)),
                   path )
         in
         fun path -> function
           | (`obj fields : Kxclib.Json.jv) ->
               let ( >>= ) = Result.bind in
               List.assoc_opt "_0" fields
               |> Option.to_result
                    ~none:("mandatory field '_0' does not exist", path)
               >>= float_of_json' (`f "_0" :: path)
               >>= fun x0 ->
               List.assoc_opt "_1" fields
               |> Option.to_result
                    ~none:("mandatory field '_1' does not exist", path)
               >>= string_of_json' (`f "_1" :: path)
               >>= fun x1 -> Ok (x0, x1)
           | jv ->
               Error
                 ( Printf.sprintf
                     "an object is expected for a tuple value, but the given \
                      is of type '%s'"
                     (let open Kxclib.Json in
                      string_of_jv_kind (classify_jv jv)),
                   path )
       in
       of_json_impl)
        path x
      |> Result.map_error (fun (msg, path) ->
             (msg, path, ex_alias_objtuple_json_shape_explanation))
    : ex_alias_objtuple Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_alias_objtuple_of_json =
  (fun x -> ex_alias_objtuple_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_alias_objtuple option)
[@@warning "-39"]

let ex_alias_objtuple_decl =
  Bindoj_test_common_typedesc_examples.Ex_alias.Objtuple.decl

let ex_alias_objtuple_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_alias.Objtuple.decl
    ex_alias_objtuple_reflect
