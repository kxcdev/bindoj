type int_list = IntNil | IntCons of int * int_list

let rec (int_list_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     let ctor_IntNil = Refl.NoParam { value = IntNil } in
     let ctor_IntCons =
       Refl.TupleLike
         {
           get =
             (function
             | IntCons (x0, x1) ->
                 [ Expr.of_int x0; (Expr.of_refl int_list_reflect) x1 ]
             | _ -> invalid_arg "IntCons is expected");
           mk =
             (function
             | [ x0; x1 ] ->
                 Expr.to_int x0 >>= fun x0 ->
                 (Expr.to_refl int_list_reflect) x1 >>= fun x1 ->
                 Some (IntCons (x0, x1))
             | _ -> None);
         }
     in
     Refl.Variant
       {
         constructors =
           StringMap.of_list
             [ ("IntNil", ctor_IntNil); ("IntCons", ctor_IntCons) ];
         classify =
           (function
           | IntNil -> ("IntNil", ctor_IntNil)
           | IntCons _ -> ("IntCons", ctor_IntCons));
       })
[@@warning "-33-39"]

let int_list_json_discriminator_value =
  (function IntNil -> "intnil" | IntCons _ -> "intcons" : int_list -> string)
[@@warning "-39"]

let int_list_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "IntList",
           `anyone_of
             [
               `object_of
                 [ `mandatory_field ("kind", `exactly (`str "intnil")) ];
               `object_of
                 [
                   `mandatory_field ("kind", `exactly (`str "intcons"));
                   `mandatory_field ("arg", `tuple_of [ `integral; `self ]);
                 ];
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec int_list_to_json =
  (let int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   function
   | IntNil -> `obj [ ("kind", `str "intnil") ]
   | IntCons (x0, x1) ->
       `obj
         [
           ("kind", `str "intcons");
           ("arg", `arr [ int_to_json x0; int_list_to_json x1 ]);
         ]
    : int_list -> Kxclib.Json.jv)
[@@warning "-39"]

and int_list_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl =
        let int_of_json' path = function
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
          match Kxclib.Jv.pump_field "kind" __bindoj_orig with
          | `obj (("kind", `str "intnil") :: _) -> Ok IntNil
          | `obj (("kind", `str "intcons") :: param) -> (
              match List.assoc_opt "arg" param with
              | Some (`arr [ x0; x1 ]) ->
                  let ( >>= ) = Result.bind in
                  int_of_json' (`i 0 :: `f "arg" :: path) x0 >>= fun x0 ->
                  of_json_impl (`i 1 :: `f "arg" :: path) x1 >>= fun x1 ->
                  Ok (IntCons (x0, x1))
              | Some (`arr xs) ->
                  Error
                    ( Printf.sprintf
                        "expecting an array of length 2, but the given has a \
                         length of %d"
                        (List.length xs),
                      `f "arg" :: path )
              | Some jv ->
                  Error
                    ( Printf.sprintf
                        "an array is expected for a tuple value, but the given \
                         is of type '%s'"
                        (let open Kxclib.Json in
                         string_of_jv_kind (classify_jv jv)),
                      `f "arg" :: path )
              | None -> Error ("mandatory field 'arg' does not exist", path))
          | `obj (("kind", `str discriminator_value) :: _) ->
              Error
                ( Printf.sprintf
                    "given discriminator field value '%s' is not one of [ \
                     'intnil', 'intcons' ]"
                    discriminator_value,
                  `f "kind" :: path )
          | `obj (("kind", jv) :: _) ->
              Error
                ( Printf.sprintf
                    "a string is expected for a variant discriminator, but the \
                     given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  `f "kind" :: path )
          | `obj _ -> Error ("discriminator field 'kind' does not exist", path)
          | jv ->
              Error
                ( Printf.sprintf
                    "an object is expected for a variant value, but the given \
                     is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, int_list_json_shape_explanation))
    : int_list Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and int_list_of_json =
  (fun x -> int_list_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> int_list option)
[@@warning "-39"]

let int_list_decl = Bindoj_test_common_typedesc_examples.Ex03.decl

let int_list_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex03.decl int_list_reflect
