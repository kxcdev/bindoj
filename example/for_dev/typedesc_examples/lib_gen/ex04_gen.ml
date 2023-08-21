type foo = [ `Foo0 | `Foo1 of int | `Foo2 of int * int ]

let rec (foo_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     let ctor_Foo0 = Refl.NoParam { value = `Foo0 } in
     let ctor_Foo1 =
       Refl.TupleLike
         {
           get =
             (function
             | `Foo1 x -> [ Expr.of_int x ]
             | _ -> invalid_arg "Foo1 is expected");
           mk =
             (function
             | x :: [] -> Expr.to_int x |> Option.map (fun x -> `Foo1 x)
             | _ -> None);
         }
     in
     let ctor_Foo2 =
       Refl.TupleLike
         {
           get =
             (function
             | `Foo2 (x0, x1) -> [ Expr.of_int x0; Expr.of_int x1 ]
             | _ -> invalid_arg "Foo2 is expected");
           mk =
             (function
             | [ x0; x1 ] ->
                 Expr.to_int x0 >>= fun x0 ->
                 Expr.to_int x1 >>= fun x1 -> Some (`Foo2 (x0, x1))
             | _ -> None);
         }
     in
     Refl.Variant
       {
         constructors =
           StringMap.of_list
             [ ("Foo0", ctor_Foo0); ("Foo1", ctor_Foo1); ("Foo2", ctor_Foo2) ];
         classify =
           (function
           | `Foo0 -> ("Foo0", ctor_Foo0)
           | `Foo1 _ -> ("Foo1", ctor_Foo1)
           | `Foo2 _ -> ("Foo2", ctor_Foo2));
       })
[@@warning "-33-39"]

let foo_json_discriminator_value =
  (function `Foo0 -> "foo0" | `Foo1 _ -> "foo1" | `Foo2 _ -> "foo2"
    : foo -> string)
[@@warning "-39"]

let foo_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "Foo",
           `anyone_of
             [
               `object_of [ `mandatory_field ("kind", `exactly (`str "foo0")) ];
               `object_of
                 [
                   `mandatory_field ("kind", `exactly (`str "foo1"));
                   `mandatory_field ("arg", `integral);
                 ];
               `object_of
                 [
                   `mandatory_field ("kind", `exactly (`str "foo2"));
                   `mandatory_field ("arg", `tuple_of [ `integral; `integral ]);
                 ];
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec foo_to_json =
  (let int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   function
   | `Foo0 -> `obj [ ("kind", `str "foo0") ]
   | `Foo1 x0 -> `obj [ ("kind", `str "foo1"); ("arg", int_to_json x0) ]
   | `Foo2 (x0, x1) ->
       `obj
         [
           ("kind", `str "foo2");
           ("arg", `arr [ int_to_json x0; int_to_json x1 ]);
         ]
    : foo -> Kxclib.Json.jv)
[@@warning "-39"]

and foo_of_json' =
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
          | `obj (("kind", `str "foo0") :: _) -> Ok `Foo0
          | `obj (("kind", `str "foo1") :: param) -> (
              match List.assoc_opt "arg" param with
              | Some arg ->
                  let ( >>= ) = Result.bind in
                  int_of_json' (`f "arg" :: path) arg >>= fun x0 ->
                  Ok (`Foo1 x0)
              | None -> Error ("mandatory field 'arg' does not exist", path))
          | `obj (("kind", `str "foo2") :: param) -> (
              match List.assoc_opt "arg" param with
              | Some (`arr [ x0; x1 ]) ->
                  let ( >>= ) = Result.bind in
                  int_of_json' (`i 0 :: `f "arg" :: path) x0 >>= fun x0 ->
                  int_of_json' (`i 1 :: `f "arg" :: path) x1 >>= fun x1 ->
                  Ok (`Foo2 (x0, x1))
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
                     'foo0', 'foo1', 'foo2' ]"
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
            (msg, path, foo_json_shape_explanation))
    : foo Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and foo_of_json =
  (fun x -> foo_of_json' x |> Result.to_option : Kxclib.Json.jv -> foo option)
[@@warning "-39"]

let foo_decl = Bindoj_test_common_typedesc_examples.Ex04.decl

let foo_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex04.decl foo_reflect
