type nonrec product_id = int

let (product_id_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Alias { get = Expr.of_int; mk = Expr.to_int })
  [@@warning "-33-39"]

let product_id_json_shape_explanation =
  (`with_warning
     ("not considering any config if exists", `named ("ProductId", `integral))
    : Bindoj_runtime.json_shape_explanation)
  [@@warning "-39"]

let rec product_id_to_json =
  (let int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   int_to_json
    : product_id -> Kxclib.Json.jv)
  [@@warning "-39"]

and product_id_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl path =
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
        int_of_json' path
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, product_id_json_shape_explanation))
    : product_id Bindoj_runtime.json_full_decoder)
  [@@warning "-39"]

and product_id_of_json =
  (fun x -> product_id_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> product_id option)
  [@@warning "-39"]

type product_details = {
  name : string; [@ocaml.doc "Product name"]
  description : string; [@ocaml.doc "Product description"]
  price : int; [@ocaml.doc "Product price"]
  count : int; [@ocaml.doc "Count of the product"]
}
[@@ocaml.doc "Product details"]

let rec (product_details_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { name; description; price; count } ->
             StringMap.of_list
               [
                 ("name", Expr.of_string name);
                 ("description", Expr.of_string description);
                 ("price", Expr.of_int price);
                 ("count", Expr.of_int count);
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "name" >>= Expr.to_string >>= fun name ->
             xs |> StringMap.find_opt "description" >>= Expr.to_string
             >>= fun description ->
             xs |> StringMap.find_opt "price" >>= Expr.to_int >>= fun price ->
             xs |> StringMap.find_opt "count" >>= Expr.to_int >>= fun count ->
             Some { name; description; price; count });
       })
  [@@warning "-33-39"]

let product_details_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ProductDetails",
           `object_of
             [
               `mandatory_field ("name", `string);
               `mandatory_field ("description", `string);
               `mandatory_field ("price", `integral);
               `mandatory_field ("count", `integral);
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
  [@@warning "-39"]

let rec product_details_to_json =
  (let string_to_json (x : string) : Kxclib.Json.jv = `str x
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   fun { name = x0; description = x1; price = x2; count = x3 } ->
     `obj
       [
         ("name", string_to_json x0);
         ("description", string_to_json x1);
         ("price", int_to_json x2);
         ("count", int_to_json x3);
       ]
    : product_details -> Kxclib.Json.jv)
  [@@warning "-39"]

and product_details_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl path =
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
        function
        | `obj param ->
            let ( >>= ) = Result.bind in
            List.assoc_opt "name" param
            |> (function
                 | Some a -> Ok a
                 | None -> Error ("mandatory field 'name' does not exist", path))
            >>= string_of_json' (`f "name" :: path)
            >>= fun x0 ->
            List.assoc_opt "description" param
            |> (function
                 | Some a -> Ok a
                 | None ->
                     Error ("mandatory field 'description' does not exist", path))
            >>= string_of_json' (`f "description" :: path)
            >>= fun x1 ->
            List.assoc_opt "price" param
            |> (function
                 | Some a -> Ok a
                 | None -> Error ("mandatory field 'price' does not exist", path))
            >>= int_of_json' (`f "price" :: path)
            >>= fun x2 ->
            List.assoc_opt "count" param
            |> (function
                 | Some a -> Ok a
                 | None -> Error ("mandatory field 'count' does not exist", path))
            >>= int_of_json' (`f "count" :: path)
            >>= fun x3 ->
            Ok { name = x0; description = x1; price = x2; count = x3 }
        | jv ->
            Error
              ( Printf.sprintf
                  "an object is expected for a record value, but the given is \
                   of type '%s'"
                  (let open Kxclib.Json in
                   string_of_jv_kind (classify_jv jv)),
                path )
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, product_details_json_shape_explanation))
    : product_details Bindoj_runtime.json_full_decoder)
  [@@warning "-39"]

and product_details_of_json =
  (fun x -> product_details_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> product_details option)
  [@@warning "-39"]

type product = {
  id : product_id; [@ocaml.doc "Product ID"]
  details : product_details;
}
[@@ocaml.doc "Product"]

let rec (product_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { id; details } ->
             StringMap.of_list
               [
                 ("id", (Expr.of_refl product_id_reflect) id);
                 ("details", (Expr.of_refl product_details_reflect) details);
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "id" >>= Expr.to_refl product_id_reflect
             >>= fun id ->
             xs
             |> StringMap.find_opt "details"
             >>= Expr.to_refl product_details_reflect
             >>= fun details -> Some { id; details });
       })
  [@@warning "-33-39"]

let product_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "Product",
           `object_of
             [
               `mandatory_field
                 ( "id",
                   match product_id_json_shape_explanation with
                   | `with_warning (_, (`named _ as s)) -> s
                   | `with_warning (_, s) | s -> `named ("ProductId", s) );
               `mandatory_field
                 ( "details",
                   match product_details_json_shape_explanation with
                   | `with_warning (_, (`named _ as s)) -> s
                   | `with_warning (_, s) | s -> `named ("ProductDetails", s) );
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
  [@@warning "-39"]

let rec product_to_json =
  (fun { id = x0; details = x1 } ->
     `obj
       [
         ("id", product_id_to_json x0); ("details", product_details_to_json x1);
       ]
    : product -> Kxclib.Json.jv)
  [@@warning "-39"]

and product_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl path = function
        | `obj param ->
            let ( >>= ) = Result.bind in
            List.assoc_opt "id" param
            |> (function
                 | Some a -> Ok a
                 | None -> Error ("mandatory field 'id' does not exist", path))
            >>= (fun path x ->
                  product_id_of_json' ~path x
                  |> Result.map_error (fun (msg, path, _) -> (msg, path)))
                  (`f "id" :: path)
            >>= fun x0 ->
            List.assoc_opt "details" param
            |> (function
                 | Some a -> Ok a
                 | None ->
                     Error ("mandatory field 'details' does not exist", path))
            >>= (fun path x ->
                  product_details_of_json' ~path x
                  |> Result.map_error (fun (msg, path, _) -> (msg, path)))
                  (`f "details" :: path)
            >>= fun x1 -> Ok { id = x0; details = x1 }
        | jv ->
            Error
              ( Printf.sprintf
                  "an object is expected for a record value, but the given is \
                   of type '%s'"
                  (let open Kxclib.Json in
                   string_of_jv_kind (classify_jv jv)),
                path )
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, product_json_shape_explanation))
    : product Bindoj_runtime.json_full_decoder)
  [@@warning "-39"]

and product_of_json =
  (fun x -> product_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> product option)
  [@@warning "-39"]

type nonrec order_id = int

let (order_id_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Alias { get = Expr.of_int; mk = Expr.to_int })
  [@@warning "-33-39"]

let order_id_json_shape_explanation =
  (`with_warning
     ("not considering any config if exists", `named ("OrderId", `integral))
    : Bindoj_runtime.json_shape_explanation)
  [@@warning "-39"]

let rec order_id_to_json =
  (let int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   int_to_json
    : order_id -> Kxclib.Json.jv)
  [@@warning "-39"]

and order_id_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl path =
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
        int_of_json' path
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, order_id_json_shape_explanation))
    : order_id Bindoj_runtime.json_full_decoder)
  [@@warning "-39"]

and order_id_of_json =
  (fun x -> order_id_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> order_id option)
  [@@warning "-39"]

type payment_method =
  | Credit_card of {
      card_number : string; [@ocaml.doc "Card number"]
      holder_name : string; [@ocaml.doc "Card holder name"]
      expiration_date : int * int; [@ocaml.doc "Expiration date"]
      cvv : string; [@ocaml.doc "Card CVV"]
    } [@ocaml.doc "Payment by credit card"]
  | Bank_transfer of {
      account_number : string; [@ocaml.doc "Account number"]
      bank_name : string; [@ocaml.doc "Bank name"]
      holder_name : string; [@ocaml.doc "Account holder name"]
    } [@ocaml.doc "Payment by bank transer"]
[@@ocaml.doc "Payment method of an order"]

let rec (payment_method_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     let ctor_Credit_card =
       Refl.InlineRecord
         {
           get =
             (function
             | Credit_card { card_number; holder_name; expiration_date; cvv } ->
                 StringMap.of_list
                   [
                     ("card_number", Expr.of_string card_number);
                     ("holder_name", Expr.of_string holder_name);
                     ( "expiration_date",
                       (fun (x0, x1) ->
                         Expr.Tuple [ Expr.of_int x0; Expr.of_int x1 ])
                         expiration_date );
                     ("cvv", Expr.of_string cvv);
                   ]
             | _ -> invalid_arg ("Credit_card" ^ " is expected"));
           mk =
             (fun xs ->
               xs |> StringMap.find_opt "card_number" >>= Expr.to_string
               >>= fun card_number ->
               xs |> StringMap.find_opt "holder_name" >>= Expr.to_string
               >>= fun holder_name ->
               (xs |> StringMap.find_opt "expiration_date" >>= function
                | Expr.Tuple [ x0; x1 ] ->
                    Expr.to_int x0 >>= fun x0 ->
                    Expr.to_int x1 >>= fun x1 -> Some (x0, x1)
                | _ -> None)
               >>= fun expiration_date ->
               xs |> StringMap.find_opt "cvv" >>= Expr.to_string >>= fun cvv ->
               Some
                 (Credit_card { card_number; holder_name; expiration_date; cvv }));
         }
     in
     let ctor_Bank_transfer =
       Refl.InlineRecord
         {
           get =
             (function
             | Bank_transfer { account_number; bank_name; holder_name } ->
                 StringMap.of_list
                   [
                     ("account_number", Expr.of_string account_number);
                     ("bank_name", Expr.of_string bank_name);
                     ("holder_name", Expr.of_string holder_name);
                   ]
             | _ -> invalid_arg ("Bank_transfer" ^ " is expected"));
           mk =
             (fun xs ->
               xs |> StringMap.find_opt "account_number" >>= Expr.to_string
               >>= fun account_number ->
               xs |> StringMap.find_opt "bank_name" >>= Expr.to_string
               >>= fun bank_name ->
               xs |> StringMap.find_opt "holder_name" >>= Expr.to_string
               >>= fun holder_name ->
               Some (Bank_transfer { account_number; bank_name; holder_name }));
         }
     in
     Refl.Variant
       {
         constructors =
           StringMap.of_list
             [
               ("Credit_card", ctor_Credit_card);
               ("Bank_transfer", ctor_Bank_transfer);
             ];
         classify =
           (function
           | Credit_card _ -> ("Credit_card", ctor_Credit_card)
           | Bank_transfer _ -> ("Bank_transfer", ctor_Bank_transfer));
       })
  [@@warning "-33-39"]

let payment_method_json_discriminator_value =
  (function
   | Credit_card _ -> "credit-card" | Bank_transfer _ -> "bank-transfer"
    : payment_method -> string)
  [@@warning "-39"]

let payment_method_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "PaymentMethod",
           `anyone_of
             [
               `object_of
                 [
                   `mandatory_field ("kind", `exactly (`str "credit-card"));
                   `mandatory_field ("cardNumber", `string);
                   `mandatory_field ("holderName", `string);
                   `mandatory_field
                     ("expirationDate", `tuple_of [ `integral; `integral ]);
                   `mandatory_field ("cvv", `string);
                 ];
               `object_of
                 [
                   `mandatory_field ("kind", `exactly (`str "bank-transfer"));
                   `mandatory_field ("accountNumber", `string);
                   `mandatory_field ("bankName", `string);
                   `mandatory_field ("holderName", `string);
                 ];
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
  [@@warning "-39"]

let rec payment_method_to_json =
  (let string_to_json (x : string) : Kxclib.Json.jv = `str x
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   function
   | Credit_card
       { card_number = x0; holder_name = x1; expiration_date = x2; cvv = x3 } ->
       `obj
         [
           ("kind", `str "credit-card");
           ("cardNumber", string_to_json x0);
           ("holderName", string_to_json x1);
           ( "expirationDate",
             (fun (x0, x1) : Kxclib.Json.jv ->
               `obj [ ("_0", int_to_json x0); ("_1", int_to_json x1) ])
               x2 );
           ("cvv", string_to_json x3);
         ]
   | Bank_transfer { account_number = x0; bank_name = x1; holder_name = x2 } ->
       `obj
         [
           ("kind", `str "bank-transfer");
           ("accountNumber", string_to_json x0);
           ("bankName", string_to_json x1);
           ("holderName", string_to_json x2);
         ]
    : payment_method -> Kxclib.Json.jv)
  [@@warning "-39"]

and payment_method_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl path __bindoj_orig =
        __bindoj_orig
        |> Kxclib.Jv.pump_field "kind"
        |>
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
        function
        | `obj (("kind", `str "credit-card") :: param) ->
            let ( >>= ) = Result.bind in
            List.assoc_opt "cardNumber" param
            |> (function
                 | Some a -> Ok a
                 | None ->
                     Error ("mandatory field 'cardNumber' does not exist", path))
            >>= string_of_json' (`f "cardNumber" :: path)
            >>= fun x0 ->
            List.assoc_opt "holderName" param
            |> (function
                 | Some a -> Ok a
                 | None ->
                     Error ("mandatory field 'holderName' does not exist", path))
            >>= string_of_json' (`f "holderName" :: path)
            >>= fun x1 ->
            List.assoc_opt "expirationDate" param
            |> (function
                 | Some a -> Ok a
                 | None ->
                     Error
                       ("mandatory field 'expirationDate' does not exist", path))
            >>= (fun path -> function
                  | (`obj fields : Kxclib.Json.jv) ->
                      let fields = Bindoj_runtime.StringMap.of_list fields in
                      let ( >>= ) = Result.bind in
                      (Bindoj_runtime.StringMap.find_opt "_0" fields |> function
                       | Some a -> Ok a
                       | None ->
                           Error ("mandatory field '_0' does not exist", path))
                      >>= fun x0 ->
                      (Bindoj_runtime.StringMap.find_opt "_1" fields |> function
                       | Some a -> Ok a
                       | None ->
                           Error ("mandatory field '_1' does not exist", path))
                      >>= fun x1 ->
                      let ( >>= ) = Result.bind in
                      int_of_json' (`f "_0" :: path) x0 >>= fun x0 ->
                      int_of_json' (`f "_1" :: path) x1 >>= fun x1 -> Ok (x0, x1)
                  | jv ->
                      Error
                        ( Printf.sprintf
                            "an object is expected for a tuple value, but the \
                             given is of type '%s'"
                            (let open Kxclib.Json in
                             string_of_jv_kind (classify_jv jv)),
                          path ))
                  (`f "expirationDate" :: path)
            >>= fun x2 ->
            List.assoc_opt "cvv" param
            |> (function
                 | Some a -> Ok a
                 | None -> Error ("mandatory field 'cvv' does not exist", path))
            >>= string_of_json' (`f "cvv" :: path)
            >>= fun x3 ->
            Ok
              (Credit_card
                 {
                   card_number = x0;
                   holder_name = x1;
                   expiration_date = x2;
                   cvv = x3;
                 })
        | `obj (("kind", `str "bank-transfer") :: param) ->
            let ( >>= ) = Result.bind in
            List.assoc_opt "accountNumber" param
            |> (function
                 | Some a -> Ok a
                 | None ->
                     Error
                       ("mandatory field 'accountNumber' does not exist", path))
            >>= string_of_json' (`f "accountNumber" :: path)
            >>= fun x0 ->
            List.assoc_opt "bankName" param
            |> (function
                 | Some a -> Ok a
                 | None ->
                     Error ("mandatory field 'bankName' does not exist", path))
            >>= string_of_json' (`f "bankName" :: path)
            >>= fun x1 ->
            List.assoc_opt "holderName" param
            |> (function
                 | Some a -> Ok a
                 | None ->
                     Error ("mandatory field 'holderName' does not exist", path))
            >>= string_of_json' (`f "holderName" :: path)
            >>= fun x2 ->
            Ok
              (Bank_transfer
                 { account_number = x0; bank_name = x1; holder_name = x2 })
        | `obj (("kind", `str discriminator_value) :: _) ->
            Error
              ( Printf.sprintf
                  "given discriminator field value '%s' is not one of [ \
                   'credit-card', 'bank-transfer' ]"
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
                  "an object is expected for a variant value, but the given is \
                   of type '%s'"
                  (let open Kxclib.Json in
                   string_of_jv_kind (classify_jv jv)),
                path )
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, payment_method_json_shape_explanation))
    : payment_method Bindoj_runtime.json_full_decoder)
  [@@warning "-39"]

and payment_method_of_json =
  (fun x -> payment_method_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> payment_method option)
  [@@warning "-39"]

type order_details = {
  products : (product_id * int) list;
      [@ocaml.doc "ID and it's count of ordered products"]
  payment_method : payment_method; [@ocaml.doc "Payment method"]
}

let rec (order_details_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { products; payment_method } ->
             StringMap.of_list
               [
                 ( "products",
                   (Expr.of_list (fun (x0, x1) ->
                        Expr.Tuple
                          [
                            (Expr.of_refl product_id_reflect) x0; Expr.of_int x1;
                          ]))
                     products );
                 ( "payment_method",
                   (Expr.of_refl payment_method_reflect) payment_method );
               ]);
         mk =
           (fun xs ->
             xs
             |> StringMap.find_opt "products"
             >>= Expr.to_list (function
                   | Expr.Tuple [ x0; x1 ] ->
                       (Expr.to_refl product_id_reflect) x0 >>= fun x0 ->
                       Expr.to_int x1 >>= fun x1 -> Some (x0, x1)
                   | _ -> None)
             >>= fun products ->
             xs
             |> StringMap.find_opt "payment_method"
             >>= Expr.to_refl payment_method_reflect
             >>= fun payment_method -> Some { products; payment_method });
       })
  [@@warning "-33-39"]

let order_details_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "OrderDetails",
           `object_of
             [
               `mandatory_field
                 ( "products",
                   `array_of
                     (`tuple_of
                       [
                         (match product_id_json_shape_explanation with
                         | `with_warning (_, (`named _ as s)) -> s
                         | `with_warning (_, s) | s -> `named ("ProductId", s));
                         `integral;
                       ]) );
               `mandatory_field
                 ( "paymentMethod",
                   match payment_method_json_shape_explanation with
                   | `with_warning (_, (`named _ as s)) -> s
                   | `with_warning (_, s) | s -> `named ("PaymentMethod", s) );
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
  [@@warning "-39"]

let rec order_details_to_json =
  (let list_to_json t_to_json xs : Kxclib.Json.jv = `arr (List.map t_to_json xs)
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   fun { products = x0; payment_method = x1 } ->
     `obj
       [
         ( "products",
           (list_to_json (fun (x0, x1) : Kxclib.Json.jv ->
                `obj [ ("_0", product_id_to_json x0); ("_1", int_to_json x1) ]))
             x0 );
         ("paymentMethod", payment_method_to_json x1);
       ]
    : order_details -> Kxclib.Json.jv)
  [@@warning "-39"]

and order_details_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl path =
        let list_of_json' t_of_json path = function
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
        function
        | `obj param ->
            let ( >>= ) = Result.bind in
            List.assoc_opt "products" param
            |> (function
                 | Some a -> Ok a
                 | None ->
                     Error ("mandatory field 'products' does not exist", path))
            >>= (list_of_json' (fun path -> function
                   | (`obj fields : Kxclib.Json.jv) ->
                       let fields = Bindoj_runtime.StringMap.of_list fields in
                       let ( >>= ) = Result.bind in
                       (Bindoj_runtime.StringMap.find_opt "_0" fields
                        |> function
                        | Some a -> Ok a
                        | None ->
                            Error ("mandatory field '_0' does not exist", path))
                       >>= fun x0 ->
                       (Bindoj_runtime.StringMap.find_opt "_1" fields
                        |> function
                        | Some a -> Ok a
                        | None ->
                            Error ("mandatory field '_1' does not exist", path))
                       >>= fun x1 ->
                       let ( >>= ) = Result.bind in
                       (fun path x ->
                         product_id_of_json' ~path x
                         |> Result.map_error (fun (msg, path, _) -> (msg, path)))
                         (`f "_0" :: path) x0
                       >>= fun x0 ->
                       int_of_json' (`f "_1" :: path) x1 >>= fun x1 ->
                       Ok (x0, x1)
                   | jv ->
                       Error
                         ( Printf.sprintf
                             "an object is expected for a tuple value, but the \
                              given is of type '%s'"
                             (let open Kxclib.Json in
                              string_of_jv_kind (classify_jv jv)),
                           path )))
                  (`f "products" :: path)
            >>= fun x0 ->
            List.assoc_opt "paymentMethod" param
            |> (function
                 | Some a -> Ok a
                 | None ->
                     Error
                       ("mandatory field 'paymentMethod' does not exist", path))
            >>= (fun path x ->
                  payment_method_of_json' ~path x
                  |> Result.map_error (fun (msg, path, _) -> (msg, path)))
                  (`f "paymentMethod" :: path)
            >>= fun x1 -> Ok { products = x0; payment_method = x1 }
        | jv ->
            Error
              ( Printf.sprintf
                  "an object is expected for a record value, but the given is \
                   of type '%s'"
                  (let open Kxclib.Json in
                   string_of_jv_kind (classify_jv jv)),
                path )
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, order_details_json_shape_explanation))
    : order_details Bindoj_runtime.json_full_decoder)
  [@@warning "-39"]

and order_details_of_json =
  (fun x -> order_details_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> order_details option)
  [@@warning "-39"]

type nonrec order_status =
  [ `Unpaid | `Paid | `Shipped | `Delivered | `Canceled ]
[@@ocaml.doc "Status of an order"]

let (order_status_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Alias
       {
         get =
           (function
           | `Unpaid -> Expr.StringEnum "Unpaid"
           | `Paid -> Expr.StringEnum "Paid"
           | `Shipped -> Expr.StringEnum "Shipped"
           | `Delivered -> Expr.StringEnum "Delivered"
           | `Canceled -> Expr.StringEnum "Canceled");
         mk =
           (function
           | Expr.StringEnum "Unpaid" -> Some `Unpaid
           | Expr.StringEnum "Paid" -> Some `Paid
           | Expr.StringEnum "Shipped" -> Some `Shipped
           | Expr.StringEnum "Delivered" -> Some `Delivered
           | Expr.StringEnum "Canceled" -> Some `Canceled
           | _ -> None);
       })
  [@@warning "-33-39"]

let order_status_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "OrderStatus",
           `string_enum [ "Unpaid"; "Paid"; "Shipped"; "Delivered"; "Canceled" ]
         ) )
    : Bindoj_runtime.json_shape_explanation)
  [@@warning "-39"]

let rec order_status_to_json =
  (function
   | `Unpaid -> `str "Unpaid"
   | `Paid -> `str "Paid"
   | `Shipped -> `str "Shipped"
   | `Delivered -> `str "Delivered"
   | `Canceled -> `str "Canceled"
    : order_status -> Kxclib.Json.jv)
  [@@warning "-39"]

and order_status_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl path =
        (fun path -> function
          | `str s ->
              (function
                | "Unpaid" -> Ok `Unpaid
                | "Paid" -> Ok `Paid
                | "Shipped" -> Ok `Shipped
                | "Delivered" -> Ok `Delivered
                | "Canceled" -> Ok `Canceled
                | s ->
                    Error
                      ( Printf.sprintf
                          "given string '%s' is not one of [ 'Unpaid', 'Paid', \
                           'Shipped', 'Delivered', 'Canceled' ]"
                          s,
                        path ))
                s
          | jv ->
              Error
                ( Printf.sprintf
                    "expecting type 'string' but the given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path ))
          path
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, order_status_json_shape_explanation))
    : order_status Bindoj_runtime.json_full_decoder)
  [@@warning "-39"]

and order_status_of_json =
  (fun x -> order_status_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> order_status option)
  [@@warning "-39"]

type order = {
  id : order_id; [@ocaml.doc "Order ID"]
  total_price : int;
  details : order_details;
  status : order_status;
}

let rec (order_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { id; total_price; details; status } ->
             StringMap.of_list
               [
                 ("id", (Expr.of_refl order_id_reflect) id);
                 ("total_price", Expr.of_int total_price);
                 ("details", (Expr.of_refl order_details_reflect) details);
                 ("status", (Expr.of_refl order_status_reflect) status);
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "id" >>= Expr.to_refl order_id_reflect
             >>= fun id ->
             xs |> StringMap.find_opt "total_price" >>= Expr.to_int
             >>= fun total_price ->
             xs
             |> StringMap.find_opt "details"
             >>= Expr.to_refl order_details_reflect
             >>= fun details ->
             xs
             |> StringMap.find_opt "status"
             >>= Expr.to_refl order_status_reflect
             >>= fun status -> Some { id; total_price; details; status });
       })
  [@@warning "-33-39"]

let order_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "Order",
           `object_of
             [
               `mandatory_field
                 ( "id",
                   match order_id_json_shape_explanation with
                   | `with_warning (_, (`named _ as s)) -> s
                   | `with_warning (_, s) | s -> `named ("OrderId", s) );
               `mandatory_field ("totalPrice", `integral);
               `mandatory_field
                 ( "details",
                   match order_details_json_shape_explanation with
                   | `with_warning (_, (`named _ as s)) -> s
                   | `with_warning (_, s) | s -> `named ("OrderDetails", s) );
               `mandatory_field
                 ( "status",
                   match order_status_json_shape_explanation with
                   | `with_warning (_, (`named _ as s)) -> s
                   | `with_warning (_, s) | s -> `named ("OrderStatus", s) );
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
  [@@warning "-39"]

let rec order_to_json =
  (let int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   fun { id = x0; total_price = x1; details = x2; status = x3 } ->
     `obj
       [
         ("id", order_id_to_json x0);
         ("totalPrice", int_to_json x1);
         ("details", order_details_to_json x2);
         ("status", order_status_to_json x3);
       ]
    : order -> Kxclib.Json.jv)
  [@@warning "-39"]

and order_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl path =
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
        function
        | `obj param ->
            let ( >>= ) = Result.bind in
            List.assoc_opt "id" param
            |> (function
                 | Some a -> Ok a
                 | None -> Error ("mandatory field 'id' does not exist", path))
            >>= (fun path x ->
                  order_id_of_json' ~path x
                  |> Result.map_error (fun (msg, path, _) -> (msg, path)))
                  (`f "id" :: path)
            >>= fun x0 ->
            List.assoc_opt "totalPrice" param
            |> (function
                 | Some a -> Ok a
                 | None ->
                     Error ("mandatory field 'totalPrice' does not exist", path))
            >>= int_of_json' (`f "totalPrice" :: path)
            >>= fun x1 ->
            List.assoc_opt "details" param
            |> (function
                 | Some a -> Ok a
                 | None ->
                     Error ("mandatory field 'details' does not exist", path))
            >>= (fun path x ->
                  order_details_of_json' ~path x
                  |> Result.map_error (fun (msg, path, _) -> (msg, path)))
                  (`f "details" :: path)
            >>= fun x2 ->
            List.assoc_opt "status" param
            |> (function
                 | Some a -> Ok a
                 | None ->
                     Error ("mandatory field 'status' does not exist", path))
            >>= (fun path x ->
                  order_status_of_json' ~path x
                  |> Result.map_error (fun (msg, path, _) -> (msg, path)))
                  (`f "status" :: path)
            >>= fun x3 ->
            Ok { id = x0; total_price = x1; details = x2; status = x3 }
        | jv ->
            Error
              ( Printf.sprintf
                  "an object is expected for a record value, but the given is \
                   of type '%s'"
                  (let open Kxclib.Json in
                   string_of_jv_kind (classify_jv jv)),
                path )
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, order_json_shape_explanation))
    : order Bindoj_runtime.json_full_decoder)
  [@@warning "-39"]

and order_of_json =
  (fun x -> order_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> order option)
  [@@warning "-39"]

type product_query = {
  searchQuery : string option;
      [@ocaml.doc
        "Optional search string to match product names or descriptions"]
  minimum_price : int option;
      [@ocaml.doc "Optional minimum price constraint for a product"]
  maximum_price : int option;
      [@ocaml.doc "Optional maximum price constraint for a product"]
  limit : int option; [@ocaml.doc "Number limit of data to be acquired."]
}
[@@ocaml.doc "Query to search products"]

let rec (product_query_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { searchQuery; minimum_price; maximum_price; limit } ->
             StringMap.of_list
               [
                 ("searchQuery", (Expr.of_option Expr.of_string) searchQuery);
                 ("minimum_price", (Expr.of_option Expr.of_int) minimum_price);
                 ("maximum_price", (Expr.of_option Expr.of_int) maximum_price);
                 ("limit", (Expr.of_option Expr.of_int) limit);
               ]);
         mk =
           (fun xs ->
             xs
             |> StringMap.find_opt "searchQuery"
             >>= Expr.to_option Expr.to_string
             >>= fun searchQuery ->
             xs
             |> StringMap.find_opt "minimum_price"
             >>= Expr.to_option Expr.to_int
             >>= fun minimum_price ->
             xs
             |> StringMap.find_opt "maximum_price"
             >>= Expr.to_option Expr.to_int
             >>= fun maximum_price ->
             xs |> StringMap.find_opt "limit" >>= Expr.to_option Expr.to_int
             >>= fun limit ->
             Some { searchQuery; minimum_price; maximum_price; limit });
       })
  [@@warning "-33-39"]

let product_query_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ProductQuery",
           `object_of
             [
               `optional_field ("searchQuery", `string);
               `optional_field ("minimumPrice", `integral);
               `optional_field ("maximumPrice", `integral);
               `optional_field ("limit", `integral);
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
  [@@warning "-39"]

let rec product_query_to_json =
  (let string_to_json (x : string) : Kxclib.Json.jv = `str x
   and option_to_json t_to_json = function
     | Some x -> t_to_json x
     | None -> (`null : Kxclib.Json.jv)
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   fun { searchQuery = x0; minimum_price = x1; maximum_price = x2; limit = x3 } ->
     `obj
       [
         ("searchQuery", (option_to_json string_to_json) x0);
         ("minimumPrice", (option_to_json int_to_json) x1);
         ("maximumPrice", (option_to_json int_to_json) x2);
         ("limit", (option_to_json int_to_json) x3);
       ]
    : product_query -> Kxclib.Json.jv)
  [@@warning "-39"]

and product_query_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl path =
        let string_of_json' path = function
          | (`str x : Kxclib.Json.jv) -> Ok x
          | jv ->
              Error
                ( Printf.sprintf
                    "expecting type 'string' but the given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        and option_of_json' t_of_json path = function
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
        function
        | `obj param ->
            let ( >>= ) = Result.bind in
            List.assoc_opt "searchQuery" param
            |> Option.value ~default:`null
            |> (option_of_json' string_of_json') (`f "searchQuery" :: path)
            >>= fun x0 ->
            List.assoc_opt "minimumPrice" param
            |> Option.value ~default:`null
            |> (option_of_json' int_of_json') (`f "minimumPrice" :: path)
            >>= fun x1 ->
            List.assoc_opt "maximumPrice" param
            |> Option.value ~default:`null
            |> (option_of_json' int_of_json') (`f "maximumPrice" :: path)
            >>= fun x2 ->
            List.assoc_opt "limit" param
            |> Option.value ~default:`null
            |> (option_of_json' int_of_json') (`f "limit" :: path)
            >>= fun x3 ->
            Ok
              {
                searchQuery = x0;
                minimum_price = x1;
                maximum_price = x2;
                limit = x3;
              }
        | jv ->
            Error
              ( Printf.sprintf
                  "an object is expected for a record value, but the given is \
                   of type '%s'"
                  (let open Kxclib.Json in
                   string_of_jv_kind (classify_jv jv)),
                path )
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, product_query_json_shape_explanation))
    : product_query Bindoj_runtime.json_full_decoder)
  [@@warning "-39"]

and product_query_of_json =
  (fun x -> product_query_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> product_query option)
  [@@warning "-39"]

type order_query = {
  products : int list option; [@ocaml.doc "List of product IDs in the order"]
  status : order_status list option;
      [@ocaml.doc "Optional order status constraint"]
  minimum_price : int option;
      [@ocaml.doc "Optional minimum total price constraint for the order"]
  maximum_price : int option;
      [@ocaml.doc "Optional maximum total price constraint for the order"]
  limit : int option; [@ocaml.doc "Number limit of data to be acquired."]
}
[@@ocaml.doc "Query to search orders"]

let rec (order_query_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { products; status; minimum_price; maximum_price; limit } ->
             StringMap.of_list
               [
                 ( "products",
                   (Expr.of_option (Expr.of_list Expr.of_int)) products );
                 ( "status",
                   (Expr.of_option
                      (Expr.of_list (Expr.of_refl order_status_reflect)))
                     status );
                 ("minimum_price", (Expr.of_option Expr.of_int) minimum_price);
                 ("maximum_price", (Expr.of_option Expr.of_int) maximum_price);
                 ("limit", (Expr.of_option Expr.of_int) limit);
               ]);
         mk =
           (fun xs ->
             xs
             |> StringMap.find_opt "products"
             >>= Expr.to_option (Expr.to_list Expr.to_int)
             >>= fun products ->
             xs
             |> StringMap.find_opt "status"
             >>= Expr.to_option
                   (Expr.to_list (Expr.to_refl order_status_reflect))
             >>= fun status ->
             xs
             |> StringMap.find_opt "minimum_price"
             >>= Expr.to_option Expr.to_int
             >>= fun minimum_price ->
             xs
             |> StringMap.find_opt "maximum_price"
             >>= Expr.to_option Expr.to_int
             >>= fun maximum_price ->
             xs |> StringMap.find_opt "limit" >>= Expr.to_option Expr.to_int
             >>= fun limit ->
             Some { products; status; minimum_price; maximum_price; limit });
       })
  [@@warning "-33-39"]

let order_query_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "OrderQuery",
           `object_of
             [
               `optional_field ("products", `array_of `integral);
               `optional_field
                 ( "status",
                   `array_of
                     (match order_status_json_shape_explanation with
                     | `with_warning (_, (`named _ as s)) -> s
                     | `with_warning (_, s) | s -> `named ("OrderStatus", s)) );
               `optional_field ("minimumPrice", `integral);
               `optional_field ("maximumPrice", `integral);
               `optional_field ("limit", `integral);
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
  [@@warning "-39"]

let rec order_query_to_json =
  (let option_to_json t_to_json = function
     | Some x -> t_to_json x
     | None -> (`null : Kxclib.Json.jv)
   and list_to_json t_to_json xs : Kxclib.Json.jv = `arr (List.map t_to_json xs)
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   fun {
         products = x0;
         status = x1;
         minimum_price = x2;
         maximum_price = x3;
         limit = x4;
       } ->
     `obj
       [
         ("products", (option_to_json (list_to_json int_to_json)) x0);
         ("status", (option_to_json (list_to_json order_status_to_json)) x1);
         ("minimumPrice", (option_to_json int_to_json) x2);
         ("maximumPrice", (option_to_json int_to_json) x3);
         ("limit", (option_to_json int_to_json) x4);
       ]
    : order_query -> Kxclib.Json.jv)
  [@@warning "-39"]

and order_query_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl path =
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
        function
        | `obj param ->
            let ( >>= ) = Result.bind in
            List.assoc_opt "products" param
            |> Option.value ~default:`null
            |> (option_of_json' (list_of_json' int_of_json'))
                 (`f "products" :: path)
            >>= fun x0 ->
            List.assoc_opt "status" param
            |> Option.value ~default:`null
            |> (option_of_json'
                  (list_of_json' (fun path x ->
                       order_status_of_json' ~path x
                       |> Result.map_error (fun (msg, path, _) -> (msg, path)))))
                 (`f "status" :: path)
            >>= fun x1 ->
            List.assoc_opt "minimumPrice" param
            |> Option.value ~default:`null
            |> (option_of_json' int_of_json') (`f "minimumPrice" :: path)
            >>= fun x2 ->
            List.assoc_opt "maximumPrice" param
            |> Option.value ~default:`null
            |> (option_of_json' int_of_json') (`f "maximumPrice" :: path)
            >>= fun x3 ->
            List.assoc_opt "limit" param
            |> Option.value ~default:`null
            |> (option_of_json' int_of_json') (`f "limit" :: path)
            >>= fun x4 ->
            Ok
              {
                products = x0;
                status = x1;
                minimum_price = x2;
                maximum_price = x3;
                limit = x4;
              }
        | jv ->
            Error
              ( Printf.sprintf
                  "an object is expected for a record value, but the given is \
                   of type '%s'"
                  (let open Kxclib.Json in
                   string_of_jv_kind (classify_jv jv)),
                path )
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, order_query_json_shape_explanation))
    : order_query Bindoj_runtime.json_full_decoder)
  [@@warning "-39"]

and order_query_of_json =
  (fun x -> order_query_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> order_query option)
  [@@warning "-39"]
