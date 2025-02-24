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
  (let int_to_json (x : int) = (`num (float_of_int x) : Kxclib.Json.jv) in
   int_to_json
    : product_id -> Kxclib.Json.jv)
[@@warning "-39"]

and product_id_of_json' =
  (fun ?(path = []) ->
     fun x ->
      (let rec of_json_impl =
         let int_of_json' path = function
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
         int_of_json'
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
  name : string;  (** Product name *)
  description : string;  (** Product description *)
  price : int;  (** Product price *)
  count : int;  (** Count of the product *)
}
(** Product details *)

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
  (let string_to_json (x : string) = (`str x : Kxclib.Json.jv)
   and int_to_json (x : int) = (`num (float_of_int x) : Kxclib.Json.jv) in
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
                List.assoc_opt "name" param
                |> Option.to_result
                     ~none:("mandatory field 'name' does not exist", path)
                >>= string_of_json' (`f "name" :: path)
                >>= fun x0 ->
                List.assoc_opt "description" param
                |> Option.to_result
                     ~none:("mandatory field 'description' does not exist", path)
                >>= string_of_json' (`f "description" :: path)
                >>= fun x1 ->
                List.assoc_opt "price" param
                |> Option.to_result
                     ~none:("mandatory field 'price' does not exist", path)
                >>= int_of_json' (`f "price" :: path)
                >>= fun x2 ->
                List.assoc_opt "count" param
                |> Option.to_result
                     ~none:("mandatory field 'count' does not exist", path)
                >>= int_of_json' (`f "count" :: path)
                >>= fun x3 ->
                Ok { name = x0; description = x1; price = x2; count = x3 }
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
             (msg, path, product_details_json_shape_explanation))
    : product_details Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and product_details_of_json =
  (fun x -> product_details_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> product_details option)
[@@warning "-39"]

type product = { id : product_id;  (** Product ID *) details : product_details }
(** Product *)

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
                 ("id", Expr.of_int id);
                 ("details", (Expr.of_refl product_details_reflect) details);
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "id" >>= Expr.to_int >>= fun id ->
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
               `mandatory_field ("id", `named ("ProductId", `integral));
               `mandatory_field
                 ( "details",
                   `named
                     ( "ProductDetails",
                       `object_of
                         [
                           `mandatory_field ("name", `string);
                           `mandatory_field ("description", `string);
                           `mandatory_field ("price", `integral);
                           `mandatory_field ("count", `integral);
                         ] ) );
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec product_to_json =
  (let string_to_json (x : string) = (`str x : Kxclib.Json.jv)
   and int_to_json (x : int) = (`num (float_of_int x) : Kxclib.Json.jv) in
   let rec product_details_to_json_nested =
     (fun { name = x0; description = x1; price = x2; count = x3 } ->
        [
          ("name", string_to_json x0);
          ("description", string_to_json x1);
          ("price", int_to_json x2);
          ("count", int_to_json x3);
        ]
       : product_details -> (string * Kxclib.Json.jv) list)
   in
   fun { id = x0; details = x1 } ->
     `obj
       [
         ("id", int_to_json x0);
         ("details", `obj (product_details_to_json_nested x1));
       ]
    : product -> Kxclib.Json.jv)
[@@warning "-39"]

and product_of_json' =
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
         let rec product_details_of_json_nested path __bindoj_orig =
           match __bindoj_orig with
           | `obj param ->
               let ( >>= ) = Result.bind in
               List.assoc_opt "name" param
               |> Option.to_result
                    ~none:("mandatory field 'name' does not exist", path)
               >>= string_of_json' (`f "name" :: path)
               >>= fun x0 ->
               List.assoc_opt "description" param
               |> Option.to_result
                    ~none:("mandatory field 'description' does not exist", path)
               >>= string_of_json' (`f "description" :: path)
               >>= fun x1 ->
               List.assoc_opt "price" param
               |> Option.to_result
                    ~none:("mandatory field 'price' does not exist", path)
               >>= int_of_json' (`f "price" :: path)
               >>= fun x2 ->
               List.assoc_opt "count" param
               |> Option.to_result
                    ~none:("mandatory field 'count' does not exist", path)
               >>= int_of_json' (`f "count" :: path)
               >>= fun x3 ->
               Ok
                 ({ name = x0; description = x1; price = x2; count = x3 }
                   : product_details)
           | jv ->
               Error
                 ( Printf.sprintf
                     "an object is expected for a record value, but the given \
                      is of type '%s'"
                     (let open Kxclib.Json in
                      string_of_jv_kind (classify_jv jv)),
                   path )
         in
         fun path ->
           fun __bindoj_orig ->
            match __bindoj_orig with
            | `obj param ->
                let ( >>= ) = Result.bind in
                List.assoc_opt "id" param
                |> Option.to_result
                     ~none:("mandatory field 'id' does not exist", path)
                >>= int_of_json' (`f "id" :: path)
                >>= fun x0 ->
                List.assoc_opt "details" param
                |> Option.to_result
                     ~none:("mandatory field 'details' does not exist", path)
                >>= product_details_of_json_nested (`f "details" :: path)
                >>= fun x1 -> Ok { id = x0; details = x1 }
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
  (let int_to_json (x : int) = (`num (float_of_int x) : Kxclib.Json.jv) in
   int_to_json
    : order_id -> Kxclib.Json.jv)
[@@warning "-39"]

and order_id_of_json' =
  (fun ?(path = []) ->
     fun x ->
      (let rec of_json_impl =
         let int_of_json' path = function
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
         int_of_json'
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

(** Payment method of an order *)
type payment_method =
  | Credit_card of {
      card_number : string;  (** Card number *)
      holder_name : string;  (** Card holder name *)
      expiration_date : int * int;  (** Expiration date *)
      cvv : string;  (** Card CVV *)
    }  (** Payment by credit card *)
  | Bank_transfer of {
      account_number : string;  (** Account number *)
      bank_name : string;  (** Bank name *)
      holder_name : string;  (** Account holder name *)
    }  (** Payment by bank transer *)

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
             | _ -> invalid_arg "Credit_card is expected");
           mk =
             (fun xs ->
               xs |> StringMap.find_opt "card_number" >>= Expr.to_string
               >>= fun card_number ->
               xs |> StringMap.find_opt "holder_name" >>= Expr.to_string
               >>= fun holder_name ->
               ( xs |> StringMap.find_opt "expiration_date" >>= function
                 | Expr.Tuple [ x0; x1 ] ->
                     Expr.to_int x0 >>= fun x0 ->
                     Expr.to_int x1 >>= fun x1 -> Some (x0, x1)
                 | _ -> None )
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
             | _ -> invalid_arg "Bank_transfer is expected");
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
   | Credit_card _ -> "credit-card"
   | Bank_transfer _ -> "bank-transfer"
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
                     ( "expirationDate",
                       `object_of
                         [
                           `mandatory_field ("_0", `integral);
                           `mandatory_field ("_1", `integral);
                         ] );
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
  (let string_to_json (x : string) = (`str x : Kxclib.Json.jv)
   and int_to_json (x : int) = (`num (float_of_int x) : Kxclib.Json.jv) in
   function
   | Credit_card
       { card_number = x0; holder_name = x1; expiration_date = x2; cvv = x3 } ->
       `obj
         [
           ("kind", `str "credit-card");
           ("cardNumber", string_to_json x0);
           ("holderName", string_to_json x1);
           ( "expirationDate",
             (fun (x0, x1) ->
               (`obj [ ("_0", int_to_json x0); ("_1", int_to_json x1) ]
                 : Kxclib.Json.jv))
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
            match Kxclib.Jv.pump_field "kind" __bindoj_orig with
            | `obj (("kind", `str "credit-card") :: param) ->
                let ( >>= ) = Result.bind in
                List.assoc_opt "cardNumber" param
                |> Option.to_result
                     ~none:("mandatory field 'cardNumber' does not exist", path)
                >>= string_of_json' (`f "cardNumber" :: path)
                >>= fun x0 ->
                List.assoc_opt "holderName" param
                |> Option.to_result
                     ~none:("mandatory field 'holderName' does not exist", path)
                >>= string_of_json' (`f "holderName" :: path)
                >>= fun x1 ->
                List.assoc_opt "expirationDate" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'expirationDate' does not exist", path)
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
                      (`f "expirationDate" :: path)
                >>= fun x2 ->
                List.assoc_opt "cvv" param
                |> Option.to_result
                     ~none:("mandatory field 'cvv' does not exist", path)
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
                |> Option.to_result
                     ~none:
                       ("mandatory field 'accountNumber' does not exist", path)
                >>= string_of_json' (`f "accountNumber" :: path)
                >>= fun x0 ->
                List.assoc_opt "bankName" param
                |> Option.to_result
                     ~none:("mandatory field 'bankName' does not exist", path)
                >>= string_of_json' (`f "bankName" :: path)
                >>= fun x1 ->
                List.assoc_opt "holderName" param
                |> Option.to_result
                     ~none:("mandatory field 'holderName' does not exist", path)
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
                      "a string is expected for a variant discriminator, but \
                       the given is of type '%s'"
                      (let open Kxclib.Json in
                       string_of_jv_kind (classify_jv jv)),
                    `f "kind" :: path )
            | `obj _ -> Error ("discriminator field 'kind' does not exist", path)
            | jv ->
                Error
                  ( Printf.sprintf
                      "an object is expected for a variant value, but the \
                       given is of type '%s'"
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
      (** ID and it's count of ordered products *)
  payment_method : payment_method;  (** Payment method *)
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
                     (`object_of
                        [
                          `mandatory_field
                            ( "_0",
                              match product_id_json_shape_explanation with
                              | `with_warning (_, (`named _ as s)) -> s
                              | `with_warning (_, s) | s ->
                                  `named ("ProductId", s) );
                          `mandatory_field ("_1", `integral);
                        ]) );
               `mandatory_field
                 ( "paymentMethod",
                   `named
                     ( "PaymentMethod",
                       `anyone_of
                         [
                           `object_of
                             [
                               `mandatory_field
                                 ("kind", `exactly (`str "credit-card"));
                               `mandatory_field ("cardNumber", `string);
                               `mandatory_field ("holderName", `string);
                               `mandatory_field
                                 ( "expirationDate",
                                   `object_of
                                     [
                                       `mandatory_field ("_0", `integral);
                                       `mandatory_field ("_1", `integral);
                                     ] );
                               `mandatory_field ("cvv", `string);
                             ];
                           `object_of
                             [
                               `mandatory_field
                                 ("kind", `exactly (`str "bank-transfer"));
                               `mandatory_field ("accountNumber", `string);
                               `mandatory_field ("bankName", `string);
                               `mandatory_field ("holderName", `string);
                             ];
                         ] ) );
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec order_details_to_json =
  (let string_to_json (x : string) = (`str x : Kxclib.Json.jv)
   and list_to_json t_to_json xs =
     (`arr (List.map t_to_json xs) : Kxclib.Json.jv)
   and int_to_json (x : int) = (`num (float_of_int x) : Kxclib.Json.jv) in
   let rec payment_method_to_json_nested =
     (function
      | Credit_card
          { card_number = x0; holder_name = x1; expiration_date = x2; cvv = x3 }
        ->
          [
            ("kind", `str "credit-card");
            ("cardNumber", string_to_json x0);
            ("holderName", string_to_json x1);
            ( "expirationDate",
              (fun (x0, x1) ->
                (`obj [ ("_0", int_to_json x0); ("_1", int_to_json x1) ]
                  : Kxclib.Json.jv))
                x2 );
            ("cvv", string_to_json x3);
          ]
      | Bank_transfer { account_number = x0; bank_name = x1; holder_name = x2 }
        ->
          [
            ("kind", `str "bank-transfer");
            ("accountNumber", string_to_json x0);
            ("bankName", string_to_json x1);
            ("holderName", string_to_json x2);
          ]
       : payment_method -> (string * Kxclib.Json.jv) list)
   in
   fun { products = x0; payment_method = x1 } ->
     `obj
       [
         ( "products",
           (list_to_json (fun (x0, x1) ->
                (`obj [ ("_0", product_id_to_json x0); ("_1", int_to_json x1) ]
                  : Kxclib.Json.jv)))
             x0 );
         ("paymentMethod", `obj (payment_method_to_json_nested x1));
       ]
    : order_details -> Kxclib.Json.jv)
[@@warning "-39"]

and order_details_of_json' =
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
         let rec payment_method_of_json_nested path __bindoj_orig =
           match Kxclib.Jv.pump_field "kind" __bindoj_orig with
           | `obj (("kind", `str "credit-card") :: param) ->
               let ( >>= ) = Result.bind in
               List.assoc_opt "cardNumber" param
               |> Option.to_result
                    ~none:("mandatory field 'cardNumber' does not exist", path)
               >>= string_of_json' (`f "cardNumber" :: path)
               >>= fun x0 ->
               List.assoc_opt "holderName" param
               |> Option.to_result
                    ~none:("mandatory field 'holderName' does not exist", path)
               >>= string_of_json' (`f "holderName" :: path)
               >>= fun x1 ->
               List.assoc_opt "expirationDate" param
               |> Option.to_result
                    ~none:
                      ("mandatory field 'expirationDate' does not exist", path)
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
                     (`f "expirationDate" :: path)
               >>= fun x2 ->
               List.assoc_opt "cvv" param
               |> Option.to_result
                    ~none:("mandatory field 'cvv' does not exist", path)
               >>= string_of_json' (`f "cvv" :: path)
               >>= fun x3 ->
               Ok
                 (Credit_card
                    {
                      card_number = x0;
                      holder_name = x1;
                      expiration_date = x2;
                      cvv = x3;
                    }
                   : payment_method)
           | `obj (("kind", `str "bank-transfer") :: param) ->
               let ( >>= ) = Result.bind in
               List.assoc_opt "accountNumber" param
               |> Option.to_result
                    ~none:
                      ("mandatory field 'accountNumber' does not exist", path)
               >>= string_of_json' (`f "accountNumber" :: path)
               >>= fun x0 ->
               List.assoc_opt "bankName" param
               |> Option.to_result
                    ~none:("mandatory field 'bankName' does not exist", path)
               >>= string_of_json' (`f "bankName" :: path)
               >>= fun x1 ->
               List.assoc_opt "holderName" param
               |> Option.to_result
                    ~none:("mandatory field 'holderName' does not exist", path)
               >>= string_of_json' (`f "holderName" :: path)
               >>= fun x2 ->
               Ok
                 (Bank_transfer
                    { account_number = x0; bank_name = x1; holder_name = x2 }
                   : payment_method)
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
                     "a string is expected for a variant discriminator, but \
                      the given is of type '%s'"
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
         fun path ->
           fun __bindoj_orig ->
            match __bindoj_orig with
            | `obj param ->
                let ( >>= ) = Result.bind in
                List.assoc_opt "products" param
                |> Option.to_result
                     ~none:("mandatory field 'products' does not exist", path)
                >>= (list_of_json' (fun path -> function
                      | (`obj fields : Kxclib.Json.jv) ->
                          let ( >>= ) = Result.bind in
                          List.assoc_opt "_0" fields
                          |> Option.to_result
                               ~none:
                                 ("mandatory field '_0' does not exist", path)
                          >>= (fun path ->
                                fun x ->
                                 product_id_of_json' ~path x
                                 |> Result.map_error (fun (msg, path, _) ->
                                        (msg, path)))
                                (`f "_0" :: path)
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
                              path )))
                      (`f "products" :: path)
                >>= fun x0 ->
                List.assoc_opt "paymentMethod" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'paymentMethod' does not exist", path)
                >>= payment_method_of_json_nested (`f "paymentMethod" :: path)
                >>= fun x1 -> Ok { products = x0; payment_method = x1 }
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
             (msg, path, order_details_json_shape_explanation))
    : order_details Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and order_details_of_json =
  (fun x -> order_details_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> order_details option)
[@@warning "-39"]

type nonrec order_status =
  [ `Unpaid | `Paid | `Shipped | `Delivered | `Canceled ]
(** Status of an order *)

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
  (fun ?(path = []) ->
     fun x ->
      (let rec of_json_impl path = function
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
                 path )
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
  id : order_id;  (** Order ID *)
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
                 ("id", Expr.of_int id);
                 ("total_price", Expr.of_int total_price);
                 ("details", (Expr.of_refl order_details_reflect) details);
                 ( "status",
                   (function
                     | `Unpaid -> Expr.StringEnum "Unpaid"
                     | `Paid -> Expr.StringEnum "Paid"
                     | `Shipped -> Expr.StringEnum "Shipped"
                     | `Delivered -> Expr.StringEnum "Delivered"
                     | `Canceled -> Expr.StringEnum "Canceled")
                     status );
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "id" >>= Expr.to_int >>= fun id ->
             xs |> StringMap.find_opt "total_price" >>= Expr.to_int
             >>= fun total_price ->
             xs
             |> StringMap.find_opt "details"
             >>= Expr.to_refl order_details_reflect
             >>= fun details ->
             ( xs |> StringMap.find_opt "status" >>= function
               | Expr.StringEnum "Unpaid" -> Some `Unpaid
               | Expr.StringEnum "Paid" -> Some `Paid
               | Expr.StringEnum "Shipped" -> Some `Shipped
               | Expr.StringEnum "Delivered" -> Some `Delivered
               | Expr.StringEnum "Canceled" -> Some `Canceled
               | _ -> None )
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
               `mandatory_field ("id", `named ("OrderId", `integral));
               `mandatory_field ("totalPrice", `integral);
               `mandatory_field
                 ( "details",
                   `named
                     ( "OrderDetails",
                       `object_of
                         [
                           `mandatory_field
                             ( "products",
                               `array_of
                                 (`object_of
                                    [
                                      `mandatory_field
                                        ( "_0",
                                          match
                                            product_id_json_shape_explanation
                                          with
                                          | `with_warning (_, (`named _ as s))
                                            ->
                                              s
                                          | `with_warning (_, s) | s ->
                                              `named ("ProductId", s) );
                                      `mandatory_field ("_1", `integral);
                                    ]) );
                           `mandatory_field
                             ( "paymentMethod",
                               `named
                                 ( "PaymentMethod",
                                   `anyone_of
                                     [
                                       `object_of
                                         [
                                           `mandatory_field
                                             ( "kind",
                                               `exactly (`str "credit-card") );
                                           `mandatory_field
                                             ("cardNumber", `string);
                                           `mandatory_field
                                             ("holderName", `string);
                                           `mandatory_field
                                             ( "expirationDate",
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ("_0", `integral);
                                                   `mandatory_field
                                                     ("_1", `integral);
                                                 ] );
                                           `mandatory_field ("cvv", `string);
                                         ];
                                       `object_of
                                         [
                                           `mandatory_field
                                             ( "kind",
                                               `exactly (`str "bank-transfer")
                                             );
                                           `mandatory_field
                                             ("accountNumber", `string);
                                           `mandatory_field ("bankName", `string);
                                           `mandatory_field
                                             ("holderName", `string);
                                         ];
                                     ] ) );
                         ] ) );
               `mandatory_field
                 ( "status",
                   `named
                     ( "OrderStatus",
                       `string_enum
                         [
                           "Unpaid"; "Paid"; "Shipped"; "Delivered"; "Canceled";
                         ] ) );
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec order_to_json =
  (let string_to_json (x : string) = (`str x : Kxclib.Json.jv)
   and list_to_json t_to_json xs =
     (`arr (List.map t_to_json xs) : Kxclib.Json.jv)
   and int_to_json (x : int) = (`num (float_of_int x) : Kxclib.Json.jv) in
   let rec order_details_to_json_nested =
     (fun { products = x0; payment_method = x1 } ->
        [
          ( "products",
            (list_to_json (fun (x0, x1) ->
                 (`obj [ ("_0", product_id_to_json x0); ("_1", int_to_json x1) ]
                   : Kxclib.Json.jv)))
              x0 );
          ("paymentMethod", `obj (payment_method_to_json_nested x1));
        ]
       : order_details -> (string * Kxclib.Json.jv) list)
   and payment_method_to_json_nested =
     (function
      | Credit_card
          { card_number = x0; holder_name = x1; expiration_date = x2; cvv = x3 }
        ->
          [
            ("kind", `str "credit-card");
            ("cardNumber", string_to_json x0);
            ("holderName", string_to_json x1);
            ( "expirationDate",
              (fun (x0, x1) ->
                (`obj [ ("_0", int_to_json x0); ("_1", int_to_json x1) ]
                  : Kxclib.Json.jv))
                x2 );
            ("cvv", string_to_json x3);
          ]
      | Bank_transfer { account_number = x0; bank_name = x1; holder_name = x2 }
        ->
          [
            ("kind", `str "bank-transfer");
            ("accountNumber", string_to_json x0);
            ("bankName", string_to_json x1);
            ("holderName", string_to_json x2);
          ]
       : payment_method -> (string * Kxclib.Json.jv) list)
   in
   fun { id = x0; total_price = x1; details = x2; status = x3 } ->
     `obj
       [
         ("id", int_to_json x0);
         ("totalPrice", int_to_json x1);
         ("details", `obj (order_details_to_json_nested x2));
         ( "status",
           (function
             | `Unpaid -> `str "Unpaid"
             | `Paid -> `str "Paid"
             | `Shipped -> `str "Shipped"
             | `Delivered -> `str "Delivered"
             | `Canceled -> `str "Canceled")
             x3 );
       ]
    : order -> Kxclib.Json.jv)
[@@warning "-39"]

and order_of_json' =
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
         let rec order_details_of_json_nested path __bindoj_orig =
           match __bindoj_orig with
           | `obj param ->
               let ( >>= ) = Result.bind in
               List.assoc_opt "products" param
               |> Option.to_result
                    ~none:("mandatory field 'products' does not exist", path)
               >>= (list_of_json' (fun path -> function
                     | (`obj fields : Kxclib.Json.jv) ->
                         let ( >>= ) = Result.bind in
                         List.assoc_opt "_0" fields
                         |> Option.to_result
                              ~none:("mandatory field '_0' does not exist", path)
                         >>= (fun path ->
                               fun x ->
                                product_id_of_json' ~path x
                                |> Result.map_error (fun (msg, path, _) ->
                                       (msg, path)))
                               (`f "_0" :: path)
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
                             path )))
                     (`f "products" :: path)
               >>= fun x0 ->
               List.assoc_opt "paymentMethod" param
               |> Option.to_result
                    ~none:
                      ("mandatory field 'paymentMethod' does not exist", path)
               >>= payment_method_of_json_nested (`f "paymentMethod" :: path)
               >>= fun x1 ->
               Ok ({ products = x0; payment_method = x1 } : order_details)
           | jv ->
               Error
                 ( Printf.sprintf
                     "an object is expected for a record value, but the given \
                      is of type '%s'"
                     (let open Kxclib.Json in
                      string_of_jv_kind (classify_jv jv)),
                   path )
         and payment_method_of_json_nested path __bindoj_orig =
           match Kxclib.Jv.pump_field "kind" __bindoj_orig with
           | `obj (("kind", `str "credit-card") :: param) ->
               let ( >>= ) = Result.bind in
               List.assoc_opt "cardNumber" param
               |> Option.to_result
                    ~none:("mandatory field 'cardNumber' does not exist", path)
               >>= string_of_json' (`f "cardNumber" :: path)
               >>= fun x0 ->
               List.assoc_opt "holderName" param
               |> Option.to_result
                    ~none:("mandatory field 'holderName' does not exist", path)
               >>= string_of_json' (`f "holderName" :: path)
               >>= fun x1 ->
               List.assoc_opt "expirationDate" param
               |> Option.to_result
                    ~none:
                      ("mandatory field 'expirationDate' does not exist", path)
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
                     (`f "expirationDate" :: path)
               >>= fun x2 ->
               List.assoc_opt "cvv" param
               |> Option.to_result
                    ~none:("mandatory field 'cvv' does not exist", path)
               >>= string_of_json' (`f "cvv" :: path)
               >>= fun x3 ->
               Ok
                 (Credit_card
                    {
                      card_number = x0;
                      holder_name = x1;
                      expiration_date = x2;
                      cvv = x3;
                    }
                   : payment_method)
           | `obj (("kind", `str "bank-transfer") :: param) ->
               let ( >>= ) = Result.bind in
               List.assoc_opt "accountNumber" param
               |> Option.to_result
                    ~none:
                      ("mandatory field 'accountNumber' does not exist", path)
               >>= string_of_json' (`f "accountNumber" :: path)
               >>= fun x0 ->
               List.assoc_opt "bankName" param
               |> Option.to_result
                    ~none:("mandatory field 'bankName' does not exist", path)
               >>= string_of_json' (`f "bankName" :: path)
               >>= fun x1 ->
               List.assoc_opt "holderName" param
               |> Option.to_result
                    ~none:("mandatory field 'holderName' does not exist", path)
               >>= string_of_json' (`f "holderName" :: path)
               >>= fun x2 ->
               Ok
                 (Bank_transfer
                    { account_number = x0; bank_name = x1; holder_name = x2 }
                   : payment_method)
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
                     "a string is expected for a variant discriminator, but \
                      the given is of type '%s'"
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
         fun path ->
           fun __bindoj_orig ->
            match __bindoj_orig with
            | `obj param ->
                let ( >>= ) = Result.bind in
                List.assoc_opt "id" param
                |> Option.to_result
                     ~none:("mandatory field 'id' does not exist", path)
                >>= int_of_json' (`f "id" :: path)
                >>= fun x0 ->
                List.assoc_opt "totalPrice" param
                |> Option.to_result
                     ~none:("mandatory field 'totalPrice' does not exist", path)
                >>= int_of_json' (`f "totalPrice" :: path)
                >>= fun x1 ->
                List.assoc_opt "details" param
                |> Option.to_result
                     ~none:("mandatory field 'details' does not exist", path)
                >>= order_details_of_json_nested (`f "details" :: path)
                >>= fun x2 ->
                List.assoc_opt "status" param
                |> Option.to_result
                     ~none:("mandatory field 'status' does not exist", path)
                >>= (fun path -> function
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
                                      "given string '%s' is not one of [ \
                                       'Unpaid', 'Paid', 'Shipped', \
                                       'Delivered', 'Canceled' ]"
                                      s,
                                    path ))
                            s
                      | jv ->
                          Error
                            ( Printf.sprintf
                                "expecting type 'string' but the given is of \
                                 type '%s'"
                                (let open Kxclib.Json in
                                 string_of_jv_kind (classify_jv jv)),
                              path ))
                      (`f "status" :: path)
                >>= fun x3 ->
                Ok { id = x0; total_price = x1; details = x2; status = x3 }
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
             (msg, path, order_json_shape_explanation))
    : order Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and order_of_json =
  (fun x -> order_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> order option)
[@@warning "-39"]

type product_query = {
  searchQuery : string option;
      (** Optional search string to match product names or descriptions *)
  minimum_price : int option;
      (** Optional minimum price constraint for a product *)
  maximum_price : int option;
      (** Optional maximum price constraint for a product *)
  limit : int option;  (** Number limit of data to be acquired. *)
}
(** Query to search products *)

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
  (let string_to_json (x : string) = (`str x : Kxclib.Json.jv)
   and int_to_json (x : int) = (`num (float_of_int x) : Kxclib.Json.jv) in
   fun { searchQuery = x0; minimum_price = x1; maximum_price = x2; limit = x3 }
     ->
     `obj
       (List.filter_map
          (fun x -> x)
          [
            Option.map (fun x0 -> ("searchQuery", string_to_json x0)) x0;
            Option.map (fun x1 -> ("minimumPrice", int_to_json x1)) x1;
            Option.map (fun x2 -> ("maximumPrice", int_to_json x2)) x2;
            Option.map (fun x3 -> ("limit", int_to_json x3)) x3;
          ])
    : product_query -> Kxclib.Json.jv)
[@@warning "-39"]

and product_query_of_json' =
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
                      "an object is expected for a record value, but the given \
                       is of type '%s'"
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
  products : int list option;  (** List of product IDs in the order *)
  status : order_status list option;  (** Optional order status constraint *)
  minimum_price : int option;
      (** Optional minimum total price constraint for the order *)
  maximum_price : int option;
      (** Optional maximum total price constraint for the order *)
  limit : int option;  (** Number limit of data to be acquired. *)
}
(** Query to search orders *)

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
  (let list_to_json t_to_json xs =
     (`arr (List.map t_to_json xs) : Kxclib.Json.jv)
   and int_to_json (x : int) = (`num (float_of_int x) : Kxclib.Json.jv) in
   fun {
         products = x0;
         status = x1;
         minimum_price = x2;
         maximum_price = x3;
         limit = x4;
       } ->
     `obj
       (List.filter_map
          (fun x -> x)
          [
            Option.map
              (fun x0 -> ("products", (list_to_json int_to_json) x0))
              x0;
            Option.map
              (fun x1 -> ("status", (list_to_json order_status_to_json) x1))
              x1;
            Option.map (fun x2 -> ("minimumPrice", int_to_json x2)) x2;
            Option.map (fun x3 -> ("maximumPrice", int_to_json x3)) x3;
            Option.map (fun x4 -> ("limit", int_to_json x4)) x4;
          ])
    : order_query -> Kxclib.Json.jv)
[@@warning "-39"]

and order_query_of_json' =
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
                List.assoc_opt "products" param
                |> Option.value ~default:`null
                |> (option_of_json' (list_of_json' int_of_json'))
                     (`f "products" :: path)
                >>= fun x0 ->
                List.assoc_opt "status" param
                |> Option.value ~default:`null
                |> (option_of_json'
                      (list_of_json' (fun path ->
                           fun x ->
                            order_status_of_json' ~path x
                            |> Result.map_error (fun (msg, path, _) ->
                                   (msg, path)))))
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
                      "an object is expected for a record value, but the given \
                       is of type '%s'"
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
