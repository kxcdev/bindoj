(* Copyright 2022-2023 Kotoi-Xie Consultancy, Inc. This file is a part of the

==== Bindoj (https://kxc.dev/bindoj) ====

software project that is developed, maintained, and distributed by
Kotoi-Xie Consultancy, Inc. (https://kxc.inc) which is also known as KXC.

Licensed under the Apache License, Version 2.0 (the "License"); you may not
use this file except in compliance with the License. You may obtain a copy
of the License at http://www.apache.org/licenses/LICENSE-2.0. Unless required
by applicable law or agreed to in writing, software distributed under the
License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS
OF ANY KIND, either express or implied. See the License for the specific
language governing permissions and limitations under the License.
                                                                              *)
(* Acknowledgements  --- AnchorZ Inc. ---  The current/initial version or a
significant portion of this file is developed under the funding provided by
AnchorZ Inc. to satisfy its needs in its product development workflow.
                                                                              *)
open Bindoj_base.Type_desc
open Bindoj_gen_config

open struct
  let cty_int = Coretype.mk_prim `int
  let cty_string = Coretype.mk_prim `string
  let cty_int_opt = Coretype.(mk_option (prim `int))
  let cty_string_opt = Coretype.(mk_option (prim `string))

  let cty_ident td = Coretype.mk_ident td.td_name
end

let product_id_decl = alias_decl "product_id" Coretype.(mk_prim `int)

let product_details_decl = record_decl "product_details" [
  record_field "name" cty_string ~doc:(`docstr "Product name");
  record_field "description" cty_string ~doc:(`docstr "Product description");
  record_field "price" cty_int ~doc:(`docstr "Product price");
  record_field "count" cty_int ~doc:(`docstr "Count of the product");
] ~doc:(`docstr "Product details")

let product_decl = record_decl "product" [
  record_field "id" (cty_ident product_id_decl) ~doc:(`docstr "Product ID");
  record_field "details" (cty_ident product_details_decl);
] ~doc:(`docstr "Product")

let order_id_decl = alias_decl "order_id" Coretype.(mk_prim `int)

let payment_method_decl = variant_decl "payment_method" [
  variant_constructor "Credit_card" (`inline_record [
    record_field "card_number" cty_string ~doc:(`docstr "Card number");
    record_field "holder_name" cty_string ~doc:(`docstr "Card holder name");
    record_field "expiration_date"
      Coretype.(mk_tuple [
        prim `int; prim `int
        ] ~configs:[
        Json_config.tuple_style (`obj `default)
      ])
      ~doc:(`docstr "Expiration date");
    record_field "cvv" cty_string ~doc:(`docstr "Card CVV");
  ]) ~doc:(`docstr "Payment by credit card");

  variant_constructor "Bank_transfer" (`inline_record [
    record_field "account_number" cty_string ~doc:(`docstr "Account number");
    record_field "bank_name" cty_string ~doc:(`docstr "Bank name");
    record_field "holder_name" cty_string ~doc:(`docstr "Account holder name");
  ]) ~doc:(`docstr "Payment by bank transer");
] ~doc:(`docstr "Payment method of an order")

let order_details_decl = record_decl "order_details" [
  record_field "products"
    Coretype.(tuple [
      ident product_id_decl.td_name; prim `int
      ] |> mk_list ~configs:[
      Json_config.tuple_style (`obj `default)
    ])
    ~doc:(`docstr "ID and it's count of ordered products");
  record_field "payment_method" (cty_ident payment_method_decl) ~doc:(`docstr "Payment method");
]

let order_status_decl = alias_decl "order_status" (
  Coretype.(mk_string_enum [
    string_enum_case "Unpaid";
    string_enum_case "Paid";
    string_enum_case "Shipped";
    string_enum_case "Delivered";
    string_enum_case "Canceled";
  ])
) ~doc:(`docstr "Status of an order")

let order_decl = record_decl "order" [
  record_field "id" (cty_ident order_id_decl) ~doc:(`docstr "Order ID");
  record_field "total_price" cty_int;
  record_field "details" (cty_ident order_details_decl);
  record_field "status" (cty_ident order_status_decl);
]

let product_query_decl = record_decl "product_query" [
  record_field "searchQuery" cty_string_opt
    ~doc:(`docstr "Optional search string to match product names or descriptions");
  record_field "minimum_price" cty_int_opt
    ~doc:(`docstr "Optional minimum price constraint for a product");
  record_field "maximum_price" cty_int_opt
    ~doc:(`docstr "Optional maximum price constraint for a product");
  record_field "limit" cty_int_opt
    ~doc:(`docstr "Number limit of data to be acquired.");
] ~doc:(`docstr "Query to search products")

let order_query_decl = record_decl "order_query" [
  record_field "products" Coretype.(prim `int |> list |> mk_option)
    ~doc:(`docstr "List of product IDs in the order");
  record_field "status" Coretype.(ident order_status_decl.td_name |> list |> mk_option)
    ~doc:(`docstr "Optional order status constraint");
  record_field "minimum_price" cty_int_opt
    ~doc:(`docstr "Optional minimum total price constraint for the order");
  record_field "maximum_price" cty_int_opt
    ~doc:(`docstr "Optional maximum total price constraint for the order");
  record_field "limit" cty_int_opt
    ~doc:(`docstr "Number limit of data to be acquired.");
] ~doc:(`docstr "Query to search orders")

let decls : (string * type_decl) list = [
  "product_id_decl", product_id_decl;
  "product_details_decl", product_details_decl;
  "product_decl", product_decl;
  "order_id_decl", order_id_decl;
  "payment_method_decl", payment_method_decl;
  "order_details_decl", order_details_decl;
  "order_status_decl", order_status_decl;
  "order_decl", order_decl;
  "product_query_decl", product_query_decl;
  "order_query_decl", order_query_decl;
]
