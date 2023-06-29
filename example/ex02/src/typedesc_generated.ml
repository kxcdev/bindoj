(* Copyright 2022 Kotoi-Xie Consultancy, Inc. This file is a part of the

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
open Bindoj_base
open Bindoj_base.Typed_type_desc
open Bindoj_example_shared_typedesc_generated.Typedesc_generated

include Bindoj_example_ex02_generated.Ex02
include Bindoj_example_ex02_typedesc.Typedesc

let product_id = Typed.mk product_id_decl product_id_reflect
let product_details = Typed.mk product_details_decl product_details_reflect
let product = Typed.mk product_decl product_reflect
let order_id = Typed.mk order_id_decl order_id_reflect
let payment_method = Typed.mk payment_method_decl payment_method_reflect
let order_details = Typed.mk order_details_decl order_details_reflect
let order_status = Typed.mk order_status_decl order_status_reflect
let order = Typed.mk order_decl order_reflect
let product_query = Typed.mk product_query_decl product_query_reflect
let order_query = Typed.mk order_query_decl order_query_reflect

let env =
  let open Bindoj_typedesc.Typed_type_desc in
  { Type_decl_environment.empty with
    alias_ident_typemap =
      StringMap.of_list [
        "product_id", (Boxed product_id);
        "product_details", (Boxed product_details);
        "product", (Boxed product);
        "order_id", (Boxed order_id);
        "payment_method", (Boxed payment_method);
        "order_details", (Boxed order_details);
        "order_status", (Boxed order_status);
        "order", (Boxed order);
        "product_query", (Boxed product_query);
        "order_query", (Boxed order_query);
      ]}

module ProductId : T with type t = product_id = struct
  type t = int [@@deriving show]

  let decl = product_id_decl
  let reflect = product_id_reflect
  let json_shape_explanation = product_id_json_shape_explanation
  let to_json = product_id_to_json
  let of_json' = product_id_of_json'
end

module ProductDetails : T with type t = product_details = struct
  type t = product_details = {
    name: string;
    description: string;
    price: int;
    count: int;
  } [@@deriving show]

  let decl = product_details_decl
  let reflect = product_details_reflect
  let json_shape_explanation = product_details_json_shape_explanation
  let to_json = product_details_to_json
  let of_json' = product_details_of_json'
end

module Product : T with type t = product = struct
  type t = product = {
    id: ProductId.t;
    details: ProductDetails.t;
  } [@@deriving show]

  let decl = product_decl
  let reflect = product_reflect
  let json_shape_explanation = product_json_shape_explanation
  let to_json = product_to_json
  let of_json' = product_of_json'
end

module OrderId : T with type t = order_id = struct
  type t = int [@@deriving show]

  let decl = order_id_decl
  let reflect = order_id_reflect
  let json_shape_explanation = order_id_json_shape_explanation
  let to_json = order_id_to_json
  let of_json' = order_id_of_json'
end

module PaymentMethod : T with type t = payment_method = struct
  type t = payment_method =
    | Credit_card of {
      card_number: string;
      holder_name: string;
      expiration_date: int * int;
      cvv: string;
    }
    | Bank_transfer of {
      account_number: string;
      bank_name: string;
      holder_name: string;
    } [@@deriving show]

  let decl = payment_method_decl
  let reflect = payment_method_reflect
  let json_shape_explanation = payment_method_json_shape_explanation
  let to_json = payment_method_to_json
  let of_json' = payment_method_of_json'
end

module OrderDetails : T with type t = order_details = struct
  type t = order_details = {
    products: (ProductId.t * int) list;
    payment_method: PaymentMethod.t;
  } [@@deriving show]

  let decl = order_details_decl
  let reflect = order_details_reflect
  let json_shape_explanation = order_details_json_shape_explanation
  let to_json = order_details_to_json
  let of_json' = order_details_of_json'
end

module OrderStatus : T with type t = order_status = struct
  type t = [ `Unpaid  | `Paid  | `Shipped  | `Delivered  | `Canceled ] [@@deriving show]

  let decl = order_status_decl
  let reflect = order_status_reflect
  let json_shape_explanation = order_status_json_shape_explanation
  let to_json = order_status_to_json
  let of_json' = order_status_of_json'
end

module Order : T with type t = order = struct
  type t = order = {
    id: OrderId.t;
    total_price: int;
    details: OrderDetails.t;
    status: OrderStatus.t;
  } [@@deriving show]

  let decl = order_decl
  let reflect = order_reflect
  let json_shape_explanation = order_json_shape_explanation
  let to_json = order_to_json
  let of_json' = order_of_json'
end

module ProductQuery : T with type t = product_query = struct
  type t = product_query = {
    searchQuery: string option;
    minimum_price: int option;
    maximum_price: int option;
    limit: int option;
  } [@@deriving show]

  let decl = product_query_decl
  let reflect = product_query_reflect
  let json_shape_explanation = product_query_json_shape_explanation
  let to_json = product_query_to_json
  let of_json' = product_query_of_json'
end

module OrderQuery : T with type t = order_query = struct
  type t = order_query = {
    products: int list option;
    status: OrderStatus.t list option;
    minimum_price: int option;
    maximum_price: int option;
    limit: int option;
  } [@@deriving show]

  let decl = order_query_decl
  let reflect = order_query_reflect
  let json_shape_explanation = order_query_json_shape_explanation
  let to_json = order_query_to_json
  let of_json' = order_query_of_json'
end
