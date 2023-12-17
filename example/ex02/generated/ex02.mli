type nonrec product_id = int

val product_id_reflect : product_id Bindoj_runtime.Refl.t
val product_id_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val product_id_to_json : product_id -> Kxclib.Json.jv
val product_id_of_json' : product_id Bindoj_runtime.json_full_decoder
val product_id_of_json : Kxclib.Json.jv -> product_id option

type product_details = {
  name : string;  (** Product name *)
  description : string;  (** Product description *)
  price : int;  (** Product price *)
  count : int;  (** Count of the product *)
}
(** Product details *)

val product_details_reflect : product_details Bindoj_runtime.Refl.t

val product_details_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val product_details_to_json : product_details -> Kxclib.Json.jv
val product_details_of_json' : product_details Bindoj_runtime.json_full_decoder
val product_details_of_json : Kxclib.Json.jv -> product_details option

type product = { id : product_id;  (** Product ID *) details : product_details }
(** Product *)

val product_reflect : product Bindoj_runtime.Refl.t
val product_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val product_to_json : product -> Kxclib.Json.jv
val product_of_json' : product Bindoj_runtime.json_full_decoder
val product_of_json : Kxclib.Json.jv -> product option

type nonrec order_id = int

val order_id_reflect : order_id Bindoj_runtime.Refl.t
val order_id_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val order_id_to_json : order_id -> Kxclib.Json.jv
val order_id_of_json' : order_id Bindoj_runtime.json_full_decoder
val order_id_of_json : Kxclib.Json.jv -> order_id option

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

val payment_method_reflect : payment_method Bindoj_runtime.Refl.t
val payment_method_json_discriminator_value : payment_method -> string

val payment_method_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val payment_method_to_json : payment_method -> Kxclib.Json.jv
val payment_method_of_json' : payment_method Bindoj_runtime.json_full_decoder
val payment_method_of_json : Kxclib.Json.jv -> payment_method option

type order_details = {
  products : (product_id * int) list;
      (** ID and it's count of ordered products *)
  payment_method : payment_method;  (** Payment method *)
}

val order_details_reflect : order_details Bindoj_runtime.Refl.t
val order_details_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val order_details_to_json : order_details -> Kxclib.Json.jv
val order_details_of_json' : order_details Bindoj_runtime.json_full_decoder
val order_details_of_json : Kxclib.Json.jv -> order_details option

type nonrec order_status =
  [ `Unpaid | `Paid | `Shipped | `Delivered | `Canceled ]
(** Status of an order *)

val order_status_reflect : order_status Bindoj_runtime.Refl.t
val order_status_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val order_status_to_json : order_status -> Kxclib.Json.jv
val order_status_of_json' : order_status Bindoj_runtime.json_full_decoder
val order_status_of_json : Kxclib.Json.jv -> order_status option

type order = {
  id : order_id;  (** Order ID *)
  total_price : int;
  details : order_details;
  status : order_status;
}

val order_reflect : order Bindoj_runtime.Refl.t
val order_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val order_to_json : order -> Kxclib.Json.jv
val order_of_json' : order Bindoj_runtime.json_full_decoder
val order_of_json : Kxclib.Json.jv -> order option

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

val product_query_reflect : product_query Bindoj_runtime.Refl.t
val product_query_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val product_query_to_json : product_query -> Kxclib.Json.jv
val product_query_of_json' : product_query Bindoj_runtime.json_full_decoder
val product_query_of_json : Kxclib.Json.jv -> product_query option

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

val order_query_reflect : order_query Bindoj_runtime.Refl.t
val order_query_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val order_query_to_json : order_query -> Kxclib.Json.jv
val order_query_of_json' : order_query Bindoj_runtime.json_full_decoder
val order_query_of_json : Kxclib.Json.jv -> order_query option
