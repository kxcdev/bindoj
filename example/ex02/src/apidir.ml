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
open Bindoj_apidir_shared
open Bindoj_base.Typed_type_desc
open Bindoj_typedesc
open Bindoj_gen_config

open Bindoj_example_shared_apidir.Apidir

module Types = struct
  include Bindoj_example_ex02_typedesc_generated.Typedesc_generated

  open struct
    let to_ttd ?configs ?self name = Coretypes.to_typed_type_decl ~env ?configs ?self name

    let with_id (id_ttd: 'a typed_type_decl) (ttd: 'b typed_type_decl): ('a * 'b) typed_type_decl =
      let { td_name = id_name; _ } = Typed.decl id_ttd in
      let { td_name = name; _ } = Typed.decl ttd in
      Coretypes.(
        Tuple.tup2 (ident id_name, of_ ttd)
        |> with_config [
          Json_config.tuple_style (`obj `default);
        ]
      )
      |> to_ttd (name^"_with_id")
  end

  let int = Coretypes.(Prims.int) |> to_ttd "int"
  let string = Coretypes.Prims.string |> to_ttd "string"

  let product_list = Coretypes.(of_ product |> list) |> to_ttd "product_list"
  let order_list = Coretypes.(of_ order |> list) |> to_ttd "order_list"

  let product_details_with_id = with_id product_id product_details
  let order_status_with_id = with_id order_id order_status
end

open struct
  module R = MakeRegistry()
  module T = Types

  let error_response_case () =
    make_response_case ~status:(`status_range `_4XX)
      ~name:"error_message"
      ~doc:"The reson why the Product ID is unavailable."
      ~pack:Packed.error ~unpack:Packed.unpack_error
      T.string
end

let () = R.add_type_decl_environment_wrapper (Tdenv.add_env Types.env)

let get_products =
  R.register_post "get-products"
    ~urlpath:"/products/get"
    ~req_type:T.product_query
    ~req_name:"product_query"
    ~req_doc:"query to filter products"
    ~resp_type:T.product_list
    ~resp_name:"products"
    ~resp_doc:"Products matching the given query in the database"

let get_orders =
  R.register_post "get-orders"
    ~urlpath:"/orders/get"
    ~req_type:T.order_query
    ~req_name:"order_query"
    ~req_doc:"query to filter orders"
    ~resp_type:T.order_list
    ~resp_name:"orders"
    ~resp_doc:"Orders matching the given query in the database"

let get_product =
  R.register_post' "get-product"
  ~urlpath:"/product/get"
  ~req_type:T.product_id
  ~req_name:"product_id"
  ~req_doc:"Product ID"
  [ make_response_case ~status:(`status_code 200)
      ~name:"product"
      ~doc:"Product of the ID"
      ~pack:Packed.ok ~unpack:Packed.unpack_ok
      T.product;
    error_response_case ();
  ]

let get_order =
  R.register_post' "get-order"
  ~urlpath:"/order/get"
  ~req_type:T.order_id
  ~req_name:"order_id"
  ~req_doc:"Rrder ID"
  [ make_response_case ~status:(`status_code 200)
      ~name:"order"
      ~doc:"order of the ID"
      ~pack:Packed.ok ~unpack:Packed.unpack_ok
      T.order;
    error_response_case ();
  ]

let register_product =
  R.register_post "register-product"
    ~urlpath:"/product/register"
    ~req_type:T.product_details
    ~req_name:"product_details"
    ~req_doc:"product details to be registered to the database"
    ~resp_type:T.product_id
    ~resp_name:"product_id"
    ~resp_doc:"Product ID added to the database"

let register_order =
  R.register_post' "register-order"
    ~urlpath:"/order/register"
    ~req_type:T.order_details
    ~req_name:"order_details"
    ~req_doc:"order details to be registered to the database"
    [ make_response_case ~status:(`status_code 200)
        ~name:"order_id"
        ~doc:"ID of the order"
        ~pack:Packed.ok ~unpack:Packed.unpack_ok
        T.order_id;
      error_response_case ();
    ]

let update_product_details =
  R.register_post' "update-product-details"
    ~urlpath:"/product/details/update"
    ~req_type:T.product_details_with_id
    ~req_name:"product_details_with_id"
    ~req_doc:"product details to be updated"
    [ make_response_case ~status:(`status_code 200)
        ~name:"order_id"
        ~doc:"ID of the order"
        ~pack:Packed.ok ~unpack:Packed.unpack_ok
        T.order_id;
      error_response_case ();
    ]

let update_order_status =
  R.register_post' "update-order-status"
    ~urlpath:"/order/status/update"
    ~req_type:T.order_status_with_id
    ~req_name:"order_status"
    ~req_doc:"order status to be updated"
    [ make_response_case ~status:(`status_code 200)
        ~name:"order_id"
        ~doc:"ID of the order"
        ~pack:Packed.ok ~unpack:Packed.unpack_ok
        T.order_id;
      error_response_case ();
    ]

include R.Public

module type Database = sig
  module Io : Monadic

  val with_read_lock_products : (unit -> 'a Io.t) -> 'a Io.t
  val with_write_lock_products : (unit -> 'a Io.t) -> 'a Io.t

  val with_read_lock_orders : (unit -> 'a Io.t) -> 'a Io.t
  val with_write_lock_orders : (unit -> 'a Io.t) -> 'a Io.t

  val select_products : [ `id of T.product_id list | `query of T.product_query ] -> T.product list Io.t
  val insert_product : details:T.product_details -> T.product_id Io.t
  val update_product : T.product_id -> (T.product -> (T.product, 'e) result Io.t) -> (unit, [ `Not_found | `Error of 'e ]) result Io.t

  val select_orders : [ `id of T.order_id | `query of T.order_query ] -> T.order list Io.t
  val insert_order : details:T.order_details -> total_price:int -> status:T.order_status -> T.order_id Io.t
  val update_order : T.order_id -> (T.order -> (T.order, 'e) result Io.t) -> (unit, [ `Not_found | `Error of 'e ]) result Io.t
end

module Builder = functor
  (D: Database)
  (M: ServerBuilder with module Io = D.Io) -> struct
  let build_handler () =
    let open MonadOps(M.Io) in

    M.register_post_handler get_products (fun req ->
      D.with_read_lock_products begin fun () ->
        D.select_products (`query req)
        >|= fun resp -> (200, resp)
      end
    );

    M.register_post_handler get_orders (fun req ->
      D.with_read_lock_orders begin fun () ->
        D.select_orders (`query req)
        >|= fun resp -> (200, resp)
      end
    );

    M.register_post_handler get_product (fun req ->
      D.with_read_lock_products begin fun () ->
        D.select_products (`id [ req ])
        >|= function
        | h :: _ -> (200, Packed.ok h)
        | [] -> (400, Packed.error "Product of the given ID is not found.")
      end
    );

    M.register_post_handler get_order (fun req ->
      D.with_read_lock_orders begin fun () ->
        D.select_orders (`id req)
        >|= function
        | h :: _ -> (200, Packed.ok h)
        | [] -> (400, Packed.error "Order of the given ID is not found.")
      end
    );

    M.register_post_handler register_product (fun req ->
      D.with_write_lock_products begin fun () ->
        D.insert_product ~details:req
        >|= fun resp -> (200, resp)
      end
    );

    M.register_post_handler register_order (fun req ->
      let ordered_products: (T.product_id * int) list =
        req.products
        |> List.group_by fst
        |&> ((?>) (List.foldl (fun s (_, x) -> s + x) 0))
      in
      let ordered_ids = ordered_products |&> fst in
      D.with_read_lock_orders begin fun () ->
      D.with_read_lock_products begin fun () ->
        D.select_products (`id ordered_ids)
        >>= fun products ->
          let products = products |&> fun p -> (p.id, p) in
          let rec loop total_price = function
          | [] ->
            D.insert_order ~details:req ~total_price ~status:`Unpaid
            >|= fun resp -> (200, Packed.ok resp)
          | (product_id, count) :: tl ->
            match List.assoc_opt product_id products with
            | None -> return (400, Packed.error "Product of the given ID is not found.")
            | Some product ->
              D.select_orders (`query {
                products = Some [ product_id ]; status = Some [ `Unpaid; `Paid ];
                minimum_price = None; maximum_price = None; limit = None })
              >>= fun orders ->
                let ordered_count =
                  List.foldl (fun s (o: T.order) ->
                    s + (List.assoc product_id o.details.products))
                    0 orders
                in
                if product.details.count - ordered_count >= count then
                  loop (total_price + count * product.details.price) tl
                else
                  return (403, Packed.error "inventory shortage")
          in
          loop 0 req.products
      end
      end
    );

    M.register_post_handler update_product_details (fun (req_id, req_details) ->
      D.with_write_lock_products begin fun () ->
        D.update_product req_id (fun p -> Ok { p with details = req_details } |> return)
        >|= function
        | Ok () -> (200, Packed.ok req_id)
        | Error `Not_found -> (400, Packed.error "Product of the given ID is not found.")
        | Error (`Error s) -> (400, Packed.error s)
      end
    );

    M.register_post_handler update_order_status (fun (req_id, req_status) ->
      D.with_write_lock_orders begin fun () ->
        D.update_order req_id (fun order ->
          match order.status, req_status with
          | `Unpaid, `Paid
          | `Shipped, `Delivered
          | (`Unpaid | `Paid), `Canceled ->
            Ok { order with status = req_status } |> return
          | `Paid, `Shipped ->
            D.with_write_lock_products begin fun () ->
              order.details.products
              |&> (fun (product_id, count) ->
                D.update_product product_id (fun p ->
                  let count = p.details.count - count in
                  Ok { p with details = { p.details with count }}
                  |> return
                ) >|= ignore)
              |> foldl (fun l r -> l >>= constant r) (return ())
              >|= fun () ->
              Ok { order with status = req_status }
            end
          | _ ->
            Error ("Invalid order status") |> return
        )
        >|= function
        | Ok () -> (200, Packed.ok req_id)
        | Error `Not_found -> (400, Packed.error "Order of the given ID is not found.")
        | Error (`Error s) -> (400, Packed.error s)
      end
    );

    ()
end
