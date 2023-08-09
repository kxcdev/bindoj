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
open Bindoj_example_shared_mock_database.Mock_database
open Bindoj_example_ex02_typedesc_generated.Typedesc_generated

open struct
  let products_table: (product_id, product) Hashtbl.t =
    [ { id = 0;
        details = {
          name = "Pride and Prejudice";
          description = "A classic romance novel, describing the love story between Elizabeth Bennet and Fitzwilliam Darcy.";
          price = 800;
          count = 15 } };
      { id = 1;
        details = {
          name = "Moby Dick";
          description = "The story of captain Ahab's relentless pursuit of the white whale, Moby Dick.";
          price = 1200; count = 10 } };
      { id = 2;
        details = {
          name = "Dracula";
          description = "A Gothic horror novel, telling the story of the vampire Count Dracula.";
          price = 1000; count = 25 } };
      { id = 3;
        details = {
          name = "Frankenstein";
          description = "A tale of young scientist Victor Frankenstein and his creation of a grotesque monster.";
          price = 950; count = 7 } };
      { id = 4;
        details = {
          name = "Great Expectations";
          description = "The coming-of-age story of an orphan named Pip and the challenges he faces in life.";
          price = 1100; count = 22 } };
      { id = 5;
        details = {
          name = "Crime and Punishment";
          description = "A novel that explores the moral dilemmas of a young student named Raskolnikov.";
          price = 1500; count = 17 } };
      { id = 6;
        details = {
          name = "The Picture of Dorian Gray";
          description = "A literary portrayal of moral decadence, in which a young man wishes for eternal youth.";
          price = 650; count = 9 } };
      { id = 7;
        details = {
          name = "Heart of Darkness";
          description = "The story of Charles Marlow's river journey into the heart of Africa and his encounter with Kurtz.";
          price = 600; count = 35 } };
      { id = 8;
        details = {
          name = "The Adventures of Sherlock Holmes";
          description = "A collection of short stories featuring the brilliant detective Sherlock Holmes and his loyal friend, Dr. Watson.";
          price = 1300; count = 12 } };
      { id = 9;
        details = {
          name = "The Call of the Wild";
          description = "A novel about a domesticated dog named Buck and his journey back to the wild.";
          price = 900; count = 20 } };
    ]
    |&> (fun (x: Product.t) -> (x.id, x))
    |> List.to_hashtbl

  let orders_table: (order_id, order) Hashtbl.t =
    [ { id = 0; status = `Delivered; total_price = 2000;
        details = {
          products = [ 0, 1; 1, 1 ];
          payment_method = Credit_card {
            card_number = "1111222233334444";
            holder_name = "John Smith";
            expiration_date = (2026, 5);
            cvv = "123" } };
      };
      { id = 1; status = `Paid; total_price = 3100;
        details = {
          products = [ 2, 2; 4, 1 ];
          payment_method = Credit_card {
            card_number = "2222333344445555";
            holder_name = "Jane Smith";
            expiration_date = (2025, 11);
            cvv = "234" } };
      };
      { id = 2; status = `Unpaid; total_price = 900;
        details = {
          products = [ 9, 1 ];
          payment_method = Bank_transfer {
            account_number = "123456789";
            bank_name = "Bank of Example";
            holder_name = "Alice Brown" } };
      };
      { id = 3; status = `Canceled; total_price = 800;
        details = {
          products = [ 0, 1 ];
          payment_method = Credit_card {
            card_number = "3333444455556666";
            holder_name = "Robert Johnson";
            expiration_date = (2027, 6);
            cvv = "345" } };
      };
      { id = 4; status = `Shipped; total_price = 1400;
        details = {
          products = [ 0, 1; 7, 1 ];
          payment_method = Bank_transfer {
            account_number = "234567890";
            bank_name = "Example Savings";
            holder_name = "Emma Davis" } };
      };
      { id = 5; status = `Delivered; total_price = 2500;
        details = {
          products = [ 2, 1; 5, 1 ];
          payment_method = Credit_card {
            card_number = "4444555566667777";
            holder_name = "James Collins";
            expiration_date = (2024, 9);
            cvv = "456" } };
      };
      { id = 6; status = `Paid; total_price = 1950;
        details = {
          products = [ 6, 1; 8, 1 ];
          payment_method = Bank_transfer {
            account_number = "345678901";
            bank_name = "Union Example";
            holder_name = "Susan Edwards" } };
      };
      { id = 7; status = `Delivered; total_price = 1300;
        details = {
          products = [ 8, 1 ];
          payment_method = Credit_card {
            card_number = "5555666677778888";
            holder_name = "William Lopez";
            expiration_date = (2023, 1);
            cvv = "567" } };
      };
      { id = 8; status = `Shipped; total_price = 2400;
        details = {
          products = [ 8, 1; 4, 1 ];
          payment_method = Bank_transfer {
            account_number = "456789012";
            bank_name = "Example Federal";
            holder_name = "Emily Clark" } };
      };
      { id = 9; status = `Unpaid; total_price = 2850;
        details = {
          products = [ 6, 1; 8, 1; 9, 1 ];
          payment_method = Credit_card {
            card_number = "8888777766665555";
            holder_name = "Daniel Rodriguez";
            expiration_date = (2024, 4);
            cvv = "678" } };
      }
    ] |&> (fun (x: Order.t) -> (x.id, x)) |> List.to_hashtbl

  let filter x f v =
    match x with
    | None -> Some v
    | Some x when f x v -> Some v
    | Some _ -> None
end

module Io = Lwt

open struct
  let rwlock_products = Read_write_lock.create ()
  let rwlock_orders = Read_write_lock.create ()
end

let with_read_lock_products f = Read_write_lock.with_read_lock rwlock_products f
let with_write_lock_products f = Read_write_lock.with_write_lock rwlock_products f
let with_read_lock_orders f = Read_write_lock.with_read_lock rwlock_orders f
let with_write_lock_orders f = Read_write_lock.with_write_lock rwlock_orders f

let select_products = function
  | `id ids ->
    ids |&?> (Hashtbl.find_opt products_table) |> Io.return
  | `query (query: product_query) ->
    let open MonadOps(Option) in
    let searchWords =
      query.searchQuery >|= String.split_on_char ' '
    in
    let products =
      products_table
      |> Hashtbl.to_seq
      |> Seq.filter_map (fun (_, p) ->
        Some p
        >>= filter searchWords (fun ws (p: product) ->
          ws |> List.for_all (fun w ->
            let re = Str.regexp_string w in
            let contains s =
              try Str.search_forward re s 0 |> ignore; true with Not_found -> false
            in
            contains p.details.name || contains p.details.description))
        >>= filter query.minimum_price (fun mp (p: product) -> p.details.price >= mp)
        >>= filter query.maximum_price (fun mp (p: product) -> p.details.price <= mp)
      )
      |> List.of_seq
      |> List.sort_uniq (fun (a: product) b -> compare a.id b.id)
    in
    let products_count = List.length products in
    begin match query.limit with
    | Some n when n < products_count -> List.take n products
    | _ -> products
    end
    |> Io.return

let insert_product =
  let current_id = ref 10 in
  fun ~details ->
    let product : product = { id = !current_id; details } in
    Hashtbl.add products_table product.id product;
    incr current_id;
    Io.return product.id

let update_product =
  fun id f ->
    match Hashtbl.find_opt products_table id with
    | None -> Io.return (Error `Not_found)
    | Some p ->
      let open MonadOps(Io) in
      f p >>= function
      | Ok p ->
        Hashtbl.replace products_table id p;
        Io.return (Ok ())
      | Error s -> Io.return (Error (`Error s))

let select_orders = function
  | `id id ->
    Hashtbl.find_opt orders_table id |> Option.to_list |> Io.return
  | `query (query: order_query) ->
    let products =
      match query.products with
      | None | Some [] -> None
      | Some ps -> Some ps
    in
    let status =
      match query.status with
      | None | Some [] -> None
      | Some ss -> Some ss
    in
    let open MonadOps(Option) in
    let orders =
      orders_table
      |> Hashtbl.to_seq
      |> Seq.filter_map (fun (_, o) ->
        Some o
        >>= filter products (fun ps (o: order) ->
          ps |> List.for_all (fun p -> List.exists (fst &> (=) p) o.details.products))
        >>= filter status (fun ss (o: order) -> List.exists ((=) o.status) ss)
        >>= filter query.minimum_price (fun mp (o: order) -> o.total_price >= mp)
        >>= filter query.maximum_price (fun mp (o: order) -> o.total_price <= mp)
      )
      |> List.of_seq
      |> List.sort_uniq (fun (a: order) b -> compare a.id b.id)
    in
    let orders_count = List.length orders in
    begin match query.limit with
    | Some n when n < orders_count -> List.take n orders
    | _ -> orders
    end
    |> Io.return

let insert_order =
  let current_id = ref 10 in
  fun ~details ~total_price ~status ->
    let order : order = { id = !current_id; details; status; total_price } in
    Hashtbl.add orders_table order.id order;
    incr current_id;
    Io.return order.id

let update_order =
  fun id f ->
    match Hashtbl.find_opt orders_table id with
    | None -> Io.return (Error `Not_found)
    | Some o ->
      let open MonadOps(Io) in
      f o >>= function
      | Ok o ->
        Hashtbl.replace orders_table id o;
        Io.return (Ok ())
      | Error s -> Io.return (Error (`Error s))
