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
open Bindoj_apidir_runtime
open Bindoj_example_shared_client.Client
open Bindoj_example_ex02_apidir
open Bindoj_example_ex02_typedesc_generated.Typedesc_generated

module Config = struct let base_url = "http://localhost:8082" end
module Fetcher = Make_cohttp_fetcher (Config)
module Client = Apidir_client.Make (Apidir) (Fetcher)

open MonadOps(Lwt)

let raise_exn = function
  | Ok x -> x
  | Error (e, _) -> raise e

let main =
  Client.perform_json_post Apidir.get_products
    { searchQuery = Some "novel";
      minimum_price = None;
      maximum_price = None;
      limit = None }
  >|= raise_exn
  >>= fun products ->
    let s = sprintf "%a" (List.pp Product.pp) products in
    Lwt_io.printf "get_products:\n%s\n" s
  >>= fun () ->
    Client.perform_json_post Apidir.register_order
      { products = products |> List.map (fun (p: product) -> (p.id, 1));
        payment_method = Credit_card {
          card_number = "1111222233334444";
          holder_name = "John Smith";
          expiration_date = (2026, 5);
          cvv = "123" } }
  >|= raise_exn
  >>= function
  | `Error s ->
    Lwt_io.printf "register_order:\nError with message \"%s\"\n" s
  | `Ok product_id -> begin
    Lwt_io.printf "register_order:\nSuccess with order id %d\n" product_id
    >>= fun () ->
      Client.perform_json_post Apidir.get_order product_id
    >|= raise_exn
    >>= function
      | `Error s ->
        Lwt_io.printf "get_order:\nError with message \"%s\"\n" s
      | `Ok order ->
        let s = sprintf "%a" Order.pp order in
        Lwt_io.printf "get_order:\n%s\n" s
    end
  >>= fun _ -> Lwt_io.(flush stdout)

let () = Lwt_main.run main
