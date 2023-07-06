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
open Bindoj_example_shared_server.Server
open Bindoj_example_ex02_apidir
open MonadOps(Lwt)

let port = 8082

let server =
  let module S = Make_cohttp_server(Apidir) in
  let module B = Apidir.Builder(Mock_database)(S.Bridge) in
  B.build_handler();

  Conduit_lwt_unix.init ~src:"127.0.0.1" () (* Bind only to localhost *)
  >>= fun ctx ->
    let ctx = Cohttp_lwt_unix.Client.custom_ctx ~ctx () in
    S.make_server()
    |> Cohttp_lwt_unix.Server.create ~ctx ~mode:(`TCP (`Port port))

let main =
  Lwt_io.printf "Server started at port %d.\n" port
  >>= fun () -> Lwt_io.(flush stdout)
  >>= fun () -> server

let () =
  server |> Lwt_main.run
