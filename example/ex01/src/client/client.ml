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
open Bindoj_apidir_runtime
open Bindoj_example_shared_client.Client
open Bindoj_example_ex01_apidir
open Bindoj_example_ex01_typedesc_generated.Typedesc_generated

module Config = struct let base_url = "http://localhost:8081" end
module Fetcher = Make_cohttp_fetcher (Config)
module Client = Apidir_client.Make (Apidir) (Fetcher)

open MonadOps(Lwt)

let raise_exn = function
  | Ok x -> x
  | Error (e, _) -> raise e

let main =
  Client.perform_json_get Apidir.get_any_student
  >|= raise_exn
  >>= fun (student: student) ->
    let s = sprintf "%a" Student.pp student in
    Lwt_io.printf "get_any_student:\n%s\n" s
  >>= fun () ->
    let req = Student { student_id = 451; name = "Ray Bradbury" } in
    Client.perform_json_post Apidir.get_student_from_person req
    >|= raise_exn
    >|= begin function
      | `Ok student ->
        let s = sprintf "%a" Student.pp student in
        Lwt_io.printf "get_student_from_person:\n%s\n" s
      | `Error e -> failwith e
    end
  >>= fun _ -> Lwt_io.(flush stdout)

let () = Lwt_main.run main
