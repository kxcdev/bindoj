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
module Read_write_lock : sig
  type t

  val create : unit -> t

  val lock_read : t -> unit Lwt.t
  val unlock_read : t -> unit Lwt.t
  val with_read_lock : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  val lock_write : t -> unit Lwt.t
  val unlock_write : t -> unit Lwt.t
  val with_write_lock : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end = struct
  open Lwt

  type t = {
    mutable read_count: int;
    mutable write_count: int;
    mutex: Lwt_mutex.t;
    read_condition: unit Lwt_condition.t;
    write_condition: unit Lwt_condition.t;
  }

  let create () = {
    read_count = 0;
    write_count = 0;
    mutex = Lwt_mutex.create ();
    read_condition = Lwt_condition.create ();
    write_condition = Lwt_condition.create ();
  }

  let lock_read lock =
    Lwt_mutex.with_lock lock.mutex (fun () ->
      let rec wait_writer () =
        if lock.write_count > 0 then
          Lwt_condition.wait ~mutex:lock.mutex lock.write_condition
          >>= wait_writer
        else Lwt.return_unit
      in
      wait_writer () >>= begin fun () ->
        lock.read_count <- lock.read_count + 1;
        Lwt.return_unit
      end
    )

  let unlock_read lock =
    Lwt_mutex.with_lock lock.mutex (fun () ->
      lock.read_count <- lock.read_count - 1;
      if lock.read_count = 0 then
        Lwt_condition.signal lock.read_condition ();
      Lwt.return_unit
    )

  let with_read_lock lock f =
    lock_read lock
    >>= f
    >>= fun ret -> unlock_read lock
    >|= (fun () -> ret)

  let lock_write lock =
    Lwt_mutex.with_lock lock.mutex (fun () ->
      let rec wait_reader () =
        if lock.read_count > 0 || lock.write_count > 0 then
          Lwt_condition.wait ~mutex:lock.mutex lock.read_condition
          >>= wait_reader
        else Lwt.return_unit
      in
      wait_reader () >>= begin fun () ->
        lock.write_count <- lock.write_count + 1;
        Lwt.return_unit
      end
    )

  let unlock_write lock =
    Lwt_mutex.with_lock lock.mutex (fun () ->
      lock.write_count <- lock.write_count - 1;
      Lwt_condition.signal lock.read_condition ();
      Lwt_condition.signal lock.write_condition ();
      Lwt.return_unit
    )

  let with_write_lock lock f =
    lock_write lock
    >>= f
    >>= fun ret -> unlock_write lock
    >|= (fun () -> ret)

end
