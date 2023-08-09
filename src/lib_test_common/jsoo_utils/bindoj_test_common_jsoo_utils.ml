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
module Bjv = Prr.Jv
open Kxclib
open Prr
open Bindoj_apidir_runtime.Utils

let cast = Obj.magic
let jstr x = Jstr.v x |> cast
let ostr x = x |> cast |> Jstr.to_string

module FutexnIo = struct
  module F = struct
    type 'x fut = 'x Fut.t

    let return0 = Fut.return

    let ( >>=! ) = Fut.bind
  end
  open F
  let pp_fut : (ppf -> 'x -> unit) -> ppf -> 'x fut -> unit =
    fun pp_x ppf fut -> Fut.await fut (pp_x ppf)

  type backtrace = backtrace_info [@@deriving show]

  let current_backtrace () : backtrace =
    match Sys.backend_type with
    | Native | Bytecode -> `ocaml_backtrace (Printexc.get_raw_backtrace ())
    | Other _backend ->
      let stacktrace =
        Js_of_ocaml.(
          (Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "Error") [||])##.stack
          |> Js.to_string) in
      `string_stacktrace stacktrace

  type 'x t = ('x, exn * backtrace) result fut [@@deriving show]

  let pp : 'x. (ppf -> 'x -> unit) -> ppf -> 'x t -> unit = fun vpp ->
    pp_fut (fun ppf -> function
        | Ok x -> vpp ppf x
        | Error (exn, bt) ->
           fprintf ppf "%a:@[<hov>%a@]"
            pp_exn exn
            pp_backtrace_info bt)

  let await : 'a t -> (('a, exn * backtrace) result -> unit) -> unit = Fut.await

  let trace : string t -> unit = fun s ->
    await s (function
        | Ok s -> Log0.log ~label:"trace" ~header_style:(Some `Thin) ~header_color:`Yellow
             "%s" s
        | _ -> ())

  let return : 'x. 'x -> 'x t = fun x -> Result.ok x |> return0
  let inject_error : 'x. exn -> 'x t =
   fun e ->
    let bt = current_backtrace () in
    Result.error (e, bt) |> return0

  let bind : 'x 'y. 'x t -> ('x -> 'y t) -> 'y t =
   fun m af ->
    m
    >>=! function
    | Error e -> Error e |> return0
    | Ok x -> ( try af x with e -> inject_error e)

  let extract_error : 'x t -> ('x, exn * backtrace_info option) result t =
   fun m ->
    m
    >>=! function
    | Ok x -> Result.ok x |> return
    | Error (e, bt) -> Result.error (e, some bt) |> return

  let catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t = fun job handler ->
    let (>>=) = bind in
    job() |> extract_error >>= function
    | Ok x -> return x
    | Error (e, _bt) -> handler e

  let make_rejection : exn * backtrace -> Bjv.t =
   fun (e, bt) ->
    let stacktrace =
      match bt with
      | `ocaml_backtrace bt -> Printexc.raw_backtrace_to_string bt
      | `string_stacktrace trace -> trace in
    match e with
    | Bad_request reason ->
      object%js
        val kind = Jstr.of_string "bad_request"
        val message = reason |> Jstr.of_string
      end
      |> Bjv.repr
    | _ ->
      object%js
        val kind = Jstr.of_string "exception"
        val message = Printexc.to_string e |> Jstr.of_string
        val stacktrace = stacktrace |> Bjv.of_string
      end
      |> Bjv.repr

  let tick_ms ms = Fut.tick ~ms |> Fut.map Result.ok

  let to_promise : Bjv.t t -> Bjv.Promise.t =
   fun m -> Fut.to_promise' m ~ok:identity ~error:make_rejection

  let wrap_js_result : (Bjv.t, Bjv.Error.t) result t -> Bjv.t t =
   fun m ->
    let ( >>= ) m f = bind m f in
    m
    >>= function
    | Ok v -> return v
    | Error je ->
      let module Err = Bjv.Error in
      Error
        ( Failure (Err.message je |> Jstr.to_string),
          `string_stacktrace (Err.stack je |> Jstr.to_string) )
      |> return0

  let wrap_future_result : ('a, Bjv.Error.t) result Fut.t -> 'a t =
   fun m ->
    let ( >>= ) m f = Fut.bind m f in
    m
    >>= function
    | Ok v -> return v
    | Error je ->
      let module Err = Bjv.Error in
      Error
        ( Failure (Err.message je |> Jstr.to_string),
          `string_stacktrace (Err.stack je |> Jstr.to_string) )
      |> return0

  let of_promise : 'a =
   fun ?err_message js_promise ->
    Fut.of_promise' js_promise ~ok:identity ~error:(fun err ->
        let msg = Brr.Console.str err |> Jstr.to_string in
        (match err_message with
        | None -> msg
        | Some m -> m ^ ": " ^ msg)
        |> jstr |> Bjv.Error.v)
    |> wrap_future_result

  let of_promise_raw : 'a -> 'b t =
   fun js_promise ->
   (* Jv.Promise.then' js_promise Result.ok Result.err *)
    Fut.of_promise' js_promise ~ok:identity ~error:(cast : 'a -> exn)
    >>=! function
    | Ok x ->
       return x
    | Error e ->
       Result.error (e, `string_stacktrace "(unknown)") |> return0
end
[@@warning "-32-34"]
module FutexnIoOps = MonadOps (FutexnIo)

module Alcotest_jsoo_platform_state = struct
  type file_descriptor = [`input of Buffer.t | `output of Buffer.t]

  let isatty = ref false
  let vfs = ref StringMap.empty (* path |-> Buffer.t content *)
  let vfs_last_created = ref []

  let home = "/alcotest_jsoo"

  let getcwd() = home
  let file_exists path = StringMap.mem path !vfs
  let open_write_only path : file_descriptor =
    StringMap.update path (function
        | Some b -> Some b
        | None ->
           refappend vfs_last_created path;
           Buffer.create 16 |> some)
    |> refupdate vfs;
    `output (StringMap.find path !vfs)
  let home_directory() = Result.error (`Msg "(no-home)")

  let dump_file_contents() =
    !vfs_last_created |> List.rev
    |!> (fun path ->
      let open Prr.Console in
      group [sprintf ">>> contents of file %s" path |> jstr];
      log [Buffer.contents (StringMap.find path !vfs) |> jstr];
      group_end()
    )

  let () =
    let open Js_of_ocaml in
    Sys_js.mount ~path:home
      (fun ~prefix ~path ->
        StringMap.find_opt (prefix^path) !vfs
        >? Buffer.contents)
end

module Alcotest_jsoo_platform : Alcotest_engine.Platform.MAKER = functor (M : Alcotest_engine.Monad.S) -> struct
  type 'x promise = 'x M.t

  open Js_of_ocaml
  let time = Unix.time (* available in jsoo *)

  let stdout_buffer = Buffer.create 32
  let stderr_buffer = Buffer.create 32
  let console_log buf s =
    let output_endline s = Prr.Console.(log [Jv.of_string s]) in
    match String.split_on_char '\n' s with
    | [] -> ()
    | [s] -> Buffer.add_string buf s
    | s :: rest ->(
       Buffer.contents buf |-> (fun _ -> Buffer.clear buf)
       |> (fun prev -> prev^s |> output_endline);
       match List.and_last rest with
       | mid, last ->
          Buffer.add_string buf last;
          mid |!> output_endline)

  let stdout_isatty() = !Alcotest_jsoo_platform_state.isatty
  let stdout_columns() = some 80 (* heuristic *)
  let log_trap_supported = false (* for now *)
  let prepare_log_trap ~root:_ ~uuid:_ ~name:_ = () (* noop for now *)

  include Alcotest_jsoo_platform_state

  let force_flush() =
    Format.(pp_print_flush std_formatter ());
    Format.(pp_print_flush err_formatter ());
    flush stdout; flush stderr;
    flush_all()

  let setup_std_outputs ?style_renderer ?utf_8 () =
    force_flush();
    if stdout_isatty() then (
      Fmt_tty.setup_std_outputs ?style_renderer ?utf_8 ())
    else (
      Fmt_tty.setup_std_outputs ~style_renderer:`None ~utf_8:true ());
    Sys_js.set_channel_flusher stdout (console_log stdout_buffer);
    Sys_js.set_channel_flusher stderr (console_log stderr_buffer)

  let close : file_descriptor -> unit = fun _ ->
    force_flush()

  let with_redirect : file_descriptor -> (unit -> 'a promise) -> 'a promise =
    function
    | `input _ -> invalid_arg' "Alcotest_jsoo_platform.with_redirect with `input"
    | `output buf ->
       fun job ->
       force_flush();
       Sys_js.set_channel_flusher stdout (fun s ->
           Buffer.add_string buf s);
       Sys_js.set_channel_flusher stderr (fun s ->
           Buffer.add_string buf s);
       let restore() =
         force_flush();
         Sys_js.set_channel_flusher stdout (console_log stdout_buffer);
         Sys_js.set_channel_flusher stderr (console_log stderr_buffer);
       in
       M.catch (fun() ->
           force_flush();
           M.bind (job()) (fun r ->
               force_flush();
               restore();
               M.return r))
         (fun exn ->
           force_flush();
           restore(); raise exn)
end

module Alcotest_jsoo = Alcotest_engine.V1.Cli.Make(Alcotest_jsoo_platform)(FutexnIo)
