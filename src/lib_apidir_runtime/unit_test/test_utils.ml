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
open Bindoj_apidir_runtime.Utils
open Bindoj_apidir_shared

module DirectIo : IoStyle = struct
  type 'x t = ('x, exn*backtrace_info option) result

  let return : 'x -> 'x t = fun x -> Result.ok x

  let bind : 'x t -> ('x -> 'y t) -> 'y t = fun x f -> Result.bind x f

  let inject_error : exn -> 'x t =
    fun exn ->
    Result.error (exn, Some (`ocaml_backtrace (Printexc.get_raw_backtrace ())))

  let extract_error : 'x t -> ('x, exn*backtrace_info option) result t =
    function
    | Ok x -> Ok x |> return
    | Error (e, bt) -> Error (e, bt) |> return

  let trace = function
    | Ok s ->
       Log0.log ~label:"trace" ~header_style:(Some `Thin) ~header_color:`Yellow
         "%s" s
    | _ -> ()
end

module Examples = struct
  (** example values of HTTP GET and POST test *)
  type key = { path : string; invp : untyped_invocation_point_info; }

  let rec equal_key key1 key2 =
    key1.path = key2.path &&
    equal_invp key1.invp key2.invp
  and equal_invp (Invp invp1) (Invp invp2) =
    invp1.ip_name = invp2.ip_name &&
    invp1.ip_urlpath = invp2.ip_urlpath &&
    invp1.ip_method = invp2.ip_method

  type ('resp, 'req) value = {
    get : 'resp get option;
    post : ('resp, 'req) post list;
  }
  and 'resp get = {
    orig : 'resp;
    jv : Json.jv;
    pp_get : ppf -> 'resp -> unit;
  }
  and ('resp, 'req) post = {
    orig_resp : 'resp;
    orig_req : 'req;
    jv_resp : Json.jv;
    jv_req : Json.jv;
    pp_post : ppf -> 'resp -> unit;
  }

  type t =
    | Ex : (key * ('resp, 'req) value) list ref -> t

  let get : t -> (key * ('resp, 'req) value) list =
    fun (Ex a) -> !(Obj.magic a)

  let set : t -> (key * ('resp, 'req) value) list -> unit =
    fun (Ex a) b -> (Obj.magic a) := b

  let empty () : t = Ex (ref [])

  let register :
    t
    -> key
    -> ('resp, 'req) value
    -> (('resp, 'req) value -> ('resp, 'req) value)
    -> unit =
    fun t key init update ->
    let rec aux acc =
      function
      | [] -> (key, init) :: acc
      | (k, v) :: rest ->
        if equal_key k key then
          if List.length acc < List.length rest then
            (key, update v) :: acc @ rest
          else
            (key, update v) :: rest @ acc
        else
          aux ((k, v) :: acc) rest in
    set t (aux [] (get t))

  let register_get t path invp ~orig ~jv ~pp =
    let key = { path; invp; } in
    let get = Some { orig; jv; pp_get=pp; } in
    register t key
      { get; post = []; }
      (fun v -> { get; post = v.post; })

  let register_post t path invp ~orig_resp ~orig_req ~jv_resp ~jv_req ~pp =
    let key = { path; invp; } in
    let post = [{ orig_resp; orig_req; jv_resp; jv_req; pp_post=pp; }] in
    register t key
      { get = None; post; }
      (fun v -> { get = v.get; post = post @ v.post; })

  let find_by_path t path =
    List.find_opt (fun (k, _) -> k.path = path) (get t)
    >? snd

  let find_get_by_path t path =
    find_by_path t path
    >>? fun v -> v.get

  let find_post_by_path t path =
    find_by_path t path
    >? (fun v -> v.post)
    |> function
    | None -> []
    | Some jv_pairs -> jv_pairs

  let find_by_invp t invp =
    List.find_opt (fun (k, _) -> equal_invp invp k.invp) (get t)
    >? snd

  let find_get_by_invp t invp =
    find_by_invp t invp
    >>? (fun v -> v.get)

  let find_post_by_invp t invp =
    find_by_invp t invp
    >? (fun v -> v.post)
    |> function
    | None -> []
    | Some jv_pairs -> jv_pairs
end
