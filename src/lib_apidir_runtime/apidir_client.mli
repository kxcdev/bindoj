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
open Kxclib
open Bindoj_base
open Bindoj_apidir_shared

exception Bad_response of
  Json.jv * Typed_type_desc.boxed_type_decl * string option

module type ScopedJsonFetcher = sig
  module IoStyle: Utils.IoStyle

  val perform_get :
    urlpath:string
    -> headers:string list
    -> query_params:(string*string) list
    -> Json.jv IoStyle.t

  val perform_post :
    urlpath:string
    -> headers:string list
    -> query_params:(string*string) list
    -> body:Json.jv
    -> Json.jv IoStyle.t
end

module type T = sig
  type 'resp io

  val perform_json_post :
    ?additional_headers:string list ->
    ?additional_query_params:(string * string) list ->
    ('req, 'resp) invocation_point_info -> 'req -> 'resp io

  val perform_json_get :
    ?additional_headers:string list ->
    ?additional_query_params:(string * string) list ->
    (unit, 'resp) invocation_point_info -> 'resp io
end

module Make :
  functor (Dir : ApiDirManifest) (Fetcher : ScopedJsonFetcher) ->
    sig
      include module type of Apidir_base.Make(Dir)(Fetcher.IoStyle)

      include T with type 'resp io := 'resp Fetcher.IoStyle.t
    end
