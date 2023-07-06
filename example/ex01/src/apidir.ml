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

open Bindoj_example_shared_apidir.Apidir

module Types = struct
  include Bindoj_example_ex01_typedesc_generated.Typedesc_generated

  open struct
    let to_ttd ?configs ?self name = Coretypes.to_typed_type_decl ~env ?configs ?self name
  end

  let string = Coretypes.Prims.string |> to_ttd "string"
end

open struct
  module R = MakeRegistry()
  module T = Types

  let student_from_person = function
    | T.Student { student_id; name; } ->
      (200, Packed.ok { T.admission_year = student_id; name; })
    | Anonymous -> (422, Packed.error "anonymous, not student")
    | With_id _ -> (422, Packed.error "with_id, not student")
    | Teacher _ -> (422, Packed.error "teacher, not student")
end

let () = R.add_type_decl_environment_wrapper (Tdenv.add_env Types.env)

let get_any_student =
  R.register_get "get-any-student"
    ~urlpath:"/student/any-one"
    ~resp_type:T.student
    ~resp_name:"student"
    ~resp_doc:"a student record (could be anyone) in the database"

let get_student_from_person =
  R.register_post' "get-student-from-person"
    ~urlpath:"/student/from-person"
    ~req_type:T.person
    ~req_name:"person"
    ~req_doc:"a person record identifying a student"
    [
      make_response_case ~status:(`status_code 200)
        ~name:"student"
        ~doc:"the student record corresponding to the supplied person"
        ~pack:Packed.ok ~unpack:Packed.unpack_ok
        T.student;
      make_response_case ~status:(`status_code 422)
        ~name:"error_message"
        ~doc:"The reson why the person is unavailable."
        ~pack:Packed.error ~unpack:Packed.unpack_error
        T.string;
    ]

let () = begin
  get_any_student
  |> R.register_response_sample { T.admission_year = 1984; name = "William Gibson" };

  get_student_from_person
  |> R.register_usage_samples
    ([ T.Anonymous;
       With_id 1619;
       Student { student_id = 451; name = "Ray Bradbury" };
       Teacher { faculty_id = 2001; name = "Arthur C. Clark"; department = "Space" }
    ] |&> (fun p ->
      let (code, res) = student_from_person p in
      (p, res, `status_code code), `nodoc))
end

include R.Public

module type Repository = sig
  module Io : Monadic
  val sample_student : Types.student Io.t
end

module Builder = functor
  (R: Repository)
  (M: ServerBuilder with module Io = R.Io) -> struct
  let build_handler () =
    let open MonadOps(M.Io) in

    M.register_get_handler get_any_student (fun () ->
      R.sample_student
      >|= (fun s -> (200, s))
    );

    M.register_post_handler get_student_from_person
      (student_from_person &> return);

    ()
end

