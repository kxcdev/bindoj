open Bindoj_objintf_gen_test_gen_utils [@@warning "-33"]

type ex_record_student = { admission_year : int; name : string }

let rec (ex_record_student_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { admission_year; name } ->
             StringMap.of_list
               [
                 ("admission_year", Expr.of_int admission_year);
                 ("name", Expr.of_string name);
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "admission_year" >>= Expr.to_int
             >>= fun admission_year ->
             xs |> StringMap.find_opt "name" >>= Expr.to_string >>= fun name ->
             Some { admission_year; name });
       })
[@@warning "-33-39"]

let ex_record_student_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExRecordStudent",
           `object_of
             [
               `mandatory_field ("admissionYear", `integral);
               `mandatory_field ("name", `string);
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_record_student_to_json =
  (let string_to_json (x : string) : Kxclib.Json.jv = `str x
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   fun { admission_year = x0; name = x1 } ->
     `obj [ ("admissionYear", int_to_json x0); ("name", string_to_json x1) ]
    : ex_record_student -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_record_student_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl =
        let string_of_json' path = function
          | (`str x : Kxclib.Json.jv) -> Ok x
          | jv ->
              Error
                ( Printf.sprintf
                    "expecting type 'string' but the given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        and int_of_json' path = function
          | (`num x : Kxclib.Json.jv) ->
              if Float.is_integer x then Ok (int_of_float x)
              else
                Error
                  ( Printf.sprintf "expecting an integer but the given is '%f'" x,
                    path )
          | jv ->
              Error
                ( Printf.sprintf
                    "expecting type 'int' but the given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        in
        fun path __bindoj_orig ->
          match __bindoj_orig with
          | `obj param ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "admissionYear" param
              |> Option.to_result
                   ~none:("mandatory field 'admissionYear' does not exist", path)
              >>= int_of_json' (`f "admissionYear" :: path)
              >>= fun x0 ->
              List.assoc_opt "name" param
              |> Option.to_result
                   ~none:("mandatory field 'name' does not exist", path)
              >>= string_of_json' (`f "name" :: path)
              >>= fun x1 -> Ok { admission_year = x0; name = x1 }
          | jv ->
              Error
                ( Printf.sprintf
                    "an object is expected for a record value, but the given \
                     is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, ex_record_student_json_shape_explanation))
    : ex_record_student Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_record_student_of_json =
  (fun x -> ex_record_student_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_record_student option)
[@@warning "-39"]

module Simple_interfaces = struct
  type nonrec hello = string -> unit
  type nonrec unit_sole = string -> unit -> unit -> string

  class type unit_obj = object
    method name : string
    method unit_01 : string -> unit -> string
    method unit_02 : string -> unit -> unit -> string
    method unit_03 : string -> unit -> unit -> unit -> string
  end

  module type unit_mod = sig
    val name : unit -> string
    val unit_01 : string -> unit -> string
    val unit_02 : string -> unit -> unit -> string
    val unit_03 : string -> unit -> unit -> unit -> string
  end

  module type with_default_value = sig
    val get_default_string : ?str:string -> string
    val get_default_student : ?student:ex_record_student -> string
  end
end

open Simple_interfaces [@@warning "-33"]

module type Endemic_object_registry_interface = sig
  val register_string : id0:string -> id1:Kxclib.int53p -> string option -> unit
  val register_hello : id:string -> hello option -> unit
end

module Concrete_bridge_interfaces = struct
  open Bindoj_objintf_shared

  (** marker type for this specific concrete bridge *)
  type nonrec br = Br

  type 'x peer = ('x, br) peer'
  type 'x endemic = ('x, br) endemic'

  let access : 'x peer -> 'x = access
  let bridge x = bridge_generic ~bridge:Br x

  module Simple_interfaces = Simple_interfaces

  module Complex_interfaces = struct
    type nonrec sole_var = string

    class type rec_obj = object
      method name : string
      method get_self : rec_obj endemic
    end
  end

  module Interfaces = struct
    module Simple_interfaces = Simple_interfaces
    module Complex_interfaces = Complex_interfaces
    include Simple_interfaces
    include Complex_interfaces
  end
end

module type Concrete_bridge = sig
  include module type of Concrete_bridge_interfaces
  module Endemic_object_registry : Endemic_object_registry_interface
end

open Concrete_bridge_interfaces.Interfaces [@@warning "-33"]

module type Endemic_setup_only_full_bridge = functor
  (_ : sig
     val my_string : string
     val my_hello : hello
     val my_sole_var : sole_var
     val my_unit_sole : unit_sole
     val my_unit_obj : unit_obj
     val my_unit_mod : (module unit_mod)
     val my_rec_obj : rec_obj
     val my_non_json_values : Utils.non_json_values
     val with_default_value : (module with_default_value)

     val initially_registry_objects :
       (module Endemic_object_registry_interface) -> unit
   end)
  -> sig
  include Concrete_bridge

  val endemic_full_bridge : Bindoj_objintf_shared.endemic_full_bridge_reference
end
