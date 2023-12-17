type nonrec my_int = int
(** definition of my_int type *)

val my_int_reflect : my_int Bindoj_runtime.Refl.t
val my_int_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val my_int_to_json : my_int -> Kxclib.Json.jv
val my_int_of_json' : my_int Bindoj_runtime.json_full_decoder
val my_int_of_json : Kxclib.Json.jv -> my_int option

type nonrec my_tuple = float * string
(** definition of my_tuple type *)

val my_tuple_reflect : my_tuple Bindoj_runtime.Refl.t
val my_tuple_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val my_tuple_to_json : my_tuple -> Kxclib.Json.jv
val my_tuple_of_json' : my_tuple Bindoj_runtime.json_full_decoder
val my_tuple_of_json : Kxclib.Json.jv -> my_tuple option

type student = {
  admission_year : int;  (** 📅 addmission_year field *)
  name : string;  (** 📛 name field *)
}
(** 📝 definition of student type *)

val student_reflect : student Bindoj_runtime.Refl.t
val student_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val student_to_json : student -> Kxclib.Json.jv
val student_of_json' : student Bindoj_runtime.json_full_decoder
val student_of_json : Kxclib.Json.jv -> student option

(** definition of person type *)
type person =
  | Anonymous  (** Anonymous constructor *)
  | With_id of int  (** With_id constructor *)
  | Student of {
      student_id : int;  (** student_id field in Student constructor *)
      name : string;  (** name field in Student constructor *)
    }  (** Student constructor *)
  | Teacher of {
      faculty_id : int;  (** faculty_id field in Teacher constructor *)
      name : string;  (** name field in Teacher constructor *)
      department : string;  (** dapartment field in Teacher constructor *)
    }  (** Teacher constructor *)

val person_reflect : person Bindoj_runtime.Refl.t
val person_json_discriminator_value : person -> string
val person_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val person_to_json : person -> Kxclib.Json.jv
val person_of_json' : person Bindoj_runtime.json_full_decoder
val person_of_json : Kxclib.Json.jv -> person option
