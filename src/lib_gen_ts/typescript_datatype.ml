let default_kind_fname = "kind"
let default_arg_fname = "arg"

module Rope = struct
  [@ocaml.warning "-32"]
  [@ocaml.warning "-34"]
  type t = Zed_rope.t
  type rope = Zed_rope.rope
  let empty = Zed_rope.empty
  let length = Zed_rope.length
  let is_empty = Zed_rope.is_empty
  let get = Zed_rope.get
  let sub = Zed_rope.sub
  let append = Zed_rope.append
  let concat = Zed_rope.concat
  let insert = Zed_rope.insert
  let of_string s = Zed_rope.of_string (Zed_string.of_utf8 s)
  let to_string r = Zed_string.to_utf8 (Zed_rope.to_string r)
end

module RopeUtil = struct
  let rope = Rope.of_string
  let (^) = Rope.append
end

module StringMap = Map.Make (String)

type type_map = string StringMap.t

let add_new_type : string -> type_map -> type_map = fun typ type_map ->
  StringMap.add typ typ type_map

let default_type_convertion_map : type_map =
  StringMap.empty
  |> StringMap.add "int" "number"
  |> StringMap.add "float" "number"
  |> StringMap.add "bool" "boolean"
  |> StringMap.add "string" "string"

let convert_type : string StringMap.t -> string -> Rope.t = fun map x ->
  match StringMap.find_opt x map with
  | None -> failwith "unknown type name"
  | Some x -> RopeUtil.rope x

let in_object : Rope.t -> Rope.t = fun content ->
  let open RopeUtil in
  rope "{ "^content^rope " }"

let ts_field_of_record_field : type_map -> record_field_desc -> Rope.t =
  fun type_map { rf_name; rf_type; _; } ->
  let open RopeUtil in
  rope rf_name^rope ": "^convert_type type_map rf_type^rope ";"

let ts_object_of_variant_constructor : type_map -> flavor -> variant_constructor_desc -> Rope.t =
  let open RopeUtil in
  fun type_map -> function
    | `flat_kind ->
(*
        let kind_fname = rope kind_fname in
        let arg_fname = rope arg_fname in
*)
      begin function
        | Cstr_tuple { ct_name; ct_args; ct_flvconfigs; _; } ->
          begin match ct_flvconfigs with
            | Flvconfig_flat_kind { kind_fname; arg_fname; } :: _ ->
              let kind_fname = rope (Option.value kind_fname ~default:default_kind_fname) in
              let arg_fname = rope (Option.value arg_fname ~default:default_arg_fname) in
              let cstr = kind_fname^rope ": "^rope "\""^rope ct_name^rope "\";" in
              begin match ct_args with
                | [] -> in_object cstr
                | [arg] -> in_object
                             (cstr^rope " "^arg_fname^rope ": "^convert_type type_map arg)
                | _ -> in_object
                         (cstr^rope " "
                          ^arg_fname^rope ": ["
                          ^Rope.concat (rope ", ")
                            (ct_args |&> fun arg -> convert_type type_map arg)
                          ^rope "];")
              end
            | _ -> failwith "unknown flavor configs"
          end
        | Cstr_record { cr_name; cr_fields; cr_flvconfigs; _; } ->
          begin match cr_flvconfigs with
            | Flvconfig_flat_kind { kind_fname; _; } :: _ ->
              let kind_fname = rope (Option.value kind_fname ~default:default_kind_fname) in
              let cstr = kind_fname^rope ": "^rope "\""^rope cr_name^rope "\";" in
              let args =
                Rope.concat (rope " ")
                  (cr_fields |&> fun (field, _) -> ts_field_of_record_field type_map field) in
              in_object (cstr^rope " "^args)
            | _ -> failwith "unknown flavor configs"
          end
      end
    | _ -> failwith "unknown flavor"

let gen_ts_type : ?flavor:flavor -> type_decl -> string =
  fun ?(flavor=`flat_kind) { td_name; td_kind=(kind, _); } ->
  let open RopeUtil in
  let type_def name body =
    rope "type "^name^rope " = "^body^rope ";" in
  let type_map =
    default_type_convertion_map
    |> add_new_type td_name in
  match kind with
  | Record_kind record ->
    type_def (rope td_name)
      (Rope.concat (rope " ")
         (record |&> fun (field, _) -> ts_field_of_record_field type_map field)
       |> in_object)
    |> Rope.to_string
  | Variant_kind variant ->
    type_def (rope td_name)
      (Rope.concat (rope "\n| ")
         (variant |&> fun (cstr, _) -> ts_object_of_variant_constructor type_map flavor cstr))
    |> Rope.to_string

let gen_ts_case_analyzer : ?flavor:flavor -> type_decl -> string =
  fun ?(flavor=`flat_kind) { td_name; td_kind=(kind, _); } ->
  match kind with
  | Variant_kind variant ->
    let open RopeUtil in
    let type_name = rope td_name in
    let analyzer type_arg arg arg_type ret_type body =
      rope "function analyze_"^type_name
      ^rope "<" ^ type_arg ^rope ">"
      ^rope "(\n" ^arg^rope " : "^arg_type ^rope "\n) : "
      ^rope "("^ret_type^rope ") {\n" ^body ^rope "\n};" in
    let cstrs = variant |&> fst in
    let type_argument = rope "__bindoj_ret" in
    let argument = rope "__bindoj_fns" in
    let argument_type type_arg =
      let type_map =
        default_type_convertion_map
        |> add_new_type td_name in
      let obj_brs =
        (cstrs |&> fun cstr ->
            let name = match cstr with
              | Cstr_tuple { ct_name; _; } -> rope ct_name
              | Cstr_record { cr_name; _; } -> rope cr_name in
            name^rope ": (v: "^
            ts_object_of_variant_constructor type_map flavor cstr
            ^rope ") => "^type_arg)
        |> Rope.concat (rope ",\n")
        |> fun content -> rope "{\n"^content^rope "\n}" in
      obj_brs in
    let var_x = rope "__bindoj_x" in
    let return_type type_arg =
      rope "("^var_x^rope ": "^type_name^rope ") => "^type_arg in
    let body arg =
      let in_strlit x = rope "\""^x^rope "\"" in
      let return_lambda var typ body =
        rope "return ("^var^rope ": "^typ^rope ") => {\n"^body^rope "\n}" in
      let cases =
        Rope.concat (rope " else ")
          (cstrs |&> fun cstr ->
              let tag = match cstr with
                | Cstr_tuple { ct_name; _; } ->
                  in_strlit (rope ct_name)
                | Cstr_record { cr_name; _; } ->
                  in_strlit (rope cr_name) in
              let kind_fname = match cstr with
                | Cstr_tuple { ct_flvconfigs=Flvconfig_flat_kind { kind_fname; _; } :: _; _; } ->
                  let kind_fname =
                    rope (Option.value kind_fname ~default:default_kind_fname) in
                  var_x^rope "."^kind_fname
                | Cstr_record { cr_flvconfigs=Flvconfig_flat_kind { kind_fname; _; } :: _; _; }->
                  let kind_fname =
                    rope (Option.value kind_fname ~default:default_kind_fname) in
                  var_x^rope "."^kind_fname
                | _ -> failwith "unknown flavor configs" in
              rope "if ("^kind_fname^rope " === "^tag^rope ") {\n"
              ^rope "return "^arg^rope "["^kind_fname^rope "]("^var_x^rope ");\n}")
        ^rope " else {\n"
        ^rope "throw new TypeError(\""
        ^rope "panic @analyze_person - unrecognized: \" + "^var_x
        ^rope ");\n}" in
      return_lambda var_x type_name cases in
    analyzer
      type_argument
      argument
      (argument_type type_argument)
      (return_type type_argument)
      (body argument)
    |> Rope.to_string
  | _ -> failwith "case analyzer is not for record"
