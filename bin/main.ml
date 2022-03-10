module Datatype_desc = struct

  open Ppxlib
  open Ast_builder.Default

  type 'x with_docstr = 'x*[ `docstr of string | `nodoc ]

  type record_type_desc = record_field_desc with_docstr list
  and record_field_desc = {
      rf_name : string;
      rf_type : string;
    }

  type variant_type_desc = variant_constructor_desc with_docstr list
  and variant_constructor_desc =
    | Cstr_tuple of {
        ct_name : string;
        ct_args : string list;
      }
    | Cstr_record of {
        cr_name : string;
        cr_fields : record_type_desc;
      }

  type type_decl = {
    td_name : string;
    td_kind : generic_kind with_docstr;
  }
  and generic_kind =
    | Record_kind of record_type_desc
    | Variant_kind of variant_type_desc

  let type_declaration_of_type_decl : type_decl -> type_declaration =
    fun { td_name=name; td_kind=(kind, doc); } ->
    let loc = Location.none in
    let attributes_of_doc = function
      | `docstr doc ->
        let loc = Location.none in
        [attribute ~loc
           ~name:(Located.mk ~loc "ocaml.doc")
           ~payload:(PStr [pstr_eval ~loc (estring ~loc doc) []])]
      | `nodoc -> []
      | _ -> failwith "unknown polymorphic variant for docstr" in
    let kind_type_of_record_type_desc : record_type_desc -> type_kind = fun fields ->
      Ptype_record (List.map (fun ({rf_name; rf_type;}, doc) ->
          { (label_declaration ~loc ~mutable_:Immutable
               ~name:(Located.mk ~loc rf_name)
               ~type_:(ptyp_constr ~loc (Located.mk ~loc (lident rf_type)) []))
            with pld_attributes = attributes_of_doc doc; })
          fields) in
    let kind_type_of_variant_type_desc : variant_type_desc -> type_kind = fun constrs ->
      Ptype_variant (List.map (function
          | Cstr_tuple {ct_name; ct_args;}, doc ->
            { (constructor_declaration ~loc ~res:None
                 ~name:(Located.mk ~loc ct_name)
                 ~args:(Pcstr_tuple (List.map (fun ct_arg ->
                     ptyp_constr ~loc (Located.mk ~loc (lident ct_arg)) [])
                     ct_args)))
              with pcd_attributes = attributes_of_doc doc; }
          | Cstr_record {cr_name; cr_fields;}, doc ->
            { (constructor_declaration ~loc ~res:None
                 ~name:(Located.mk ~loc cr_name)
                 ~args:(match kind_type_of_record_type_desc cr_fields with
                     | Ptype_record fields -> Pcstr_record fields
                     | _ -> failwith "impossible type_kind"))
              with pcd_attributes = attributes_of_doc doc; })
          constrs) in
    { (type_declaration ~loc ~params:[] ~cstrs:[] ~private_:Public ~manifest:None
         ~name:(Located.mk ~loc name)
         ~kind:(match kind with
             | Record_kind record -> kind_type_of_record_type_desc record
             | Variant_kind variant -> kind_type_of_variant_type_desc variant))
      with ptype_attributes = attributes_of_doc doc }

  let gen_jsonm_encoder : string -> type_decl -> value_binding =
    fun ec_name { td_name=_; td_kind=(kind, _); } ->
    let loc = Location.none in
    match kind with
    | Record_kind record ->
       let fields = List.map fst record in
       let vari i = "__bindoj_record_field_var_"^string_of_int i in
       let evari i = evar ~loc (vari i) in
       let pvari i = pvar ~loc (vari i) in
       let members = List.mapi (fun i { rf_name; rf_type; } ->
           let name = [%expr `Name [%e estring ~loc rf_name]] in
           if rf_type = "int" then
             [%expr [[%e name]; `Float (float_of_int [%e evari i])]]
           else if rf_type = "float" then
             [%expr [[%e name]; `Float [%e evari i]]]
           else if rf_type = "string" then
             [%expr [[%e name]; `String [%e evari i]]]
           else if rf_type = "bool" then
             [%expr [[%e name]; `Bool [%e evari i]]]
           else failwith "unsupported record field type") fields in
       let obj = elist ~loc ([[%expr [`Os]]] @ members @ [[%expr [`Oe]]])
                 |> fun e -> [%expr List.concat [%e e]] in
       let rf_binds = List.mapi (fun i { rf_name; rf_type=_ } ->
           (Located.mk ~loc (lident rf_name), pvari i)) fields in
       value_binding ~loc
         ~pat:(pvar ~loc ec_name)
         ~expr:[%expr fun [%p ppat_record ~loc rf_binds Closed ] -> [%e obj]]
    | Variant_kind _ ->
       failwith "encoder of variant is not unimplemented."

end

let ex01 : Datatype_desc.type_decl =
  let open Datatype_desc in
  { td_name = "student";
    td_kind =
      Record_kind
        ([{ rf_name = "admission_year"; rf_type = "int"; }, `nodoc;
          { rf_name = "name"; rf_type = "string"; }, `nodoc;]),
      `nodoc; }

let ex02 : Datatype_desc.type_decl =
  let open Datatype_desc in
  { td_name = "person";
    td_kind =
      Variant_kind
        ([ Cstr_tuple { ct_name = "Anonymous"; ct_args = []; }, `nodoc;
           Cstr_tuple { ct_name = "With_id"; ct_args = ["int"]}, `nodoc;
           Cstr_record { cr_name = "Student";
                         cr_fields =
                           [{ rf_name = "student_id"; rf_type = "int" }, `nodoc;
                            { rf_name = "name"; rf_type = "string" }, `nodoc]; }, `nodoc;
           Cstr_record { cr_name = "Teacher";
                         cr_fields =
                           [{ rf_name = "faculty_id"; rf_type = "int" }, `nodoc;
                            { rf_name = "name"; rf_type = "string" }, `nodoc;
                            { rf_name = "department"; rf_type = "string" }, `nodoc ]; }, `nodoc]),
      `nodoc; }

let ex01_docstr : Datatype_desc.type_decl =
  let open Datatype_desc in
  { td_name = "student";
    td_kind =
      Record_kind
        [{ rf_name = "admission_year"; rf_type = "int"; }, `docstr "addmission_year field";
         { rf_name = "name"; rf_type = "string"; }, `docstr "name field";],
      `docstr "definition of student type"; }

let ex02_docstr : Datatype_desc.type_decl =
  let open Datatype_desc in
  { td_name = "person";
    td_kind =
      Variant_kind
        [ Cstr_tuple { ct_name = "Anonymous"; ct_args = []; }, `docstr "Anonymous constructor";
          Cstr_tuple { ct_name = "With_id"; ct_args = ["int"]; }, `docstr "With_id constructor";
          Cstr_record {
            cr_name = "Student";
            cr_fields = [
              { rf_name = "student_id"; rf_type = "int" }, `docstr "student_id field in Student constructor";
              { rf_name = "name"; rf_type = "string" }, `docstr "name field in Student constructor";
            ]
          },
          `docstr "Student constructor";
          Cstr_record {
            cr_name = "Teacher";
            cr_fields = [
              { rf_name = "faculty_id"; rf_type = "int" }, `docstr "faculty_id field in Teacher constructor";
              { rf_name = "name"; rf_type = "string" }, `docstr "name field in Teacher constructor";
              { rf_name = "department"; rf_type = "string" }, `docstr "dapartment field in Teacher constructor";
            ]
          },
          `docstr "Teacher constructor"],
      `docstr "definition of person type"; }

module ExpectedEx01_and_Ex02 = struct

  type ex01 = {
      admission_year : int;
      name : string;
    }

  type ex02 =
    | Anonymous
    | With_id of int
    | Student of {
        student_id : int;
        name : string;
      }
    | Teacher of {
        faculty_id : int;
        name : string;
        department : string;
      }

end

let () =
  Format.printf "Test_gen.greeting \"Alice\": %s@."
    (Codegen.Greeting.greeting "Alice");
  Format.printf "add 1 2 = %d@." (Codegen.Arith.add 1 2);
  let open Ppxlib in
  let open Ast_builder.Default in
  let loc = Location.none in
  Astlib.Pprintast.signature Format.std_formatter [
    (psig_type ~loc Recursive
       [Datatype_desc.type_declaration_of_type_decl ex01]);
    (psig_type ~loc Recursive
       [Datatype_desc.type_declaration_of_type_decl ex02]);
    (psig_type ~loc Recursive
       [Datatype_desc.type_declaration_of_type_decl ex01_docstr]);
    (psig_type ~loc Recursive
       [Datatype_desc.type_declaration_of_type_decl ex02_docstr]);
  ];
  Astlib.Pprintast.structure Format.std_formatter [
    (pstr_value ~loc Nonrecursive
       [Datatype_desc.gen_jsonm_encoder "encode_student_jsonm" ex01])
  ];
