(**pp -syntax camlp5o $(MIGRATE_OCAMLCFLAGS) -package pa_ppx.import,pa_ppx_q_ast,camlp5.parser_quotations_base *)
(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base
open Ppxutil
open Pa_ppx_parsetree_pattern_parsetree_VERSION.Pattern_misc

let antiloc_payload ~loc s =
  let open Q_ast_base in
  match split_anti_loc s with
    None -> Fmt.(failwithf "Q_parsetree.antiloc_payload: unparseable antiquotation <<%s>>" s)
  | Some(_,_,p) -> p
 
let antiloc_kind ~loc s =
  let open Q_ast_base in
  match split_anti_loc s with
    None -> Fmt.(failwithf "Q_parsetree.antiloc_kind: unparseable antiquotation <<%s>>" s)
  | Some(_,k,_) -> k

module Regular = struct

module MetaE = struct
  include Q_ast_base.E_MetaSig
  let int n = let loc = Ploc.dummy in <:expr< $int:string_of_int n$ >>
  open Pa_ppx_base
  open MLast
    let vala elem x =
      match Pa_ppx_q_ast_runtime.MetaE.vala elem x with
        <:expr< Ploc.VaVal $e$ >> -> e
      | e -> Ploc.raise (loc_of_expr e)
               (Failure Fmt.(str "Q_parsetree.vala: unexpected result expr:@ %a"
                               Pp_MLast.pp_expr e))
end

module MetaP = struct
  include Q_ast_base.P_MetaSig
  let int n = let loc = Ploc.dummy in <:patt< $int:string_of_int n$ >>
  open Pa_ppx_base
  open MLast
    let vala elem x =
      match Pa_ppx_q_ast_runtime.MetaP.vala elem x with
        <:patt< Ploc.VaVal $e$ >> -> e
      | e -> Ploc.raise (loc_of_patt e)
               (Failure Fmt.(str "Q_parsetree.vala: unexpected result patt:@ %a"
                               Pp_MLast.pp_patt e))
end

let lexwrap paf s =
  let s = Scanf.unescaped s in
  paf (Lexing.from_string s)

let parse_expression s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.expression s

let parse_module_expr s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.module_expr s

let parse_module_type s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.module_type s

let parse_pattern s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.pattern s

let parse_constant s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.constant s

let parse_core_type s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.core_type s

let parse_longident s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.longident s

let parse_extension_constructor s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.extension_constructor s

let parse_extended_module_path s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.extended_module_path s

let parse_longlident s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.longlident s

let parse_structure_item s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.structure_item s

let parse_structure s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.structure s

let parse_signature_item s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.signature_item s

let parse_signature s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.signature s

let parse_constructor_declaration s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.constructor_declaration s

let parse_attribute s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.attribute s

let parse_extension s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.extension s

let parse_label_declaration s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.label_declaration s

let parse_match_case s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.match_case s

let parse_value_binding s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.value_binding s

let parse_arg_label s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.arg_label s

let parse_lident_vala_loc s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.lident_vala_loc s

let parse_binding_op s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.binding_op s

let parse_type_declaration s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.type_declaration s

let parse_type_substitution s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.type_substitution s

let parse_row_field s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.row_field s

let parse_object_field s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.object_field s

let parse_class_description s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.class_description s

let parse_class_expr s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.class_expr s

let parse_class_type s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.class_type s

let parse_class_field s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.class_field s

let parse_functor_parameter s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.functor_parameter s

let parse_module_declaration s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.module_declaration s

let parse_with_constraint s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.with_constraint s

let parse_class_type_field s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.class_type_field s

let parse_str_type_extension s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.str_type_extension s

let parse_sig_type_extension s =
  lexwrap Pa_ppx_parsetree_pattern_parsetree_VERSION.Parse.sig_type_extension s

module Q = struct

[%%import: Reorg_parsetree.attribute]
[@@deriving q_ast {
    default_data_source_module = Parsetree
  ; default_quotation_source_module = Reorg_parsetree
  ; expr_meta_module = MetaE
  ; patt_meta_module = MetaP
  ; loc_mode = CustomLoc { loc_varname = loc ; loc_type = [%typ: location] ; loc_function_name = location }
  ; minimal_record_module_labels = true
  ; custom_type = [
      ([%typ: location_stack], {
         pattern = (fun ctxt _ -> <:patt< _ >>)
       ; expression = (fun ctxt _ -> <:expr< [] >>)
       ; function_name = location_stack
      })
    ]
  ; pertype = {
      located = {
        data_source_module = Location
      ; quotation_source_module = Reorg_parsetree
      }
    ; longident_t = {
        data_source_module = Longident
      ; quotation_source_module = Reorg_parsetree
      }
    ; arg_label = {
        data_source_module = Asttypes
      ; quotation_source_module = Reorg_parsetree
      }
    ; arg_label = {
        data_source_module = Asttypes
      ; quotation_source_module = Reorg_parsetree
      }
    ; rec_flag = {
        data_source_module = Asttypes
      ; quotation_source_module = Reorg_parsetree
      }
    ; direction_flag = {
        data_source_module = Asttypes
      ; quotation_source_module = Reorg_parsetree
      }
    ; private_flag = {
        data_source_module = Asttypes
      ; quotation_source_module = Reorg_parsetree
      }
    ; mutable_flag = {
        data_source_module = Asttypes
      ; quotation_source_module = Reorg_parsetree
      }
    ; override_flag = {
        data_source_module = Asttypes
      ; quotation_source_module = Reorg_parsetree
      }
    ; closed_flag = {
        data_source_module = Asttypes
      ; quotation_source_module = Reorg_parsetree
      }
    ; expression = {
        add_branches_patt_code = (function
          | {pexp_desc=Pexp_xtr{txt;loc};pexp_attributes=Ploc.VaVal[];} when antiloc_payload ~loc txt <> "_" -> C.xtr (ploc_of_location loc) txt
          | {pexp_desc=Pexp_xtr{txt;loc};} when antiloc_payload ~loc txt <> "_" -> failwith "non-blank Pexp_xtr with non-empty attributes is nearly always an error"
                                 )
      ; add_branches_expr_code = (function
          | {pexp_desc=Pexp_xtr{txt;loc};pexp_attributes=Ploc.VaVal[];} when antiloc_kind ~loc txt <> "noattrs" ->
             C.xtr (ploc_of_location loc) txt
          | {pexp_desc=Pexp_xtr{txt;loc};pexp_attributes} when antiloc_kind ~loc txt = "noattrs" ->
             let txt = Q_ast_base.replace_antiloc_kind  ~newkind:"" txt  in
             let e = C.xtr (ploc_of_location loc) txt in
             let attrs = attributes __ctxt__ pexp_attributes in
             let loc = ploc_of_location loc in
             <:expr< { ($e$) with Parsetree.pexp_attributes = $attrs$ } >>
          | {pexp_desc=Pexp_xtr{txt;loc};} -> failwith "Pexp_xtr with non-empty attributes is nearly always an error"
                                 )
      }
    ; expression_desc = {
        add_branches_patt_code = (function
          | Pexp_xtr{txt;loc} when antiloc_payload ~loc txt = "_" -> C.xtr (ploc_of_location loc) txt
                                 )
      }
    ; module_expr = {
        add_branches_patt_code = (function
          | {pmod_desc=Pmod_xtr{txt;loc};pmod_attributes=Ploc.VaVal[];} when antiloc_payload ~loc txt <> "_" -> C.xtr (ploc_of_location loc) txt
          | {pmod_desc=Pmod_xtr{txt;loc};} when antiloc_payload ~loc txt <> "_" -> failwith "Pmod_xtr with non-empty attributes is nearly always an error"
                                 )
      ; add_branches_expr_code = (function
          | {pmod_desc=Pmod_xtr{txt;loc};pmod_attributes=Ploc.VaVal[];} when antiloc_kind ~loc txt <> "noattrs" ->
             C.xtr (ploc_of_location loc) txt
          | {pmod_desc=Pmod_xtr{txt;loc};pmod_attributes} when antiloc_kind ~loc txt = "noattrs" ->
             let txt = Q_ast_base.replace_antiloc_kind  ~newkind:"" txt  in
             let e = C.xtr (ploc_of_location loc) txt in
             let attrs = attributes __ctxt__ pmod_attributes in
             let loc = ploc_of_location loc in
             <:expr< { ($e$) with Parsetree.pmod_attributes = $attrs$ } >>
          | {pmod_desc=Pmod_xtr{txt;loc};} -> failwith "Pmod_xtr with non-empty attributes is nearly always an error"
                                 )
      }
    ; module_expr_desc = {
        add_branches_patt_code = (function
          | Pmod_xtr{txt;loc} when antiloc_payload ~loc txt = "_" -> C.xtr (ploc_of_location loc) txt
                                 )
      }
    ; module_type = {
        add_branches_patt_code = (function
          | {pmty_desc=Pmty_xtr{txt;loc};pmty_attributes=Ploc.VaVal[];} when antiloc_payload ~loc txt <> "_" -> C.xtr (ploc_of_location loc) txt
          | {pmty_desc=Pmty_xtr{txt;loc};} when antiloc_payload ~loc txt <> "_" -> failwith "Pmty_xtr with non-empty attributes is nearly always an error"
                                 )
      ; add_branches_expr_code = (function
          | {pmty_desc=Pmty_xtr{txt;loc};pmty_attributes=Ploc.VaVal[];} when antiloc_kind ~loc txt <> "noattrs" ->
             C.xtr (ploc_of_location loc) txt
          | {pmty_desc=Pmty_xtr{txt;loc};pmty_attributes} when antiloc_kind ~loc txt = "noattrs" ->
             let txt = Q_ast_base.replace_antiloc_kind  ~newkind:"" txt  in
             let e = C.xtr (ploc_of_location loc) txt in
             let attrs = attributes __ctxt__ pmty_attributes in
             let loc = ploc_of_location loc in
             <:expr< { ($e$) with Parsetree.pmty_attributes = $attrs$ } >>
          | {pmty_desc=Pmty_xtr{txt;loc};} -> failwith "Pmty_xtr with non-empty attributes is nearly always an error"
                                 )
      }
    ; module_type_desc = {
        add_branches_patt_code = (function
          | Pmty_xtr{txt;loc} when antiloc_payload ~loc txt = "_" -> C.xtr (ploc_of_location loc) txt
                                 )
      }

    ; pattern = {
        add_branches_patt_code = (function
          | {ppat_desc=Ppat_xtr{txt;loc};ppat_attributes=Ploc.VaVal[];} when antiloc_payload ~loc txt <> "_" -> C.xtr (ploc_of_location loc) txt
          | {ppat_desc=Ppat_xtr{txt;loc};} when antiloc_payload ~loc txt <> "_" -> failwith "Ppat_xtr with non-empty attributes is nearly always an error"
                                 )
      ; add_branches_expr_code = (function
          | {ppat_desc=Ppat_xtr{txt;loc};ppat_attributes=Ploc.VaVal[];} when antiloc_kind ~loc txt <> "noattrs" ->
             C.xtr (ploc_of_location loc) txt
          | {ppat_desc=Ppat_xtr{txt;loc};ppat_attributes} when antiloc_kind ~loc txt = "noattrs" ->
             let txt = Q_ast_base.replace_antiloc_kind  ~newkind:"" txt  in
             let e = C.xtr (ploc_of_location loc) txt in
             let attrs = attributes __ctxt__ ppat_attributes in
             let loc = ploc_of_location loc in
             <:expr< { ($e$) with Parsetree.ppat_attributes = $attrs$ } >>
          | {ppat_desc=Ppat_xtr{txt;loc};} -> failwith "Ppat_xtr with non-empty attributes is nearly always an error"
                                 )
      }
    ; pattern_desc = {
        add_branches_patt_code = (function
          | Ppat_xtr{txt;loc} when antiloc_payload ~loc txt = "_" -> C.xtr (ploc_of_location loc) txt
                                 )
      }
    ; core_type = {
        add_branches_patt_code = (function
          | {ptyp_desc=Ptyp_xtr{txt;loc};ptyp_attributes=Ploc.VaVal[];} when antiloc_payload ~loc txt <> "_" -> C.xtr (ploc_of_location loc) txt
          | {ptyp_desc=Ptyp_xtr{txt;loc};} when antiloc_payload ~loc txt <> "_" -> failwith "Ptyp_xtr with non-empty attributes is nearly always an error"
                                 )
      ; add_branches_expr_code = (function
          | {ptyp_desc=Ptyp_xtr{txt;loc};ptyp_attributes=Ploc.VaVal[];} when antiloc_kind ~loc txt <> "noattrs" ->
             C.xtr (ploc_of_location loc) txt
          | {ptyp_desc=Ptyp_xtr{txt;loc};ptyp_attributes} when antiloc_kind ~loc txt = "noattrs" ->
             let txt = Q_ast_base.replace_antiloc_kind  ~newkind:"" txt  in
             let e = C.xtr (ploc_of_location loc) txt in
             let attrs = attributes __ctxt__ ptyp_attributes in
             let loc = ploc_of_location loc in
             <:expr< { ($e$) with Parsetree.ptyp_attributes = $attrs$ } >>
          | {ptyp_desc=Ptyp_xtr{txt;loc};} -> failwith "Ptyp_xtr with non-empty attributes is nearly always an error"
                                 )
      }
    ; core_type_desc = {
        add_branches_patt_code = (function
          | Ptyp_xtr{txt;loc} when antiloc_payload ~loc txt = "_" -> C.xtr (ploc_of_location loc) txt
                                 )
      }
    ; class_type = {
        add_branches_patt_code = (function
          | {pcty_desc=Pcty_xtr{txt;loc};pcty_attributes=Ploc.VaVal[];} when antiloc_payload ~loc txt <> "_" -> C.xtr (ploc_of_location loc) txt
          | {pcty_desc=Pcty_xtr{txt;loc};} when antiloc_payload ~loc txt <> "_" -> failwith "Pcty_xtr with non-empty attributes is nearly always an error"
                                 )
      ; add_branches_expr_code = (function
          | {pcty_desc=Pcty_xtr{txt;loc};pcty_attributes=Ploc.VaVal[];} when antiloc_kind ~loc txt <> "noattrs" ->
             C.xtr (ploc_of_location loc) txt
          | {pcty_desc=Pcty_xtr{txt;loc};pcty_attributes} when antiloc_kind ~loc txt = "noattrs" ->
             let txt = Q_ast_base.replace_antiloc_kind  ~newkind:"" txt  in
             let e = C.xtr (ploc_of_location loc) txt in
             let attrs = attributes __ctxt__ pcty_attributes in
             let loc = ploc_of_location loc in
             <:expr< { ($e$) with Parsetree.pcty_attributes = $attrs$ } >>
          | {pcty_desc=Pcty_xtr{txt;loc};} -> failwith "Pcty_xtr with non-empty attributes is nearly always an error"
                                 )
      }
    ; class_type_desc = {
        add_branches_patt_code = (function
          | Pcty_xtr{txt;loc} when antiloc_payload ~loc txt = "_" -> C.xtr (ploc_of_location loc) txt
                                 )
      }
    ; class_expr = {
        add_branches_patt_code = (function
          | {pcl_desc=Pcl_xtr{txt;loc};pcl_attributes=Ploc.VaVal[];} when antiloc_payload ~loc txt <> "_" -> C.xtr (ploc_of_location loc) txt
          | {pcl_desc=Pcl_xtr{txt;loc};} when antiloc_payload ~loc txt <> "_" -> failwith "Pcl_xtr with non-empty attributes is nearly always an error"
                                 )
      ; add_branches_expr_code = (function
          | {pcl_desc=Pcl_xtr{txt;loc};pcl_attributes=Ploc.VaVal[];} when antiloc_kind ~loc txt <> "noattrs" ->
             C.xtr (ploc_of_location loc) txt
          | {pcl_desc=Pcl_xtr{txt;loc};pcl_attributes} when antiloc_kind ~loc txt = "noattrs" ->
             let txt = Q_ast_base.replace_antiloc_kind  ~newkind:"" txt  in
             let e = C.xtr (ploc_of_location loc) txt in
             let attrs = attributes __ctxt__ pcl_attributes in
             let loc = ploc_of_location loc in
             <:expr< { ($e$) with Parsetree.pcl_attributes = $attrs$ } >>
          | {pcl_desc=Pcl_xtr{txt;loc};} -> failwith "Pcl_xtr with non-empty attributes is nearly always an error"
                                 )
      }
    ; class_expr_desc = {
        add_branches_patt_code = (function
          | Pcl_xtr{txt;loc} when antiloc_payload ~loc txt = "_" -> C.xtr (ploc_of_location loc) txt
                                 )
      }

    ; str_vala = {
        data_source_module = Ast_helper
      ; quotation_source_module = Reorg_parsetree
      }
    }
  ; entrypoints = [
      {name = "expression"; from_string = parse_expression ; type_name = expression }
    ; {name = "module_expr"; from_string = parse_module_expr ; type_name = module_expr }
    ; {name = "module_type"; from_string = parse_module_type ; type_name = module_type }
    ; {name = "pattern"; from_string = parse_pattern ; type_name = pattern }
    ; {name = "constant"; from_string = parse_constant ; type_name = constant }
    ; {name = "core_type"; from_string = parse_core_type ; type_name = core_type }
    ; {name = "longident_t"; from_string = parse_longident ; type_name = longident_t }
    ; {name = "longlident"; from_string = parse_longlident ; type_name = longident_t }
    ; {name = "extended_module_path"; from_string = parse_extended_module_path ; type_name = longident_t }
    ; {name = "structure_item"; from_string = parse_structure_item ; type_name = structure_item }
    ; {name = "structure"; from_string = parse_structure ; type_name = structure }
    ; {name = "signature_item"; from_string = parse_signature_item ; type_name = signature_item }
    ; {name = "signature"; from_string = parse_signature ; type_name = signature }
    ; {name = "constructor_declaration"; from_string = parse_constructor_declaration ; type_name = constructor_declaration }
    ; {name = "attribute"; from_string = parse_attribute ; type_name = attribute }
    ; {name = "extension"; from_string = parse_extension ; type_name = extension }
    ; {name = "label_declaration"; from_string = parse_label_declaration ; type_name = label_declaration }
    ; {name = "case"; from_string = parse_match_case ; type_name = case }
    ; {name = "value_binding"; from_string = parse_value_binding ; type_name = value_binding }
    ; {name = "arg_label"; from_string = parse_arg_label ; type_name = arg_label }
    ; {name = "lident_loc"; from_string = parse_lident_vala_loc ; type_name = str_vala }
    ; {name = "extension_constructor"; from_string = parse_extension_constructor ; type_name = extension_constructor }
    ; {name = "binding_op"; from_string = parse_binding_op ; type_name = binding_op }
    ; {name = "type_decl"; from_string = parse_type_declaration ; type_name = type_declaration }
    ; {name = "type_subst"; from_string = parse_type_substitution ; type_name = type_declaration }
    ; {name = "row_field"; from_string = parse_row_field ; type_name = row_field }
    ; {name = "object_field"; from_string = parse_object_field ; type_name = object_field }
    ; {name = "class_description"; from_string = parse_class_description ; type_name = class_description }
    ; {name = "class_expr"; from_string = parse_class_expr ; type_name = class_expr }
    ; {name = "class_type"; from_string = parse_class_type ; type_name = class_type }
    ; {name = "class_field"; from_string = parse_class_field ; type_name = class_field }
    ; {name = "functor_parameter"; from_string = parse_functor_parameter ; type_name = functor_parameter_vala }
    ; {name = "module_declaration"; from_string = parse_module_declaration ; type_name = module_declaration }
    ; {name = "with_constraint"; from_string = parse_with_constraint ; type_name = with_constraint }
    ; {name = "class_type_field"; from_string = parse_class_type_field ; type_name = class_type_field }
    ; {name = "str_type_extension"; from_string = parse_str_type_extension ; type_name = type_extension }
    ; {name = "sig_type_extension"; from_string = parse_str_type_extension ; type_name = type_extension }
    ]
 }]

end

module QNoAttr = struct

[%%import: Reorg_parsetree.attribute]
[@@deriving q_ast {
    default_data_source_module = Parsetree
  ; default_quotation_source_module = Reorg_parsetree
  ; expr_meta_module = MetaE
  ; patt_meta_module = MetaP
  ; loc_mode = CustomLoc { loc_varname = loc ; loc_type = [%typ: location] ; loc_function_name = location }
  ; minimal_record_module_labels = true
  ; custom_type = [
      ([%typ: location_stack], {
         pattern = (fun ctxt _ -> <:patt< _ >>)
       ; expression = (fun ctxt _ -> <:expr< [] >>)
       ; function_name = location_stack
      })
    ; ([%typ: attributes], {
         pattern = (fun ctxt _ -> <:patt< _ >>)
       ; expression = (fun ctxt _ -> <:expr< [] >>)
       ; function_name = attributes
      })
    ]
  ; pertype = {
      located = {
        data_source_module = Location
      ; quotation_source_module = Reorg_parsetree
      }
    ; longident_t = {
        data_source_module = Longident
      ; quotation_source_module = Reorg_parsetree
      }
    ; arg_label = {
        data_source_module = Asttypes
      ; quotation_source_module = Reorg_parsetree
      }
    ; arg_label = {
        data_source_module = Asttypes
      ; quotation_source_module = Reorg_parsetree
      }
    ; rec_flag = {
        data_source_module = Asttypes
      ; quotation_source_module = Reorg_parsetree
      }
    ; direction_flag = {
        data_source_module = Asttypes
      ; quotation_source_module = Reorg_parsetree
      }
    ; private_flag = {
        data_source_module = Asttypes
      ; quotation_source_module = Reorg_parsetree
      }
    ; mutable_flag = {
        data_source_module = Asttypes
      ; quotation_source_module = Reorg_parsetree
      }
    ; override_flag = {
        data_source_module = Asttypes
      ; quotation_source_module = Reorg_parsetree
      }
    ; closed_flag = {
        data_source_module = Asttypes
      ; quotation_source_module = Reorg_parsetree
      }
    ; expression = {
        add_branches_patt_code = (function
          | {pexp_desc=Pexp_xtr{txt;loc};pexp_attributes=Ploc.VaVal[];} when antiloc_payload ~loc txt <> "_" -> C.xtr (ploc_of_location loc) txt
          | {pexp_desc=Pexp_xtr{txt;loc};} when antiloc_payload ~loc txt <> "_" -> failwith "non-blank Pexp_xtr with non-empty attributes is nearly always an error"
                                 )
      ; add_branches_expr_code = (function
          | {pexp_desc=Pexp_xtr{txt;loc};pexp_attributes=Ploc.VaVal[];} -> C.xtr (ploc_of_location loc) txt
          | {pexp_desc=Pexp_xtr{txt;loc};} -> failwith "Pexp_xtr with non-empty attributes is nearly always an error"
                                 )
      }
    ; expression_desc = {
        add_branches_patt_code = (function
          | Pexp_xtr{txt;loc} when antiloc_payload ~loc txt = "_" -> C.xtr (ploc_of_location loc) txt
                                 )
      }
    ; module_expr = {
        add_branches_patt_code = (function
          | {pmod_desc=Pmod_xtr{txt;loc};pmod_attributes=Ploc.VaVal[];} when antiloc_payload ~loc txt <> "_" -> C.xtr (ploc_of_location loc) txt
          | {pmod_desc=Pmod_xtr{txt;loc};} when antiloc_payload ~loc txt <> "_" -> failwith "Pmod_xtr with non-empty attributes is nearly always an error"
                                 )
      ; add_branches_expr_code = (function
          | {pmod_desc=Pmod_xtr{txt;loc};pmod_attributes=Ploc.VaVal[];} -> C.xtr (ploc_of_location loc) txt
          | {pmod_desc=Pmod_xtr{txt;loc};} -> failwith "Pmod_xtr with non-empty attributes is nearly always an error"
                                 )
      }
    ; module_expr_desc = {
        add_branches_patt_code = (function
          | Pmod_xtr{txt;loc} when antiloc_payload ~loc txt = "_" -> C.xtr (ploc_of_location loc) txt
                                 )
      }
    ; module_type = {
        add_branches_patt_code = (function
          | {pmty_desc=Pmty_xtr{txt;loc};pmty_attributes=Ploc.VaVal[];} when antiloc_payload ~loc txt <> "_" -> C.xtr (ploc_of_location loc) txt
          | {pmty_desc=Pmty_xtr{txt;loc};} when antiloc_payload ~loc txt <> "_" -> failwith "Pmty_xtr with non-empty attributes is nearly always an error"
                                 )
      ; add_branches_expr_code = (function
          | {pmty_desc=Pmty_xtr{txt;loc};pmty_attributes=Ploc.VaVal[];} -> C.xtr (ploc_of_location loc) txt
          | {pmty_desc=Pmty_xtr{txt;loc};} -> failwith "Pmty_xtr with non-empty attributes is nearly always an error"
                                 )
      }
    ; module_type_desc = {
        add_branches_patt_code = (function
          | Pmty_xtr{txt;loc} when antiloc_payload ~loc txt = "_" -> C.xtr (ploc_of_location loc) txt
                                 )
      }

    ; pattern = {
        add_branches_patt_code = (function
          | {ppat_desc=Ppat_xtr{txt;loc};ppat_attributes=Ploc.VaVal[];} when antiloc_payload ~loc txt <> "_" -> C.xtr (ploc_of_location loc) txt
          | {ppat_desc=Ppat_xtr{txt;loc};} when antiloc_payload ~loc txt <> "_" -> failwith "Ppat_xtr with non-empty attributes is nearly always an error"
                                 )
      ; add_branches_expr_code = (function
          | {ppat_desc=Ppat_xtr{txt;loc};ppat_attributes=Ploc.VaVal[];} -> C.xtr (ploc_of_location loc) txt
          | {ppat_desc=Ppat_xtr{txt;loc};} -> failwith "Ppat_xtr with non-empty attributes is nearly always an error"
                                 )
      }
    ; pattern_desc = {
        add_branches_patt_code = (function
          | Ppat_xtr{txt;loc} when antiloc_payload ~loc txt = "_" -> C.xtr (ploc_of_location loc) txt
                                 )
      }
    ; core_type = {
        add_branches_patt_code = (function
          | {ptyp_desc=Ptyp_xtr{txt;loc};ptyp_attributes=Ploc.VaVal[];} when antiloc_payload ~loc txt <> "_" -> C.xtr (ploc_of_location loc) txt
          | {ptyp_desc=Ptyp_xtr{txt;loc};} when antiloc_payload ~loc txt <> "_" -> failwith "Ptyp_xtr with non-empty attributes is nearly always an error"
                                 )
      ; add_branches_expr_code = (function
          | {ptyp_desc=Ptyp_xtr{txt;loc};ptyp_attributes=Ploc.VaVal[];} -> C.xtr (ploc_of_location loc) txt
          | {ptyp_desc=Ptyp_xtr{txt;loc};} -> failwith "Ptyp_xtr with non-empty attributes is nearly always an error"
                                 )
      }
    ; core_type_desc = {
        add_branches_patt_code = (function
          | Ptyp_xtr{txt;loc} when antiloc_payload ~loc txt = "_" -> C.xtr (ploc_of_location loc) txt
                                 )
      }
    ; class_type = {
        add_branches_patt_code = (function
          | {pcty_desc=Pcty_xtr{txt;loc};pcty_attributes=Ploc.VaVal[];} when antiloc_payload ~loc txt <> "_" -> C.xtr (ploc_of_location loc) txt
          | {pcty_desc=Pcty_xtr{txt;loc};} when antiloc_payload ~loc txt <> "_" -> failwith "Pcty_xtr with non-empty attributes is nearly always an error"
                                 )
      ; add_branches_expr_code = (function
          | {pcty_desc=Pcty_xtr{txt;loc};pcty_attributes=Ploc.VaVal[];} -> C.xtr (ploc_of_location loc) txt
          | {pcty_desc=Pcty_xtr{txt;loc};} -> failwith "Pcty_xtr with non-empty attributes is nearly always an error"
                                 )
      }
    ; class_type_desc = {
        add_branches_patt_code = (function
          | Pcty_xtr{txt;loc} when antiloc_payload ~loc txt = "_" -> C.xtr (ploc_of_location loc) txt
                                 )
      }
    ; class_expr = {
        add_branches_patt_code = (function
          | {pcl_desc=Pcl_xtr{txt;loc};pcl_attributes=Ploc.VaVal[];} when antiloc_payload ~loc txt <> "_" -> C.xtr (ploc_of_location loc) txt
          | {pcl_desc=Pcl_xtr{txt;loc};} when antiloc_payload ~loc txt <> "_" -> failwith "Pcl_xtr with non-empty attributes is nearly always an error"
                                 )
      ; add_branches_expr_code = (function
          | {pcl_desc=Pcl_xtr{txt;loc};pcl_attributes=Ploc.VaVal[];} -> C.xtr (ploc_of_location loc) txt
          | {pcl_desc=Pcl_xtr{txt;loc};} -> failwith "Pcl_xtr with non-empty attributes is nearly always an error"
                                 )
      }
    ; class_expr_desc = {
        add_branches_patt_code = (function
          | Pcl_xtr{txt;loc} when antiloc_payload ~loc txt = "_" -> C.xtr (ploc_of_location loc) txt
                                 )
      }

    ; str_vala = {
        data_source_module = Ast_helper
      ; quotation_source_module = Reorg_parsetree
      }
    }
  ; entrypoints = [
      {name = "expression.noattr"; from_string = parse_expression ; type_name = expression }
    ; {name = "module_expr.noattr"; from_string = parse_module_expr ; type_name = module_expr }
    ; {name = "module_type.noattr"; from_string = parse_module_type ; type_name = module_type }
    ; {name = "pattern.noattr"; from_string = parse_pattern ; type_name = pattern }
    ; {name = "constant.noattr"; from_string = parse_constant ; type_name = constant }
    ; {name = "core_type.noattr"; from_string = parse_core_type ; type_name = core_type }
    ; {name = "longident_t.noattr"; from_string = parse_longident ; type_name = longident_t }
    ; {name = "longlident.noattr"; from_string = parse_longlident ; type_name = longident_t }
    ; {name = "extended_module_path.noattr"; from_string = parse_extended_module_path ; type_name = longident_t }
    ; {name = "structure_item.noattr"; from_string = parse_structure_item ; type_name = structure_item }
    ; {name = "structure.noattr"; from_string = parse_structure ; type_name = structure }
    ; {name = "signature_item.noattr"; from_string = parse_signature_item ; type_name = signature_item }
    ; {name = "signature.noattr"; from_string = parse_signature ; type_name = signature }
    ; {name = "constructor_declaration.noattr"; from_string = parse_constructor_declaration ; type_name = constructor_declaration }
    ; {name = "attribute.noattr"; from_string = parse_attribute ; type_name = attribute }
    ; {name = "extension.noattr"; from_string = parse_extension ; type_name = extension }
    ; {name = "label_declaration.noattr"; from_string = parse_label_declaration ; type_name = label_declaration }
    ; {name = "case.noattr"; from_string = parse_match_case ; type_name = case }
    ; {name = "value_binding.noattr"; from_string = parse_value_binding ; type_name = value_binding }
    ; {name = "arg_label.noattr"; from_string = parse_arg_label ; type_name = arg_label }
    ; {name = "lident_loc.noattr"; from_string = parse_lident_vala_loc ; type_name = str_vala }
    ; {name = "extension_constructor.noattr"; from_string = parse_extension_constructor ; type_name = extension_constructor }
    ; {name = "binding_op.noattr"; from_string = parse_binding_op ; type_name = binding_op }
    ; {name = "type_decl.noattr"; from_string = parse_type_declaration ; type_name = type_declaration }
    ; {name = "type_subst.noattr"; from_string = parse_type_substitution ; type_name = type_declaration }
    ; {name = "row_field.noattr"; from_string = parse_row_field ; type_name = row_field }
    ; {name = "object_field.noattr"; from_string = parse_object_field ; type_name = object_field }
    ; {name = "class_description.noattr"; from_string = parse_class_description ; type_name = class_description }
    ; {name = "class_expr.noattr"; from_string = parse_class_expr ; type_name = class_expr }
    ; {name = "class_type.noattr"; from_string = parse_class_type ; type_name = class_type }
    ; {name = "class_field.noattr"; from_string = parse_class_field ; type_name = class_field }
    ; {name = "functor_parameter.noattr"; from_string = parse_functor_parameter ; type_name = functor_parameter_vala }
    ; {name = "module_declaration.noattr"; from_string = parse_module_declaration ; type_name = module_declaration }
    ; {name = "with_constraint.noattr"; from_string = parse_with_constraint ; type_name = with_constraint }
    ; {name = "class_type_field.noattr"; from_string = parse_class_type_field ; type_name = class_type_field }
    ; {name = "str_type_extension.noattr"; from_string = parse_str_type_extension ; type_name = type_extension }
    ; {name = "sig_type_extension.noattr"; from_string = parse_str_type_extension ; type_name = type_extension }
    ]
 }]
end

end
