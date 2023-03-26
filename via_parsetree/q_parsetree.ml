(**pp -syntax camlp5o $(MIGRATE_OCAMLCFLAGS) -package pa_ppx.import,pa_ppx_q_ast,camlp5.parser_quotations_base *)
(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_parsetree_pattern_parsetree.Pattern_misc

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

let parse_expression s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.expression (Lexing.from_string s)

let parse_module_expr s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.module_expr (Lexing.from_string s)

let parse_pattern s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.pattern (Lexing.from_string s)

let parse_core_type s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.core_type (Lexing.from_string s)

let parse_longident s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.longident (Lexing.from_string s)

let parse_extension_constructor s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.extension_constructor (Lexing.from_string s)

let parse_extended_module_path s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.extended_module_path (Lexing.from_string s)

let parse_structure_item s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.structure_item (Lexing.from_string s)

let parse_constructor_declaration s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.constructor_declaration (Lexing.from_string s)

let parse_attribute s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.attribute (Lexing.from_string s)

let parse_label_declaration s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.label_declaration (Lexing.from_string s)

let parse_match_case s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.match_case (Lexing.from_string s)

let parse_value_binding s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.value_binding (Lexing.from_string s)

let parse_arg_label s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.arg_label (Lexing.from_string s)

let parse_lident_vala_loc s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.lident_vala_loc (Lexing.from_string s)

[%%import: Reorg_parsetree.attribute]
[@@deriving q_ast {
    default_data_source_module = Reorg_parsetree
  ; expr_meta_module = MetaE
  ; patt_meta_module = MetaP
  ; loc_mode = CustomLoc { loc_varname = __loc__ ; loc_type = [%typ: location] ; loc_function_name = location }
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
    ; value_binding = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; letop = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; binding_op = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; case = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; attribute = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; payload = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; type_exception = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; extension_constructor = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; extension_constructor_kind = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; constructor_declaration = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; label_declaration = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; constructor_arguments = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; constant = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; expression = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      ; add_branches_patt_code = (function
          | {pexp_desc=Pexp_xtr{txt;loc};} -> C.xtr (ploc_of_location loc) txt
                                 )
      ; add_branches_expr_code = (function
          | {pexp_desc=Pexp_xtr{txt;loc};} -> C.xtr (ploc_of_location loc) txt
                                 )
      }
    ; expression_desc = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; module_expr = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      ; add_branches_patt_code = (function
          | {pmod_desc=Pmod_xtr{txt;loc};} -> C.xtr (ploc_of_location loc) txt
                                 )
      ; add_branches_expr_code = (function
          | {pmod_desc=Pmod_xtr{txt;loc};} -> C.xtr (ploc_of_location loc) txt
                                 )
      }
    ; module_expr_desc = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; pattern = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      ; add_branches_patt_code = (function
          | {ppat_desc=Ppat_xtr{txt;loc};} -> C.xtr (ploc_of_location loc) txt
                                 )
      ; add_branches_expr_code = (function
          | {ppat_desc=Ppat_xtr{txt;loc};} -> C.xtr (ploc_of_location loc) txt
                                 )
      }
    ; pattern_desc = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; structure_item = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; structure_item_desc = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; type_kind = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; type_declaration = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; core_type = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      ; add_branches_patt_code = (function
          | {ptyp_desc=Ptyp_xtr{txt;loc};} -> C.xtr (ploc_of_location loc) txt
                                 )
      ; add_branches_expr_code = (function
          | {ptyp_desc=Ptyp_xtr{txt;loc};} -> C.xtr (ploc_of_location loc) txt
                                 )
      }
    ; core_type_desc = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; class_structure = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; str_vala = {
        data_source_module = Ast_helper
      ; quotation_source_module = Reorg_parsetree
      }
    }
  ; entrypoints = [
      {name = "expression"; from_string = parse_expression ; type_name = expression }
    ; {name = "module_expr"; from_string = parse_module_expr ; type_name = module_expr }
    ; {name = "pattern"; from_string = parse_pattern ; type_name = pattern }
    ; {name = "core_type"; from_string = parse_core_type ; type_name = core_type }
    ; {name = "longident_t"; from_string = parse_longident ; type_name = longident_t }
    ; {name = "extended_module_path"; from_string = parse_extended_module_path ; type_name = longident_t }
    ; {name = "structure_item"; from_string = parse_structure_item ; type_name = structure_item }
    ; {name = "constructor_declaration"; from_string = parse_constructor_declaration ; type_name = constructor_declaration }
    ; {name = "attribute"; from_string = parse_attribute ; type_name = attribute }
    ; {name = "field"; from_string = parse_label_declaration ; type_name = label_declaration }
    ; {name = "case"; from_string = parse_match_case ; type_name = case }
    ; {name = "value_binding"; from_string = parse_value_binding ; type_name = value_binding }
    ; {name = "arg_label"; from_string = parse_arg_label ; type_name = arg_label }
    ; {name = "lident_loc"; from_string = parse_lident_vala_loc ; type_name = str_vala }
    ; {name = "extension_constructor"; from_string = parse_extension_constructor ; type_name = extension_constructor }
    ]
 }]

end
