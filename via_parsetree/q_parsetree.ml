(**pp -syntax camlp5o $(MIGRATE_OCAMLCFLAGS) -package pa_ppx.import,pa_ppx_q_ast,camlp5.parser_quotations *)
(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)


module Regular = struct

module MetaE = struct
  include Q_ast_base.E_MetaSig
  let int n = let loc = Ploc.dummy in <:expr< $int:string_of_int n$ >>
  open Pa_ppx_base
  open MLast
    let xtr s = <:expr< $lid:s$ >>
    let vala elem x =
      match x with
        Ploc.VaVal p -> elem p
      | Ploc.VaAnt s -> xtr s
end

module MetaP = struct
  include Q_ast_base.P_MetaSig
  let int n = let loc = Ploc.dummy in <:patt< $int:string_of_int n$ >>
  open Pa_ppx_base
  open MLast
    let xtr = function
        "_" -> <:patt< _ >>
       | s -> <:patt< $lid:s$ >>
    let vala elem x =
      match x with
        Ploc.VaVal p -> elem p
      | Ploc.VaAnt "_" -> <:patt< _ >>
      | Ploc.VaAnt s -> xtr s
end

let parse_expression s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.expression (Lexing.from_string s)

let parse_pattern s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.pattern (Lexing.from_string s)

let parse_core_type s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.core_type (Lexing.from_string s)

let parse_longident s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.longident (Lexing.from_string s)

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
          | {pexp_desc=Pexp_xtr{txt = s;};} -> C.xtr s)
      ; add_branches_expr_code = (function
          | {pexp_desc=Pexp_xtr{txt = s;};} -> C.xtr s)
      }
    ; expression_desc = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    ; pattern = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      ; add_branches_patt_code = (function
          | {ppat_desc=Ppat_xtr{txt = s;};} -> C.xtr s)
      ; add_branches_expr_code = (function
          | {ppat_desc=Ppat_xtr{txt = s;};} -> C.xtr s)
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
          | {ptyp_desc=Ptyp_xtr{txt = s;};} -> C.xtr s)
      ; add_branches_expr_code = (function
          | {ptyp_desc=Ptyp_xtr{txt = s;};} -> C.xtr s)
      }
    ; core_type_desc = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    }
  ; entrypoints = [
      {name = "expression"; from_string = parse_expression ; type_name = expression }
    ; {name = "pattern"; from_string = parse_pattern ; type_name = pattern }
    ; {name = "core_type"; from_string = parse_core_type ; type_name = core_type }
    ; {name = "longident_t"; from_string = parse_longident ; type_name = longident_t }
    ; {name = "extended_module_path"; from_string = parse_extended_module_path ; type_name = longident_t }
    ; {name = "structure_item"; from_string = parse_structure_item ; type_name = structure_item }
    ; {name = "constructor_declaration"; from_string = parse_constructor_declaration ; type_name = constructor_declaration }
    ; {name = "attribute"; from_string = parse_attribute ; type_name = attribute }
    ; {name = "field"; from_string = parse_label_declaration ; type_name = label_declaration }
    ; {name = "case"; from_string = parse_match_case ; type_name = case }
    ]
 }]

end
