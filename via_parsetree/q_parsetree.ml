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
    let xtr s = <:patt< $lid:s$ >>
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

[%%import: Reorg_parsetree.attribute]
[@@deriving q_ast {
    default_data_source_module = Reorg_parsetree
  ; expr_meta_module = MetaE
  ; patt_meta_module = MetaP
  ; loc_mode = CustomLoc { loc_varname = __loc__ ; loc_type = [%typ: location] ; loc_function_name = location }
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
    ; position = {
        data_source_module = Lexing
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
    ]
 }]

end
