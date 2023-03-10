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
    let vala elem x =
      match Pa_ppx_q_ast_runtime.MetaE.vala elem x with
        <:expr< Ploc.VaVal $e$ >> -> e
      | e -> Ploc.raise (loc_of_expr e)
               (Failure Fmt.(str "Sexp_example.NoVala.vala: unexpected result expr:@ %a"
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
               (Failure Fmt.(str "Sexp_example.NoVala.vala: unexpected result patt:@ %a"
                               Pp_MLast.pp_patt e))
end

let parse_expression s =
  Pa_ppx_parsetree_pattern_parsetree.Parse.expression (Lexing.from_string s)

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
      }
    ; expression_desc = {
        data_source_module = Parsetree
      ; quotation_source_module = Reorg_parsetree
      }
    }
  ; entrypoints = [
      {name = "expression"; from_string = parse_expression ; type_name = expression }
    ]
 }]

end
