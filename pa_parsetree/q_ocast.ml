(** -syntax camlp5o $(MIGRATE_OCAMLCFLAGS) -package pa_ppx.import,pa_ppx_q_ast,camlp5.parser_quotations *)
(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)


module Regular = struct

module MetaE = struct
  include Q_ast_base.E_MetaSig
  let int n = let loc = Ploc.dummy in <:expr< $int:string_of_int n$ >>
end

module MetaP = struct
  include Q_ast_base.P_MetaSig
  let int n = let loc = Ploc.dummy in <:patt< $int:string_of_int n$ >>
end

[%%import: Import_ocast.attribute]
[@@deriving q_ast {
    default_data_source_module = Import_ocast
  ; expr_meta_module = MetaE
  ; patt_meta_module = MetaP
  ; loc_mode = NoLoc
  ; pertype = {
      location = {
        data_source_module = Location
      ; quotation_source_module = Import_ocast
      }
    ; position = {
        data_source_module = Lexing
      ; quotation_source_module = Import_ocast
      }
    ; constant = {
        data_source_module = Parsetree
      ; quotation_source_module = Import_ocast
      }
    ; expression = {
        data_source_module = Parsetree
      ; quotation_source_module = Import_ocast
      }
    ; expression_desc = {
        data_source_module = Parsetree
      ; quotation_source_module = Import_ocast
      }
    }
  ; entrypoints = [
      {name = "expression"; grammar_entry = Pa_ocast.expression_eoi ; type_name = expression }
    ]
 }]

end
