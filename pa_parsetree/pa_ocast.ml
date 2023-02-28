(**pp -syntax camlp5r -package camlp5.parser_quotations,camlp5.extend_m *)
(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pcaml;

value expression_eoi = Grammar.Entry.create gram "expression_eoi";

EXTEND
  GLOBAL: expr expression_eoi;

  expression: [
    [
      e = expr -> MLast2Pattern_OCast.expr e
    ]
  ]
  ;

  expression_eoi: [ [ x = expression; EOI -> x ] ];

END;
