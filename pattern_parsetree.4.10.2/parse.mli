(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Entry points in the parser

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

val implementation : Lexing.lexbuf -> Parsetree.structure
val interface : Lexing.lexbuf -> Parsetree.signature
val toplevel_phrase : Lexing.lexbuf -> Parsetree.toplevel_phrase
val use_file : Lexing.lexbuf -> Parsetree.toplevel_phrase list
val core_type : Lexing.lexbuf -> Parsetree.core_type
val expression : Lexing.lexbuf -> Parsetree.expression
val pattern : Lexing.lexbuf -> Parsetree.pattern
(*-*)val module_type : Lexing.lexbuf -> Parsetree.module_type
(*-*)val module_expr : Lexing.lexbuf -> Parsetree.module_expr
(*-*)val structure_item : Lexing.lexbuf -> Parsetree.structure_item
(*-*)val signature_item : Lexing.lexbuf -> Parsetree.signature_item
(*-*)val constructor_declaration : Lexing.lexbuf -> Parsetree.constructor_declaration
(*-*)val attribute : Lexing.lexbuf -> Parsetree.attribute
(*-*)val extension : Lexing.lexbuf -> Parsetree.extension
(*-*)val label_declaration : Lexing.lexbuf -> Parsetree.label_declaration
(*-*)val match_case : Lexing.lexbuf -> Parsetree.case
(*-*)val value_binding : Lexing.lexbuf -> Parsetree.value_binding
(*-*)val arg_label : Lexing.lexbuf -> Asttypes.arg_label
(*-*)val lident_vala_loc : Lexing.lexbuf -> Ast_helper.str_vala
(*-*)val extension_constructor : Lexing.lexbuf -> Parsetree.extension_constructor
(*-*)val binding_op : Lexing.lexbuf -> Parsetree.binding_op
(*-*)val type_declaration : Lexing.lexbuf -> Parsetree.type_declaration
(*-*)val type_substitution : Lexing.lexbuf -> Parsetree.type_declaration
(*-*)val constant : Lexing.lexbuf -> Parsetree.constant
(*-*)val row_field : Lexing.lexbuf -> Parsetree.row_field
(*-*)val object_field : Lexing.lexbuf -> Parsetree.object_field
(*-*)val class_description : Lexing.lexbuf -> Parsetree.class_description
(*-*)val class_expr : Lexing.lexbuf -> Parsetree.class_expr
(*-*)val class_type : Lexing.lexbuf -> Parsetree.class_type
(*-*)val class_field : Lexing.lexbuf -> Parsetree.class_field
(*-*)val functor_parameter : Lexing.lexbuf -> Parsetree.functor_parameter Ploc.vala
(*-*)val module_declaration : Lexing.lexbuf -> Parsetree.module_declaration
(*-*)val with_constraint : Lexing.lexbuf -> Parsetree.with_constraint
(*-*)val class_type_field : Lexing.lexbuf -> Parsetree.class_type_field
