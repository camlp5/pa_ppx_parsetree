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
val module_type : Lexing.lexbuf -> Parsetree.module_type
val module_expr : Lexing.lexbuf -> Parsetree.module_expr
(*-*)val structure_item : Lexing.lexbuf -> Parsetree.structure_item
(*-*)val structure : Lexing.lexbuf -> Parsetree.structure
(*-*)val signature_item : Lexing.lexbuf -> Parsetree.signature_item
(*-*)val signature : Lexing.lexbuf -> Parsetree.signature
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
(*-*)val longlident : Lexing.lexbuf -> Longident.t

(** The functions below can be used to parse Longident safely. *)

val longident: Lexing.lexbuf -> Longident.t
(**
   The function [longident] is guaranteed to parse all subclasses
   of {!Longident.t} used in OCaml: values, constructors, simple or extended
   module paths, and types or module types.

   However, this function accepts inputs which are not accepted by the
   compiler, because they combine functor applications and infix operators.
   In valid OCaml syntax, only value-level identifiers may end with infix
   operators [Foo.( + )].
   Moreover, in value-level identifiers the module path [Foo] must be simple
   ([M.N] rather than [F(X)]): functor applications may only appear in
   type-level identifiers.
   As a consequence, a path such as [F(X).( + )] is not a valid OCaml
   identifier; but it is accepted by this function.
*)

(** The next functions are specialized to a subclass of {!Longident.t} *)

val val_ident: Lexing.lexbuf -> Longident.t
(**
   This function parses a syntactically valid path for a value. For instance,
   [x], [M.x], and [(+.)] are valid. Contrarily, [M.A], [F(X).x], and [true]
   are rejected.

   Longident for OCaml's value cannot contain functor application.
   The last component of the {!Longident.t} is not capitalized,
   but can be an operator [A.Path.To.(.%.%.(;..)<-)]
*)

val constr_ident: Lexing.lexbuf -> Longident.t
(**
   This function parses a syntactically valid path for a variant constructor.
   For instance, [A], [M.A] and [M.(::)] are valid, but both [M.a]
   and [F(X).A] are rejected.

   Longident for OCaml's variant constructors cannot contain functor
   application.
   The last component of the {!Longident.t} is capitalized,
   or it may be one the special constructors: [true],[false],[()],[[]],[(::)].
   Among those special constructors, only [(::)] can be prefixed by a module
   path ([A.B.C.(::)]).
*)


val simple_module_path: Lexing.lexbuf -> Longident.t
(**
   This function parses a syntactically valid path for a module.
   For instance, [A], and [M.A] are valid, but both [M.a]
   and [F(X).A] are rejected.

   Longident for OCaml's module cannot contain functor application.
   The last component of the {!Longident.t} is capitalized.
*)


val extended_module_path: Lexing.lexbuf -> Longident.t
(**
   This function parse syntactically valid path for an extended module.
   For instance, [A.B] and [F(A).B] are valid. Contrarily,
   [(.%())] or [[]] are both rejected.

   The last component of the {!Longident.t} is capitalized.

*)

val type_ident: Lexing.lexbuf -> Longident.t
(**
   This function parse syntactically valid path for a type or a module type.
   For instance, [A], [t], [M.t] and [F(X).t] are valid. Contrarily,
   [(.%())] or [[]] are both rejected.

   In path for type and module types, only operators and special constructors
   are rejected.

*)
