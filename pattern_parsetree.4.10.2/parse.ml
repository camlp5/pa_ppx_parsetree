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

(* Entry points in the parser *)

(* Skip tokens to the end of the phrase *)

let last_token = ref Parser.EOF

let token lexbuf =
  let token = Lexer.token lexbuf in
  last_token := token;
  token

let rec skip_phrase lexbuf =
  match token lexbuf with
  | Parser.SEMISEMI | Parser.EOF -> ()
  | _ -> skip_phrase lexbuf
  | exception (Lexer.Error (Lexer.Unterminated_comment _, _)
              | Lexer.Error (Lexer.Unterminated_string, _)
              | Lexer.Error (Lexer.Reserved_sequence _, _)
              | Lexer.Error (Lexer.Unterminated_string_in_comment _, _)
              | Lexer.Error (Lexer.Illegal_character _, _)) ->
      skip_phrase lexbuf

let maybe_skip_phrase lexbuf =
  match !last_token with
  | Parser.SEMISEMI | Parser.EOF -> ()
  | _ -> skip_phrase lexbuf

type 'a parser =
  (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> 'a

let wrap (parser : 'a parser) lexbuf : 'a =
  try
    Docstrings.init ();
    Lexer.init ();
    let ast = parser token lexbuf in
    Parsing.clear_parser();
    Docstrings.warn_bad_docstrings ();
    last_token := Parser.EOF;
    ast
  with
  | Lexer.Error(Lexer.Illegal_character _, _) as err
    when !Location.input_name = "//toplevel//"->
      skip_phrase lexbuf;
      raise err
  | Syntaxerr.Error _ as err
    when !Location.input_name = "//toplevel//" ->
      maybe_skip_phrase lexbuf;
      raise err
  | Parsing.Parse_error | Syntaxerr.Escape_error ->
      let loc = Location.curr lexbuf in
      if !Location.input_name = "//toplevel//"
      then maybe_skip_phrase lexbuf;
      raise(Syntaxerr.Error(Syntaxerr.Other loc))

let implementation = wrap Parser.implementation
and interface = wrap Parser.interface
and toplevel_phrase = wrap Parser.toplevel_phrase
and use_file = wrap Parser.use_file
and core_type = wrap Parser.parse_core_type
and expression = wrap Parser.parse_expression
and pattern = wrap Parser.parse_pattern
(*-*)let longident = wrap Parser.parse_mty_longident
(*-*)let val_ident = wrap Parser.parse_val_longident
(*-*)let constr_ident= wrap Parser.parse_constr_longident
(*-*)let extended_module_path =
(*-*)  wrap Parser.parse_mod_ext_longident
(*-*)let simple_module_path = wrap Parser.parse_mod_longident
(*-*)let type_ident = wrap Parser.parse_mty_longident
(*-*)let module_type = wrap Parser.parse_module_type
(*-*)let module_expr = wrap Parser.parse_module_expr
(*-*)let structure_item = wrap Parser.parse_structure_item
(*-*)let structure = wrap Parser.parse_structure
(*-*)let signature_item = wrap Parser.parse_signature_item
(*-*)let signature = wrap Parser.parse_signature
(*-*)let constructor_declaration = wrap Parser.parse_constructor_declaration
(*-*)let attribute = wrap Parser.parse_attribute
(*-*)let extension = wrap Parser.parse_extension
(*-*)let label_declaration = wrap Parser.parse_label_declaration
(*-*)let match_case = wrap Parser.parse_match_case
(*-*)let value_binding = wrap Parser.parse_value_binding
(*-*)let arg_label = wrap Parser.parse_arg_label
(*-*)let lident_vala_loc = wrap Parser.parse_lident_vala_loc
(*-*)let extension_constructor = wrap Parser.parse_extension_constructor
(*-*)let binding_op = wrap Parser.parse_binding_op
(*-*)let type_declaration = wrap Parser.parse_type_declaration
(*-*)let type_substitution = wrap Parser.parse_type_substitution

(*-*)let val_ident = wrap Parser.parse_val_longident
(*-*)let constr_ident= wrap Parser.parse_constr_longident
(*-*)let extended_module_path =
(*-*)  wrap Parser.parse_mod_ext_longident
(*-*)let simple_module_path = wrap Parser.parse_mod_longident
(*-*)let type_ident = wrap Parser.parse_mty_longident
(*-*)let constant = wrap Parser.parse_constant
(*-*)let row_field = wrap Parser.parse_row_field
(*-*)let object_field = wrap Parser.parse_object_field
(*-*)let class_description = wrap Parser.parse_class_description
(*-*)let class_expr = wrap Parser.parse_class_expr
(*-*)let class_type = wrap Parser.parse_class_type
(*-*)let class_field = wrap Parser.parse_class_field
(*-*)let functor_parameter = wrap Parser.parse_functor_parameter
(*-*)let module_declaration = wrap Parser.parse_module_declaration
(*-*)let with_constraint = wrap Parser.parse_with_constraint
(*-*)let class_type_field = wrap Parser.parse_class_type_field

(* Error reporting for Syntaxerr *)
(* The code has been moved here so that one can reuse Pprintast.tyvar *)

let prepare_error err =
  let open Syntaxerr in
  match err with
  | Unclosed(opening_loc, opening, closing_loc, closing) ->
      Location.errorf
        ~loc:closing_loc
        ~sub:[
          Location.msg ~loc:opening_loc
            "This '%s' might be unmatched" opening
        ]
        "Syntax error: '%s' expected" closing

  | Expecting (loc, nonterm) ->
      Location.errorf ~loc "Syntax error: %s expected." nonterm
  | Not_expecting (loc, nonterm) ->
      Location.errorf ~loc "Syntax error: %s not expected." nonterm
  | Applicative_path loc ->
      Location.errorf ~loc
        "Syntax error: applicative paths of the form F(X).t \
         are not supported when the option -no-app-func is set."
  | Variable_in_scope (loc, var) ->
      Location.errorf ~loc
        "In this scoped type, variable %a \
         is reserved for the local type %s."
        Pprintast.tyvar var var
  | Other loc ->
      Location.errorf ~loc "Syntax error"
  | Ill_formed_ast (loc, s) ->
      Location.errorf ~loc
        "broken invariant in parsetree: %s" s
  | Invalid_package_type (loc, s) ->
      Location.errorf ~loc "invalid package type: %s" s

let () =
  Location.register_error_of_exn
    (function
      | Syntaxerr.Error err -> Some (prepare_error err)
      | _ -> None
    )
