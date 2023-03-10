
(* The type of tokens. *)

type token = 
  | WITH
  | WHILE
  | WHEN
  | VIRTUAL
  | VAL
  | UNDERSCORE
  | UIDENT of (string)
  | TYPE
  | TRY
  | TRUE
  | TO
  | TILDE
  | THEN
  | STRUCT
  | STRING of (string * Location.t * string option)
  | STAR
  | SIG
  | SEMISEMI
  | SEMI
  | RPAREN
  | REC
  | RBRACKET
  | RBRACE
  | QUOTED_STRING_ITEM of (string * Location.t * string * Location.t * string option)
  | QUOTED_STRING_EXPR of (string * Location.t * string * Location.t * string option)
  | QUOTE
  | QUESTION
  | PRIVATE
  | PREFIXOP of (string)
  | PLUSEQ
  | PLUSDOT
  | PLUS
  | PERCENT
  | OR
  | OPTLABEL of (string)
  | OPEN
  | OF
  | OBJECT
  | NONREC
  | NEW
  | MUTABLE
  | MODULE
  | MINUSGREATER
  | MINUSDOT
  | MINUS
  | METHOD
  | MATCH
  | LPAREN
  | LIDENT of (string)
  | LETOP of (string)
  | LET
  | LESSMINUS
  | LESS
  | LBRACKETPERCENTPERCENT
  | LBRACKETPERCENT
  | LBRACKETLESS
  | LBRACKETGREATER
  | LBRACKETBAR
  | LBRACKETATATAT
  | LBRACKETATAT
  | LBRACKETAT
  | LBRACKET
  | LBRACELESS
  | LBRACE
  | LAZY
  | LABEL of (string)
  | INT of (string * char option)
  | INITIALIZER
  | INHERIT
  | INFIXOP4 of (string)
  | INFIXOP3 of (string)
  | INFIXOP2 of (string)
  | INFIXOP1 of (string)
  | INFIXOP0 of (string)
  | INCLUDE
  | IN
  | IF
  | HASHOP of (string)
  | HASH
  | GREATERRBRACKET
  | GREATERRBRACE
  | GREATER
  | FUNCTOR
  | FUNCTION
  | FUN
  | FOR
  | FLOAT of (string * char option)
  | FALSE
  | EXTERNAL
  | EXCEPTION
  | EQUAL
  | EOL
  | EOF
  | END
  | ELSE
  | DOWNTO
  | DOTOP of (string)
  | DOTDOT
  | DOT
  | DONE
  | DOCSTRING of (Docstrings.docstring)
  | DO
  | CONSTRAINT
  | COMMENT of (string * Location.t)
  | COMMA
  | COLONGREATER
  | COLONEQUAL
  | COLONCOLON
  | COLON
  | CLASS
  | CHAR of (char)
  | BEGIN
  | BARRBRACKET
  | BARBAR
  | BAR
  | BANG
  | BACKQUOTE
  | ASSERT
  | AS
  | ANTI_WITHE of (string * Location.t)
  | ANTI_WHENO of (string * Location.t)
  | ANTI_UID of (string * Location.t)
  | ANTI_TYP of (string * Location.t)
  | ANTI_TUPLELIST of (string * Location.t)
  | ANTI_PRIV of (string * Location.t)
  | ANTI_MUTABLE of (string * Location.t)
  | ANTI_LONGID of (string * Location.t)
  | ANTI_LIST of (string * Location.t)
  | ANTI_LID of (string * Location.t)
  | ANTI_CONSTRUCTORLIST of (string * Location.t)
  | ANTI_ALGATTRS of (string * Location.t)
  | ANTI of (string * Location.t)
  | ANDOP of (string)
  | AND
  | AMPERSAND
  | AMPERAMPER

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val use_file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.toplevel_phrase list)

val toplevel_phrase: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.toplevel_phrase)

val parse_val_longident: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Longident.t)

val parse_structure_item: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.structure_item)

val parse_pattern: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.pattern)

val parse_mty_longident: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Longident.t)

val parse_module_type: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.module_type)

val parse_module_expr: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.module_expr)

val parse_mod_longident: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Longident.t)

val parse_mod_ext_longident: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Longident.t)

val parse_match_case: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.case)

val parse_label_declaration: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.label_declaration)

val parse_expression: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.expression)

val parse_core_type: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.core_type)

val parse_constructor_declaration: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.constructor_declaration)

val parse_constr_longident: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Longident.t)

val parse_attribute: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.attribute)

val parse_any_longident: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Longident.t)

val interface: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.signature)

val implementation: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.structure)
