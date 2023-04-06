type token =
    WITH
  | WHILE
  | WHEN
  | VIRTUAL
  | VAL
  | UNDERSCORE
  | UIDENT of string
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
  | QUOTED_STRING_ITEM of
      (string * Location.t * string * Location.t * string option)
  | QUOTED_STRING_EXPR of
      (string * Location.t * string * Location.t * string option)
  | QUOTE
  | QUESTION
  | PRIVATE
  | PREFIXOP of string
  | PLUSEQ
  | PLUSDOT
  | PLUS
  | PERCENT
  | OR
  | OPTLABEL of string
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
  | LIDENT of string
  | LETOP of string
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
  | LABEL of string
  | INT of (string * char option)
  | INITIALIZER
  | INHERIT
  | INFIXOP4 of string
  | INFIXOP3 of string
  | INFIXOP2 of string
  | INFIXOP1 of string
  | INFIXOP0 of string
  | INCLUDE
  | IN
  | IF
  | HASHOP of string
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
  | DOTOP of string
  | DOTDOT
  | DOT
  | DONE
  | DOCSTRING of Docstrings.docstring
  | DO
  | CONSTRAINT
  | COMMENT of (string * Location.t)
  | COMMA
  | COLONGREATER
  | COLONEQUAL
  | COLONCOLON
  | COLON
  | CLASS
  | CHAR of char
  | BEGIN
  | BARRBRACKET
  | BARBAR
  | BAR
  | BANG
  | BACKQUOTE
  | ASSERT
  | AS
  | ANTI_WITHE of string
  | ANTI_WHENO of string
  | ANTI_UIDOPT of string
  | ANTI_UID of string
  | ANTI_TYP of string
  | ANTI_TUPLELIST of string
  | ANTI_STRING of string
  | ANTI_RECFLAG of string
  | ANTI_PRIV of string
  | ANTI_PATTOPT of string
  | ANTI_PATT of string
  | ANTI_OVERRIDEFLAG of string
  | ANTI_NONRECFLAG of string
  | ANTI_NATIVEINT of string
  | ANTI_MUTABLE of string
  | ANTI_LONGID of string
  | ANTI_LIST of string
  | ANTI_LID of string
  | ANTI_LETOP of string
  | ANTI_LABEL of string
  | ANTI_INT64 of string
  | ANTI_INT32 of string
  | ANTI_INT of string
  | ANTI_ID of string
  | ANTI_FLOAT of string
  | ANTI_EXPROPT of string
  | ANTI_EXCON of string
  | ANTI_DIRFLAG of string
  | ANTI_DELIM of string
  | ANTI_CTYPOPT of string
  | ANTI_CONSTRUCTORLIST of string
  | ANTI_CONSTANT of string
  | ANTI_CLOSEDFLAG of string
  | ANTI_CHAR of string
  | ANTI_ATTRID of string
  | ANTI_ALGATTRS of string
  | ANTI of string
  | ANDOP of string
  | AND
  | AMPERSAND
  | AMPERAMPER
val mkloc : 'a -> Location.t -> 'a Location.loc
val mknoloc : 'a -> 'a Location.loc
val make_loc : Lexing.position * Lexing.position -> Location.t
val ghost_loc : Lexing.position * Lexing.position -> Location.t
val mktyp :
  loc:Lexing.position * Lexing.position ->
  ?attrs:Ast_helper.attrs -> Parsetree.core_type_desc -> Parsetree.core_type
val mkpat :
  loc:Lexing.position * Lexing.position ->
  Parsetree.pattern_desc -> Parsetree.pattern
val mkexp :
  loc:Lexing.position * Lexing.position ->
  Parsetree.expression_desc -> Parsetree.expression
val mkmty :
  loc:Lexing.position * Lexing.position ->
  ?attrs:Ast_helper.attrs ->
  Parsetree.module_type_desc -> Parsetree.module_type
val mksig :
  loc:Lexing.position * Lexing.position ->
  Parsetree.signature_item_desc -> Parsetree.signature_item
val mkmod :
  loc:Lexing.position * Lexing.position ->
  ?attrs:Ast_helper.attrs ->
  Parsetree.module_expr_desc -> Parsetree.module_expr
val mkstr :
  loc:Lexing.position * Lexing.position ->
  Parsetree.structure_item_desc -> Parsetree.structure_item
val mkclass :
  loc:Lexing.position * Lexing.position ->
  ?attrs:Ast_helper.attrs ->
  Parsetree.class_expr_desc -> Parsetree.class_expr
val mkcty :
  loc:Lexing.position * Lexing.position ->
  ?attrs:Ast_helper.attrs ->
  Parsetree.class_type_desc -> Parsetree.class_type
val pstr_typext :
  Parsetree.type_extension * 'a -> Parsetree.structure_item_desc * 'a
val pstr_primitive :
  Parsetree.value_description * 'a -> Parsetree.structure_item_desc * 'a
val pstr_type :
  (Asttypes.rec_flag Ploc.vala * 'a) *
  Parsetree.type_declaration list Ploc.vala ->
  Parsetree.structure_item_desc * 'a
val pstr_exception :
  Parsetree.type_exception * 'a -> Parsetree.structure_item_desc * 'a
val pstr_include :
  Parsetree.include_declaration * 'a -> Parsetree.structure_item_desc * 'a
val pstr_recmodule :
  'a * Parsetree.module_binding list -> Parsetree.structure_item_desc * 'a
val psig_typext :
  Parsetree.type_extension * 'a -> Parsetree.signature_item_desc * 'a
val psig_value :
  Parsetree.value_description * 'a -> Parsetree.signature_item_desc * 'a
val psig_type :
  (Asttypes.rec_flag Ploc.vala * 'a) *
  Parsetree.type_declaration list Ploc.vala ->
  Parsetree.signature_item_desc * 'a
val psig_typesubst :
  (Asttypes.rec_flag * 'a) * Parsetree.type_declaration list Ploc.vala ->
  Parsetree.signature_item_desc * 'a
val psig_exception :
  Parsetree.type_exception * 'a -> Parsetree.signature_item_desc * 'a
val psig_include :
  Parsetree.include_description * 'a -> Parsetree.signature_item_desc * 'a
val mkctf :
  loc:Lexing.position * Lexing.position ->
  ?attrs:Ast_helper.attrs ->
  ?docs:Docstrings.docs ->
  Parsetree.class_type_field_desc -> Parsetree.class_type_field
val mkcf :
  loc:Lexing.position * Lexing.position ->
  ?attrs:Ast_helper.attrs ->
  ?docs:Docstrings.docs ->
  Parsetree.class_field_desc -> Parsetree.class_field
val mkrhs : 'a -> Lexing.position * Lexing.position -> 'a Location.loc
val ghrhs : 'a -> Lexing.position * Lexing.position -> 'a Location.loc
val push_loc : Location.t -> Location.t list -> Location.t list
val reloc_pat :
  loc:Lexing.position * Lexing.position ->
  Parsetree.pattern -> Parsetree.pattern
val reloc_exp :
  loc:Lexing.position * Lexing.position ->
  Parsetree.expression -> Parsetree.expression
val reloc_typ :
  loc:Lexing.position * Lexing.position ->
  Parsetree.core_type -> Parsetree.core_type
val mkexpvar :
  loc:Lexing.position * Lexing.position ->
  string Ploc.vala -> Parsetree.expression
val mkoperator :
  loc:Lexing.position * Lexing.position ->
  string Ploc.vala -> Parsetree.expression
val mkpatvar :
  loc:Lexing.position * Lexing.position ->
  string Ploc.vala -> Parsetree.pattern
val ghexp :
  loc:Lexing.position * Lexing.position ->
  Parsetree.expression_desc -> Parsetree.expression
val ghpat :
  loc:Lexing.position * Lexing.position ->
  Parsetree.pattern_desc -> Parsetree.pattern
val ghtyp :
  loc:Lexing.position * Lexing.position ->
  Parsetree.core_type_desc -> Parsetree.core_type
val ghloc : loc:Lexing.position * Lexing.position -> 'a -> 'a Asttypes.loc
val ghstr :
  loc:Lexing.position * Lexing.position ->
  Parsetree.structure_item_desc -> Parsetree.structure_item
val ghsig :
  loc:Lexing.position * Lexing.position ->
  Parsetree.signature_item_desc -> Parsetree.signature_item
val mkinfix :
  Parsetree.expression ->
  Parsetree.expression -> Parsetree.expression -> Parsetree.expression_desc
val neg_string : string -> string
val mkuminus :
  oploc:Lexing.position * Lexing.position ->
  string -> Parsetree.expression -> Parsetree.expression_desc
val mkuplus :
  oploc:Lexing.position * Lexing.position ->
  string -> Parsetree.expression -> Parsetree.expression_desc
val mkexp_cons_desc :
  Lexing.position * Lexing.position ->
  Parsetree.expression -> Parsetree.expression_desc
val mkexp_cons :
  loc:Lexing.position * Lexing.position ->
  Lexing.position * Lexing.position ->
  Parsetree.expression -> Parsetree.expression
val mkpat_cons_desc :
  Lexing.position * Lexing.position ->
  Parsetree.pattern -> Parsetree.pattern_desc
val mkpat_cons :
  loc:Lexing.position * Lexing.position ->
  Lexing.position * Lexing.position -> Parsetree.pattern -> Parsetree.pattern
val ghexp_cons_desc :
  Lexing.position * Lexing.position ->
  Parsetree.expression -> Parsetree.expression_desc
val ghpat_cons_desc :
  Lexing.position * Lexing.position ->
  Parsetree.pattern -> Parsetree.pattern_desc
val mktailexp :
  Lexing.position * Lexing.position ->
  Parsetree.expression list ->
  Parsetree.expression_desc * (Lexing.position * Lexing.position)
val mktailpat :
  Lexing.position * Lexing.position ->
  Parsetree.pattern list ->
  Parsetree.pattern_desc * (Lexing.position * Lexing.position)
val mkstrexp :
  Parsetree.expression -> Parsetree.attributes -> Parsetree.structure_item
val mkexp_constraint :
  loc:Lexing.position * Lexing.position ->
  Parsetree.expression ->
  Parsetree.core_type option * Parsetree.core_type option ->
  Parsetree.expression
val mkexp_opt_constraint :
  loc:Lexing.position * Lexing.position ->
  Parsetree.expression ->
  (Parsetree.core_type option * Parsetree.core_type option) option ->
  Parsetree.expression
val mkpat_opt_constraint :
  loc:Lexing.position * Lexing.position ->
  Parsetree.pattern -> Parsetree.core_type option -> Parsetree.pattern
val syntax_error : unit -> 'a
val unclosed :
  string ->
  Lexing.position * Lexing.position ->
  string -> Lexing.position * Lexing.position -> 'a
val expecting : Lexing.position * Lexing.position -> string -> 'a
val removed_string_set : Lexing.position * Lexing.position -> 'a
val not_expecting : Lexing.position * Lexing.position -> string -> 'a
type paren_kind = Paren | Brace | Bracket
type index_dim = One | Two | Three | Many
type ('dot, 'index) array_family = {
  name :
    Lexing.position * Lexing.position ->
    'dot ->
    assign:bool -> paren_kind -> index_dim -> Longident.t Location.loc;
  index :
    Lexing.position * Lexing.position ->
    paren_kind ->
    'index -> index_dim * (Asttypes.arg_label * Parsetree.expression) list;
}
val bigarray_untuplify : Parsetree.expression -> Parsetree.expression list
val builtin_arraylike_name :
  Lexing.position * Lexing.position ->
  'a -> assign:bool -> paren_kind -> index_dim -> Longident.t Asttypes.loc
val builtin_arraylike_index :
  Lexing.position * Lexing.position ->
  paren_kind ->
  Parsetree.expression ->
  index_dim * (Asttypes.arg_label * Parsetree.expression) list
val builtin_indexing_operators : (unit, Parsetree.expression) array_family
val paren_to_strings : paren_kind -> string * string
val user_indexing_operator_name :
  Lexing.position * Lexing.position ->
  Longident.t Ploc.vala option * string ->
  assign:bool -> paren_kind -> index_dim -> Longident.t Asttypes.loc
val user_index :
  Lexing.position * Lexing.position ->
  'a ->
  Parsetree.expression list ->
  index_dim * (Asttypes.arg_label * Parsetree.expression) list
val user_indexing_operators :
  (Longident.t Ploc.vala option * string, Parsetree.expression list)
  array_family
val mk_indexop_expr :
  ('a, 'b) array_family ->
  loc:Lexing.position * Lexing.position ->
  Parsetree.expression * 'a * paren_kind * 'b * Parsetree.expression option ->
  Parsetree.expression
val indexop_unclosed_error :
  Lexing.position * Lexing.position ->
  paren_kind -> Lexing.position * Lexing.position -> 'a
val lapply :
  loc:Lexing.position * Lexing.position ->
  Longident.t Ploc.vala -> Longident.t Ploc.vala -> Longident.t
val loc_map : ('a -> 'b) -> 'a Location.loc -> 'b Location.loc
val make_ghost : 'a Asttypes.loc -> 'a Asttypes.loc
val loc_last : Longident.t Location.loc -> string Location.loc
val loc_vala_last :
  Longident.t Ploc.vala Location.loc -> string Ploc.vala Location.loc
val loc_lident : string Location.loc -> Longident.t Location.loc
val exp_of_longident : Longident.t Location.loc -> Parsetree.expression
val exp_of_label : string Asttypes.loc -> Parsetree.expression
val pat_of_label : Longident.t Ploc.vala Asttypes.loc -> Parsetree.pattern
val mk_newtypes :
  loc:Lexing.position * Lexing.position ->
  string Ploc.vala Asttypes.loc list ->
  Parsetree.expression -> Parsetree.expression
val wrap_type_annotation :
  loc:Lexing.position * Lexing.position ->
  Ast_helper.str_vala list ->
  Parsetree.core_type ->
  Parsetree.expression -> Parsetree.expression * Parsetree.core_type
val wrap_exp_attrs :
  loc:Lexing.position * Lexing.position ->
  Parsetree.expression ->
  string Ploc.vala Asttypes.loc option * Parsetree.attribute list ->
  Parsetree.expression
val mkexp_attrs :
  loc:Lexing.position * Lexing.position ->
  Parsetree.expression_desc ->
  string Ploc.vala Asttypes.loc option * Parsetree.attribute list ->
  Parsetree.expression
val wrap_typ_attrs :
  loc:Lexing.position * Lexing.position ->
  Parsetree.core_type ->
  string Ploc.vala Asttypes.loc option * Parsetree.attribute list ->
  Parsetree.core_type
val wrap_pat_attrs :
  loc:Lexing.position * Lexing.position ->
  Parsetree.pattern ->
  string Ploc.vala Asttypes.loc option * Parsetree.attribute list ->
  Parsetree.pattern
val mkpat_attrs :
  loc:Lexing.position * Lexing.position ->
  Parsetree.pattern_desc ->
  string Ploc.vala Asttypes.loc option * Parsetree.attribute list ->
  Parsetree.pattern
val wrap_class_attrs :
  loc:'a ->
  Parsetree.class_expr -> Parsetree.attribute list -> Parsetree.class_expr
val wrap_mod_attrs :
  loc:'a ->
  Parsetree.attribute list -> Parsetree.module_expr -> Parsetree.module_expr
val wrap_mty_attrs :
  loc:'a ->
  Parsetree.attribute list -> Parsetree.module_type -> Parsetree.module_type
val wrap_str_ext :
  loc:Lexing.position * Lexing.position ->
  Parsetree.structure_item ->
  string Ploc.vala Asttypes.loc option -> Parsetree.structure_item
val wrap_mkstr_ext :
  loc:Lexing.position * Lexing.position ->
  Parsetree.structure_item_desc * string Ploc.vala Asttypes.loc option ->
  Parsetree.structure_item
val wrap_sig_ext :
  loc:Lexing.position * Lexing.position ->
  Parsetree.signature_item ->
  string Ploc.vala Asttypes.loc option -> Parsetree.signature_item
val wrap_mksig_ext :
  loc:Lexing.position * Lexing.position ->
  Parsetree.signature_item_desc * string Ploc.vala Asttypes.loc option ->
  Parsetree.signature_item
val mk_quotedext :
  loc:Lexing.position * Lexing.position ->
  'a * Location.t * string * Location.t * string option ->
  'a Ast_helper.vala Location.loc * Parsetree.payload
val text_str : Lexing.position -> Parsetree.structure_item list
val text_sig : Lexing.position -> Parsetree.signature_item list
val text_cstr : Lexing.position -> Parsetree.class_field list
val text_csig : Lexing.position -> Parsetree.class_type_field list
val text_def : Lexing.position -> Parsetree.toplevel_phrase list
val extra_text :
  Lexing.position ->
  Lexing.position -> (Docstrings.text -> 'a list) -> 'a list -> 'a list
val extra_str :
  Lexing.position ->
  Lexing.position ->
  Parsetree.structure_item list -> Parsetree.structure_item list
val extra_sig :
  Lexing.position ->
  Lexing.position ->
  Parsetree.signature_item list -> Parsetree.signature_item list
val extra_cstr :
  Lexing.position ->
  Lexing.position -> Parsetree.class_field list -> Parsetree.class_field list
val extra_csig :
  Lexing.position ->
  Lexing.position ->
  Parsetree.class_type_field list -> Parsetree.class_type_field list
val extra_def :
  Lexing.position ->
  Lexing.position ->
  Parsetree.toplevel_phrase list -> Parsetree.toplevel_phrase list
val extra_rhs_core_type :
  Parsetree.core_type -> pos:Lexing.position -> Parsetree.core_type
type let_binding = {
  lb_pattern : Parsetree.pattern;
  lb_expression : Parsetree.expression;
  lb_is_pun : bool;
  lb_attributes : Parsetree.attributes;
  lb_docs : Docstrings.docs Lazy.t;
  lb_text : Docstrings.text Lazy.t;
  lb_loc : Location.t;
}
type let_bindings = {
  lbs_bindings : let_binding list Ploc.vala;
  lbs_rec : Asttypes.rec_flag Ploc.vala;
  lbs_extension : string Ploc.vala Asttypes.loc option;
}
val mklb :
  bool ->
  loc:Lexing.position * Lexing.position ->
  Parsetree.pattern * Parsetree.expression * bool ->
  Parsetree.attributes -> let_binding
val addlb : let_bindings -> let_binding -> let_bindings
val mklbs :
  string Ploc.vala Asttypes.loc option ->
  Asttypes.rec_flag Ploc.vala -> let_binding -> let_bindings
val vb_of_lb : let_binding -> Parsetree.value_binding
val val_of_let_bindings :
  loc:Lexing.position * Lexing.position ->
  let_bindings -> Parsetree.structure_item
val expr_of_let_bindings :
  loc:Lexing.position * Lexing.position ->
  let_bindings -> Parsetree.expression -> Parsetree.expression
val class_of_let_bindings :
  loc:Lexing.position * Lexing.position ->
  let_bindings -> Parsetree.class_expr -> Parsetree.class_expr
val package_type_of_module_type :
  Parsetree.module_type ->
  Longident.t Asttypes.loc *
  (Longident.t Asttypes.loc * Parsetree.core_type) list *
  Parsetree.attributes
val mk_directive_arg :
  loc:Lexing.position * Lexing.position ->
  Parsetree.directive_argument_desc -> Parsetree.directive_argument
val mk_directive :
  loc:Lexing.position * Lexing.position ->
  string Asttypes.loc ->
  Parsetree.directive_argument option -> Parsetree.toplevel_phrase
val menhir_begin_marker : int
val xv_xlist_vala_generic_type_declaration_vala_nonrec_flag_ANTI_NONRECFLAG__type_kind__generic_and_type_declaration_type_kind__ :
  (Asttypes.rec_flag Ast_helper.vala *
   string Ast_helper.vala Asttypes.loc option) *
  Parsetree.type_declaration list Ast_helper.vala
val xv_xlist_vala_generic_type_declaration_no_nonrec_flag_type_subst_kind__generic_and_type_declaration_type_subst_kind__ :
  (Asttypes.rec_flag * string Ast_helper.vala Asttypes.loc option) *
  Parsetree.type_declaration list Ast_helper.vala
val xv_xlist_rec_module_declaration_and_module_declaration_ :
  string Ast_helper.vala Asttypes.loc option *
  Parsetree.module_declaration list
val xv_xlist_rec_module_binding_and_module_binding_ :
  string Ast_helper.vala Asttypes.loc option * Parsetree.module_binding list
val xv_xlist_class_type_declaration_and_class_type_declaration_ :
  string Ast_helper.vala Asttypes.loc option *
  Parsetree.class_type_declaration list
val xv_xlist_class_description_and_class_description_ :
  string Ast_helper.vala Asttypes.loc option *
  Parsetree.class_description list
val xv_xlist_class_declaration_and_class_declaration_ :
  string Ast_helper.vala Asttypes.loc option *
  Parsetree.class_declaration list
val xv_wrap_mkstr_ext___anonymous_2_ : Parsetree.structure_item
val xv_wrap_mksig_ext___anonymous_6_ : Parsetree.signature_item
val xv_with_type_binder : Asttypes.private_flag
val xv_with_constraint : Parsetree.with_constraint
val xv_virtual_with_private_flag : Asttypes.private_flag
val xv_virtual_with_mutable_flag : Asttypes.mutable_flag
val xv_virtual_flag : Asttypes.virtual_flag
val xv_vaval_val_extra_ident_ : string Ast_helper.vala
val xv_vaval_type_parameters_ :
  (Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) list
  Ast_helper.vala
val xv_vaval_seq_expr_ : Parsetree.expression Ast_helper.vala
val xv_vaval_pattern_ : Parsetree.pattern Ast_helper.vala
val xv_vaval_label_longident_ : Longident.t Ast_helper.vala
val xv_vaval_ident_ : Asttypes.label Ast_helper.vala
val xv_vaval_constraints_ :
  (Parsetree.core_type * Parsetree.core_type * Ast_helper.loc) list
  Ast_helper.vala
val xv_vaval_constr_extra_ident_ : string Ast_helper.vala
val xv_vaval_class_self_pattern_ : Parsetree.pattern Ast_helper.vala
val xv_vaval_class_longident_ : Longident.t Ast_helper.vala
val xv_vaval_LIDENT_ : string Ast_helper.vala
val xv_vaval_LETOP_ : string Ast_helper.vala
val xv_vaval_ANDOP_ : string Ast_helper.vala
val xv_value_type :
  Asttypes.label Asttypes.loc * Asttypes.mutable_flag *
  Asttypes.virtual_flag * Parsetree.core_type
val xv_value_description :
  Parsetree.value_description * string Ast_helper.vala Asttypes.loc option
val xv_value_binding : Parsetree.value_binding
val xv_value :
  (Asttypes.label Asttypes.loc * Asttypes.mutable_flag *
   Parsetree.class_field_kind) *
  Parsetree.attributes
val xv_vala_val_ident_ANTI_LID_ : string Ast_helper.vala
val xv_vala_type_parameters_ANTI_LIST_ :
  (Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) list
  Ast_helper.vala
val xv_vala_signed_constant_ANTI_CONSTANT_ :
  Parsetree.constant Ast_helper.vala
val xv_vala_separated_or_terminated_nonempty_list_SEMI_record_expr_field__ANTI_LIST_ :
  (Longident.t Ast_helper.vala Asttypes.loc * Parsetree.expression) list
  Ast_helper.vala
val xv_vala_separated_or_terminated_nonempty_list_SEMI_object_expr_field__ANTI_LIST_ :
  (Asttypes.label Ast_helper.vala Asttypes.loc * Parsetree.expression) list
  Ast_helper.vala
val xv_vala_rec_flag_ANTI_RECFLAG_ : Asttypes.rec_flag Ast_helper.vala
val xv_vala_private_flag_ANTI_PRIV_ : Asttypes.private_flag Ast_helper.vala
val xv_vala_poly_type_no_attr_ANTI_TYP_ : Parsetree.core_type Ast_helper.vala
val xv_vala_pattern_semi_list_ANTI_LIST_ :
  Parsetree.pattern list Ast_helper.vala
val xv_vala_override_flag_ANTI_OVERRIDEFLAG_ :
  Asttypes.override_flag Ast_helper.vala
val xv_vala_option_UNDERSCORE__ANTI_CLOSEDFLAG_ : unit option Ast_helper.vala
val xv_vala_opt_default_ANTI_EXPROPT_ :
  Parsetree.expression option Ast_helper.vala
val xv_vala_nonrec_flag_ANTI_NONRECFLAG_ : Asttypes.rec_flag Ast_helper.vala
val xv_vala_nonempty_llist_labeled_simple_expr__ANTI_LIST_ :
  (Asttypes.arg_label * Parsetree.expression) list Ast_helper.vala
val xv_vala_mutable_flag_ANTI_MUTABLE_ :
  Asttypes.mutable_flag Ast_helper.vala
val xv_vala_module_name__ANTI_UIDOPT_ :
  string Ast_helper.vala option Ast_helper.vala
val xv_vala_mod_longident_ANTI_LONGID_ : Longident.t Ast_helper.vala
val xv_vala_mod_ext_longident_ANTI_LONGID_ : Longident.t Ast_helper.vala
val xv_vala_match_cases_ANTI_LIST_ : Parsetree.case list Ast_helper.vala
val xv_vala_lident_list_ANTI_LIST_ :
  string Ploc.vala Asttypes.loc list Ast_helper.vala
val xv_vala_label_longident_ANTI_LONGID_ : Longident.t Ast_helper.vala
val xv_vala_label_declarations_ANTI_LIST_ :
  Parsetree.label_declaration list Ast_helper.vala
val xv_vala_label_ANTI_LID_ : Asttypes.label Ast_helper.vala
val xv_vala_ioption_terminated_simple_expr_WITH___ANTI_WITHE_ :
  Parsetree.expression option Ast_helper.vala
val xv_vala_inline_separated_nonempty_llist_STAR_atomic_type__ANTI_LIST_ :
  Parsetree.core_type list Ast_helper.vala
val xv_vala_inline_private_flag_ANTI_PRIV_ :
  Asttypes.private_flag Ast_helper.vala
val xv_vala_ident_ANTI_LID_ : Asttypes.label Ast_helper.vala
val xv_vala_ident_ANTI_ID_ : Asttypes.label Ast_helper.vala
val xv_vala_extra_cstr_class_fields__ANTI_LIST_ :
  Parsetree.class_field list Ast_helper.vala
val xv_vala_expr_semi_list_ANTI_LIST_ :
  Parsetree.expression list Ast_helper.vala
val xv_vala_direction_flag_ANTI_DIRFLAG_ :
  Asttypes.direction_flag Ast_helper.vala
val xv_vala_core_type_ANTI_TYP_ : Parsetree.core_type Ast_helper.vala
val xv_vala_constructor_declarations_ANTI_CONSTRUCTORLIST_ :
  Parsetree.constructor_declaration list Ast_helper.vala
val xv_vala_constr_longident_ANTI_LONGID_ : Longident.t Ast_helper.vala
val xv_vala_constr_ident_ANTI_UID_ : string Ast_helper.vala
val xv_vala_constant_ANTI_CONSTANT_ : Parsetree.constant Ast_helper.vala
val xv_vala_attributes_ANTI_ALGATTRS_ : Parsetree.attributes Ast_helper.vala
val xv_vala_UIDENT_ANTI_UID_ : string Ast_helper.vala
val xv_vala_LIDENT_ANTI_LID_ : string Ast_helper.vala
val xv_val_longident : Longident.t
val xv_val_ident_vala : string Ast_helper.vala
val xv_val_ident : string
val xv_val_extra_ident : string
val xv_vaant_ANTI_WHENO_ :
  Parsetree.expression Ploc.vala option Ast_helper.vala
val xv_vaant_ANTI_TUPLELIST_ : Parsetree.expression list Ast_helper.vala
val xv_vaant_ANTI_LIST_ : let_binding list Ast_helper.vala
val xv_vaant_ANTI_EXPROPT_ : Parsetree.expression option Ast_helper.vala
val xv_use_file_element : Parsetree.toplevel_phrase list
val xv_use_file : Parsetree.toplevel_phrase list
val xv_typevar_list : Ast_helper.str_vala list
val xv_typevar : Asttypes.label Ast_helper.vala Asttypes.loc
val xv_type_variance : Asttypes.variance * Asttypes.injectivity
val xv_type_variable : Parsetree.core_type
val xv_type_synonym : Parsetree.core_type Ast_helper.vala option
val xv_type_subst_kind :
  Parsetree.type_kind * Asttypes.private_flag Ast_helper.vala *
  Parsetree.core_type Ast_helper.vala option
val xv_type_subst_declarations :
  (Asttypes.rec_flag * string Ast_helper.vala Asttypes.loc option) *
  Parsetree.type_declaration list Ast_helper.vala
val xv_type_parameters :
  (Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) list
val xv_type_parameter :
  Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)
val xv_type_longident : Longident.t
val xv_type_kind :
  Parsetree.type_kind * Asttypes.private_flag Ast_helper.vala *
  Parsetree.core_type Ast_helper.vala option
val xv_type_extension_extension_constructor_declaration_ :
  Parsetree.type_extension * string Ast_helper.vala Asttypes.loc option
val xv_type_extension_extension_constructor_ :
  Parsetree.type_extension * string Ast_helper.vala Asttypes.loc option
val xv_type_declarations :
  (Asttypes.rec_flag Ast_helper.vala *
   string Ast_helper.vala Asttypes.loc option) *
  Parsetree.type_declaration list Ast_helper.vala
val xv_type_constraint :
  Parsetree.core_type option * Parsetree.core_type option
val xv_tuple_type : Parsetree.core_type
val xv_toplevel_phrase : Parsetree.toplevel_phrase
val xv_toplevel_directive_argument : Parsetree.directive_argument_desc
val xv_toplevel_directive : Parsetree.toplevel_phrase
val xv_top_def_structure_item_ : Parsetree.toplevel_phrase
val xv_top_def_str_exp_ : Parsetree.toplevel_phrase
val xv_text_str_SEMISEMI : Parsetree.structure_item list
val xv_text_str_structure_item_ : Parsetree.structure_item list
val xv_text_str_str_exp_ : Parsetree.structure_item list
val xv_text_sig_SEMISEMI : Parsetree.signature_item list
val xv_text_sig_signature_item_ : Parsetree.signature_item list
val xv_text_def_top_def_structure_item__ : Parsetree.toplevel_phrase list
val xv_text_def_top_def_str_exp__ : Parsetree.toplevel_phrase list
val xv_text_def_mark_rhs_docs_toplevel_directive__ :
  Parsetree.toplevel_phrase list
val xv_text_cstr_class_field_ : Parsetree.class_field list
val xv_text_csig_class_sig_field_ : Parsetree.class_type_field list
val xv_terminated_vala_core_type_ANTI_TYP__EQUAL_ :
  Parsetree.core_type Ast_helper.vala
val xv_terminated_simple_expr_WITH_ : Parsetree.expression
val xv_tag_field : Parsetree.row_field
val xv_subtractive : string
val xv_structure_item : Parsetree.structure_item
val xv_structure_element : Parsetree.structure_item list
val xv_structure : Parsetree.structure_item list Ast_helper.vala
val xv_strict_binding : Parsetree.expression
val xv_str_type_extension :
  Parsetree.type_extension * string Ast_helper.vala Asttypes.loc option
val xv_str_exp : Parsetree.structure_item
val xv_str_exception_declaration :
  Parsetree.type_exception * string Ast_helper.vala Asttypes.loc option
val xv_single_attr_id : string
val xv_simple_pattern_not_ident_ : Parsetree.pattern_desc
val xv_simple_pattern_not_ident : Parsetree.pattern
val xv_simple_pattern : Parsetree.pattern
val xv_simple_expr_attrs :
  Parsetree.expression_desc *
  (string Ast_helper.vala Asttypes.loc option * Parsetree.attributes)
val xv_simple_expr_ : Parsetree.expression_desc
val xv_simple_expr : Parsetree.expression
val xv_simple_delimited_pattern : Parsetree.pattern
val xv_signed_constant : Parsetree.constant
val xv_signature_item : Parsetree.signature_item
val xv_signature_element : Parsetree.signature_item list
val xv_signature : Parsetree.signature_item list Ast_helper.vala
val xv_sig_type_extension :
  Parsetree.type_extension * string Ast_helper.vala Asttypes.loc option
val xv_sig_exception_declaration :
  Parsetree.type_exception * string Ast_helper.vala Asttypes.loc option
val xv_seq_expr : Parsetree.expression
val xv_separated_or_terminated_nonempty_list_SEMI_record_expr_field_ :
  (Longident.t Ast_helper.vala Asttypes.loc * Parsetree.expression) list
val xv_separated_or_terminated_nonempty_list_SEMI_pattern_ :
  Parsetree.pattern list
val xv_separated_or_terminated_nonempty_list_SEMI_object_expr_field_ :
  (Asttypes.label Ast_helper.vala Asttypes.loc * Parsetree.expression) list
val xv_separated_or_terminated_nonempty_list_SEMI_expr_ :
  Parsetree.expression list
val xv_separated_nontrivial_llist_STAR_atomic_type_ :
  Parsetree.core_type list
val xv_separated_nontrivial_llist_COMMA_expr_ : Parsetree.expression list
val xv_separated_nontrivial_llist_COMMA_core_type_ : Parsetree.core_type list
val xv_separated_nonempty_llist_COMMA_type_parameter_ :
  (Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) list
val xv_separated_nonempty_llist_COMMA_core_type_ : Parsetree.core_type list
val xv_separated_nonempty_llist_BAR_row_field_ : Parsetree.row_field list
val xv_separated_nonempty_llist_AND_with_constraint_ :
  Parsetree.with_constraint list
val xv_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ :
  Parsetree.core_type list
val xv_row_field_list : Parsetree.row_field list
val xv_row_field : Parsetree.row_field
val xv_reversed_separated_nontrivial_llist_STAR_atomic_type_ :
  Parsetree.core_type list
val xv_reversed_separated_nontrivial_llist_COMMA_expr_ :
  Parsetree.expression list
val xv_reversed_separated_nontrivial_llist_COMMA_core_type_ :
  Parsetree.core_type list
val xv_reversed_separated_nonempty_llist_STAR_atomic_type_ :
  Parsetree.core_type list
val xv_reversed_separated_nonempty_llist_COMMA_type_parameter_ :
  (Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) list
val xv_reversed_separated_nonempty_llist_COMMA_core_type_ :
  Parsetree.core_type list
val xv_reversed_separated_nonempty_llist_BAR_row_field_ :
  Parsetree.row_field list
val xv_reversed_separated_nonempty_llist_AND_with_constraint_ :
  Parsetree.with_constraint list
val xv_reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ :
  Parsetree.core_type list
val xv_reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ :
  Parsetree.case list
val xv_reversed_nonempty_llist_typevar_ :
  Asttypes.label Ast_helper.vala Asttypes.loc list
val xv_reversed_nonempty_llist_name_tag_vala_ :
  Asttypes.label Ast_helper.vala list
val xv_reversed_nonempty_llist_labeled_simple_expr_ :
  (Asttypes.arg_label * Parsetree.expression) list
val xv_reversed_nonempty_llist_functor_arg_ :
  (Lexing.position * Parsetree.functor_parameter) list
val xv_reversed_llist_preceded_CONSTRAINT_constrain__ :
  (Parsetree.core_type * Parsetree.core_type * Ast_helper.loc) list
val xv_reversed_bar_llist_extension_constructor_declaration_ :
  Parsetree.extension_constructor list
val xv_reversed_bar_llist_extension_constructor_ :
  Parsetree.extension_constructor list
val xv_reversed_bar_llist_constructor_declaration_ :
  Parsetree.constructor_declaration list
val xv_rev_reversed_separated_nontrivial_llist_STAR_atomic_type__ :
  Parsetree.core_type list
val xv_rev_reversed_separated_nontrivial_llist_COMMA_expr__ :
  Parsetree.expression list
val xv_rev_reversed_separated_nontrivial_llist_COMMA_core_type__ :
  Parsetree.core_type list
val xv_rev_reversed_separated_nonempty_llist_COMMA_type_parameter__ :
  (Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) list
val xv_rev_reversed_separated_nonempty_llist_COMMA_core_type__ :
  Parsetree.core_type list
val xv_rev_reversed_separated_nonempty_llist_BAR_row_field__ :
  Parsetree.row_field list
val xv_rev_reversed_separated_nonempty_llist_AND_with_constraint__ :
  Parsetree.with_constraint list
val xv_rev_reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr__ :
  Parsetree.core_type list
val xv_rev_reversed_preceded_or_separated_nonempty_llist_BAR_match_case__ :
  Parsetree.case list
val xv_rev_reversed_nonempty_llist_typevar__ : Ast_helper.str_vala list
val xv_rev_reversed_nonempty_llist_name_tag_vala__ :
  Asttypes.label Ast_helper.vala list
val xv_rev_reversed_nonempty_llist_labeled_simple_expr__ :
  (Asttypes.arg_label * Parsetree.expression) list
val xv_rev_reversed_llist_preceded_CONSTRAINT_constrain___ :
  (Parsetree.core_type * Parsetree.core_type * Ast_helper.loc) list
val xv_rev_inline_reversed_separated_nonempty_llist_STAR_atomic_type__ :
  Parsetree.core_type list
val xv_record_pat_field :
  Longident.t Ast_helper.vala Asttypes.loc * Parsetree.pattern
val xv_record_pat_content :
  (Longident.t Ast_helper.vala Asttypes.loc * Parsetree.pattern) list
  Ast_helper.vala * Asttypes.closed_flag Ast_helper.vala
val xv_record_expr_field :
  Longident.t Ast_helper.vala Asttypes.loc * Parsetree.expression
val xv_record_expr_content :
  Parsetree.expression option Ast_helper.vala *
  (Longident.t Ast_helper.vala Asttypes.loc * Parsetree.expression) list
  Ast_helper.vala
val xv_rec_module_declarations :
  string Ast_helper.vala Asttypes.loc option *
  Parsetree.module_declaration list
val xv_rec_module_declaration :
  string Ast_helper.vala Asttypes.loc option * Parsetree.module_declaration
val xv_rec_module_bindings :
  string Ast_helper.vala Asttypes.loc option * Parsetree.module_binding list
val xv_rec_module_binding :
  string Ast_helper.vala Asttypes.loc option * Parsetree.module_binding
val xv_rec_flag : Asttypes.rec_flag
val xv_raw_string : string
val xv_qualified_dotop : Longident.t Ast_helper.vala option * string
val xv_private_virtual_flags : Asttypes.private_flag * Asttypes.virtual_flag
val xv_private_flag : Asttypes.private_flag
val xv_primitive_declaration :
  Parsetree.value_description * string Ast_helper.vala Asttypes.loc option
val xv_preceded_or_separated_nonempty_llist_BAR_match_case_ :
  Parsetree.case list
val xv_preceded_SEMISEMI_optional_use_file_standalone_expression_ :
  Parsetree.toplevel_phrase list
val xv_preceded_EQUAL_seq_expr_ : Parsetree.expression
val xv_preceded_EQUAL_pattern_ : Parsetree.pattern
val xv_preceded_EQUAL_module_type_ : Parsetree.module_type
val xv_preceded_EQUAL_expr_ : Parsetree.expression
val xv_preceded_CONSTRAINT_constrain_ :
  Parsetree.core_type * Parsetree.core_type * Ast_helper.loc
val xv_preceded_COLON_core_type_ : Parsetree.core_type
val xv_preceded_AS_mkrhs_LIDENT__ : Ast_helper.str
val xv_post_item_attributes : Parsetree.attributes
val xv_post_item_attribute : Parsetree.attribute
val xv_possibly_poly_core_type_no_attr_ : Parsetree.core_type
val xv_possibly_poly_core_type_ : Parsetree.core_type
val xv_poly_type_no_attr : Parsetree.core_type
val xv_poly_type : Parsetree.core_type
val xv_poly_core_type_no_attr_ : Parsetree.core_type_desc
val xv_poly_core_type_ : Parsetree.core_type_desc
val xv_payload : Parsetree.payload
val xv_pattern_var : Parsetree.pattern
val xv_pattern_semi_list : Parsetree.pattern list
val xv_pattern_no_exn : Parsetree.pattern
val xv_pattern_gen : Parsetree.pattern
val xv_pattern_comma_list_pattern_no_exn_ : Parsetree.pattern list
val xv_pattern_comma_list_pattern_ : Parsetree.pattern list
val xv_pattern__pattern_no_exn_ : Parsetree.pattern
val xv_pattern__pattern_ : Parsetree.pattern
val xv_pattern : Parsetree.pattern
val xv_parse_value_binding : Parsetree.value_binding
val xv_parse_val_longident : Longident.t
val xv_parse_type_substitution : Parsetree.type_declaration
val xv_parse_type_declaration : Parsetree.type_declaration
val xv_parse_structure_item : Parsetree.structure_item
val xv_parse_pattern : Parsetree.pattern
val xv_parse_mty_longident : Longident.t
val xv_parse_module_type : Parsetree.module_type
val xv_parse_module_expr : Parsetree.module_expr
val xv_parse_mod_longident : Longident.t
val xv_parse_mod_ext_longident : Longident.t
val xv_parse_match_case : Parsetree.case
val xv_parse_lident_vala_loc : string Ast_helper.vala Location.loc
val xv_parse_label_declaration : Parsetree.label_declaration
val xv_parse_extension_constructor : Parsetree.extension_constructor
val xv_parse_expression : Parsetree.expression
val xv_parse_core_type : Parsetree.core_type
val xv_parse_constructor_declaration : Parsetree.constructor_declaration
val xv_parse_constr_longident : Longident.t
val xv_parse_constant : Parsetree.constant
val xv_parse_binding_op : Parsetree.binding_op
val xv_parse_attribute : Parsetree.attribute
val xv_parse_arg_label : Asttypes.arg_label
val xv_parse_any_longident : Longident.t
val xv_paren_module_expr : Parsetree.module_expr
val xv_package_type : Parsetree.core_type
val xv_override_flag_vala : Asttypes.override_flag Ast_helper.vala
val xv_override_flag : Asttypes.override_flag
val xv_optlabel : string Ast_helper.vala
val xv_optional_use_file_standalone_expression :
  Parsetree.toplevel_phrase list
val xv_optional_structure_standalone_expression :
  Parsetree.structure_item list
val xv_option_type_constraint_ :
  (Parsetree.core_type option * Parsetree.core_type option) option
val xv_option_preceded_EQUAL_seq_expr__ : Parsetree.expression option
val xv_option_preceded_EQUAL_pattern__ : Parsetree.pattern option
val xv_option_preceded_EQUAL_module_type__ : Parsetree.module_type option
val xv_option_preceded_EQUAL_expr__ : Parsetree.expression option
val xv_option_preceded_COLON_core_type__ : Parsetree.core_type option
val xv_option_preceded_AS_mkrhs_LIDENT___ : string Asttypes.loc option
val xv_option_UNDERSCORE_ : unit option
val xv_option_SEMI_ : unit option
val xv_option_BAR_ : unit option
val xv_opt_default : Parsetree.expression option
val xv_opt_ampersand : bool
val xv_operator : string
val xv_open_dot_declaration : Parsetree.open_declaration
val xv_open_description :
  Parsetree.open_description * string Ast_helper.vala Asttypes.loc option
val xv_open_declaration :
  Parsetree.open_declaration * string Ast_helper.vala Asttypes.loc option
val xv_op_infix_operator_ : Parsetree.expression
val xv_op___anonymous_26_ : Parsetree.expression
val xv_op_PREFIXOP_ : Parsetree.expression
val xv_op_HASHOP_ : Parsetree.expression
val xv_object_expr_field :
  Asttypes.label Ast_helper.vala Asttypes.loc * Parsetree.expression
val xv_object_expr_content :
  (Asttypes.label Ast_helper.vala Asttypes.loc * Parsetree.expression) list
  Ast_helper.vala
val xv_nonrec_flag : Asttypes.rec_flag
val xv_nonempty_type_kind :
  Parsetree.type_kind * Asttypes.private_flag Ast_helper.vala *
  Parsetree.core_type Ast_helper.vala option
val xv_nonempty_llist_typevar_ : Ast_helper.str_vala list
val xv_nonempty_llist_name_tag_vala_ : Asttypes.label Ast_helper.vala list
val xv_nonempty_llist_labeled_simple_expr_ :
  (Asttypes.arg_label * Parsetree.expression) list
val xv_nonempty_list_raw_string_ : string list
val xv_nonempty_list_mkrhs_vala_LIDENT_ANTI_LID___ :
  string Ploc.vala Asttypes.loc list
val xv_no_override_flag : Asttypes.override_flag
val xv_no_nonrec_flag : Asttypes.rec_flag
val xv_no_ext : string Ploc.vala Asttypes.loc option
val xv_name_tag_vala_list : Asttypes.label Ast_helper.vala list
val xv_name_tag_vala : Asttypes.label Ast_helper.vala
val xv_name_tag : Asttypes.label
val xv_mutable_virtual_flags : Asttypes.mutable_flag * Asttypes.virtual_flag
val xv_mutable_flag : Asttypes.mutable_flag
val xv_mty_longident : Longident.t
val xv_module_type_subst :
  Parsetree.module_type_declaration *
  string Ast_helper.vala Asttypes.loc option
val xv_module_type_declaration :
  Parsetree.module_type_declaration *
  string Ast_helper.vala Asttypes.loc option
val xv_module_type : Parsetree.module_type
val xv_module_subst :
  Parsetree.module_substitution * string Ast_helper.vala Asttypes.loc option
val xv_module_name_ : string Ast_helper.vala option
val xv_module_name : string Ast_helper.vala option Ast_helper.vala
val xv_module_expr_alias : Parsetree.module_type
val xv_module_expr : Parsetree.module_expr
val xv_module_declaration_body : Parsetree.module_type
val xv_module_declaration :
  Parsetree.module_declaration * string Ast_helper.vala Asttypes.loc option
val xv_module_binding_body : Parsetree.module_expr
val xv_module_binding :
  Parsetree.structure_item_desc * string Ast_helper.vala Asttypes.loc option
val xv_module_alias :
  Parsetree.module_declaration * string Ast_helper.vala Asttypes.loc option
val xv_mod_longident : Longident.t
val xv_mod_ext_longident : Longident.t
val xv_mktyp_poly_core_type_no_attr__ : Parsetree.core_type
val xv_mktyp_poly_core_type__ : Parsetree.core_type
val xv_mktyp___anonymous_40_ : Parsetree.core_type
val xv_mktyp___anonymous_39_ : Parsetree.core_type
val xv_mktyp___anonymous_38_ : Parsetree.core_type
val xv_mktyp___anonymous_37_ : Parsetree.core_type
val xv_mktyp___anonymous_36_ : Parsetree.core_type
val xv_mktyp___anonymous_16_ : Parsetree.core_type
val xv_mkstr___anonymous_1_ : Parsetree.structure_item
val xv_mksig___anonymous_5_ : Parsetree.signature_item
val xv_mkrhs_vaval_label_longident__ :
  Longident.t Ast_helper.vala Asttypes.loc
val xv_mkrhs_vaval_class_longident__ :
  Longident.t Ast_helper.vala Asttypes.loc
val xv_mkrhs_vaval_LETOP__ : string Ast_helper.vala Asttypes.loc
val xv_mkrhs_vaval_ANDOP__ : string Ast_helper.vala Asttypes.loc
val xv_mkrhs_vala_val_ident_ANTI_LID__ : string Ast_helper.vala Asttypes.loc
val xv_mkrhs_vala_label_longident_ANTI_LONGID__ :
  Longident.t Ast_helper.vala Asttypes.loc
val xv_mkrhs_vala_label_ANTI_LID__ :
  Asttypes.label Ast_helper.vala Asttypes.loc
val xv_mkrhs_vala_ident_ANTI_LID__ :
  Asttypes.label Ast_helper.vala Asttypes.loc
val xv_mkrhs_vala_constr_longident_ANTI_LONGID__ :
  Longident.t Ast_helper.vala Asttypes.loc
val xv_mkrhs_vala_constr_ident_ANTI_UID__ : Ast_helper.str_vala
val xv_mkrhs_vala_LIDENT_ANTI_LID__ : Ast_helper.str_vala
val xv_mkrhs_val_longident_ : Longident.t Asttypes.loc
val xv_mkrhs_val_ident_ : Ast_helper.str
val xv_mkrhs_type_longident_ : Ast_helper.lid
val xv_mkrhs_name_tag_ : Asttypes.label Ast_helper.with_loc
val xv_mkrhs_mty_longident_ : Longident.t Asttypes.loc
val xv_mkrhs_module_name_ :
  string Ast_helper.vala option Ast_helper.vala Asttypes.loc
val xv_mkrhs_mod_longident_ : Longident.t Asttypes.loc
val xv_mkrhs_mod_ext_longident_ : Longident.t Asttypes.loc
val xv_mkrhs_label_longident_ : Longident.t Asttypes.loc
val xv_mkrhs_label_ : Asttypes.label Ast_helper.with_loc
val xv_mkrhs_ident_ : Asttypes.label Asttypes.loc
val xv_mkrhs_constr_longident_ : Ast_helper.lid
val xv_mkrhs_clty_longident_ : Longident.t Asttypes.loc
val xv_mkrhs_class_longident_ : Longident.t Asttypes.loc
val xv_mkrhs___anonymous_34_ : Longident.t Ast_helper.vala Asttypes.loc
val xv_mkrhs___anonymous_33_ : Longident.t Ast_helper.vala Asttypes.loc
val xv_mkrhs___anonymous_28_ : Longident.t Ast_helper.vala Asttypes.loc
val xv_mkrhs___anonymous_27_ : Longident.t Ast_helper.vala Asttypes.loc
val xv_mkrhs_UIDENT_ : Ast_helper.str
val xv_mkrhs_LIDENT_ : Ast_helper.str
val xv_mkpat_simple_pattern_not_ident__ : Parsetree.pattern
val xv_mkpat___anonymous_35_ : Parsetree.pattern
val xv_mkpat___anonymous_32_ : Parsetree.pattern
val xv_mkpat___anonymous_31_ : Parsetree.pattern
val xv_mkpat___anonymous_30_pattern_no_exn__ : Parsetree.pattern
val xv_mkpat___anonymous_30_pattern__ : Parsetree.pattern
val xv_mkpat___anonymous_20_ : Parsetree.pattern
val xv_mkpat___anonymous_19_ : Parsetree.pattern
val xv_mkpat___anonymous_12_ : Parsetree.pattern
val xv_mkmty___anonymous_7_ : Parsetree.module_type
val xv_mkmty___anonymous_4_ : Parsetree.module_type
val xv_mkmod___anonymous_3_ : Parsetree.module_expr
val xv_mkmod___anonymous_0_ : Parsetree.module_expr
val xv_mkloc_vala_LIDENT_ANTI_LID__ : string Ast_helper.vala Location.loc
val xv_mkloc___anonymous_42_ : string Ast_helper.vala Asttypes.loc
val xv_mkexp_simple_expr__ : Parsetree.expression
val xv_mkexp_expr__ : Parsetree.expression
val xv_mkexp___anonymous_29_ : Parsetree.expression
val xv_mkexp___anonymous_18_ : Parsetree.expression
val xv_mkcty___anonymous_15_ : Parsetree.class_type
val xv_mkcty___anonymous_14_ : Parsetree.class_type
val xv_mkctf___anonymous_17_ : Parsetree.class_type_field
val xv_mkclass___anonymous_9_ : Parsetree.class_expr
val xv_mkclass___anonymous_8_ : Parsetree.class_expr
val xv_mkclass___anonymous_11_ : Parsetree.class_expr
val xv_mkclass___anonymous_10_ : Parsetree.class_expr
val xv_mkcf___anonymous_13_ : Parsetree.class_field
val xv_mk_longident_vala_mod_longident_ANTI_LONGID__vala_UIDENT_ANTI_UID__ :
  Longident.t
val xv_mk_longident_vala_mod_longident_ANTI_LONGID__vala_LIDENT_ANTI_LID__ :
  Longident.t
val xv_mk_longident_vala_mod_longident_ANTI_LONGID__val_ident_vala_ :
  Longident.t
val xv_mk_longident_vala_mod_ext_longident_ANTI_LONGID__vaval_ident__ :
  Longident.t
val xv_mk_longident_vala_mod_ext_longident_ANTI_LONGID__vaval_LIDENT__ :
  Longident.t
val xv_mk_longident_vala_mod_ext_longident_ANTI_LONGID__vala_UIDENT_ANTI_UID__ :
  Longident.t
val xv_mk_longident_vala_mod_ext_longident_ANTI_LONGID__vala_LIDENT_ANTI_LID__ :
  Longident.t
val xv_mk_longident_vala_mod_ext_longident_ANTI_LONGID____anonymous_41_ :
  Longident.t
val xv_mk_directive_arg_toplevel_directive_argument_ :
  Parsetree.directive_argument
val xv_method_ :
  (Asttypes.label Asttypes.loc * Asttypes.private_flag *
   Parsetree.class_field_kind) *
  Parsetree.attributes
val xv_meth_list : Parsetree.object_field list * Asttypes.closed_flag
val xv_match_cases : Parsetree.case list
val xv_match_case : Parsetree.case
val xv_mark_rhs_docs_toplevel_directive_ : Parsetree.toplevel_phrase
val xv_mark_rhs_docs_text_str_str_exp__ : Parsetree.structure_item list
val xv_llist_preceded_CONSTRAINT_constrain__ :
  (Parsetree.core_type * Parsetree.core_type * Ast_helper.loc) list
val xv_listx_SEMI_record_pat_field_UNDERSCORE_ :
  (Longident.t Ast_helper.vala Asttypes.loc * Parsetree.pattern) list *
  unit option
val xv_list_use_file_element_ : Parsetree.toplevel_phrase list list
val xv_list_text_str_structure_item__ : Parsetree.structure_item list list
val xv_list_text_cstr_class_field__ : Parsetree.class_field list list
val xv_list_text_csig_class_sig_field__ :
  Parsetree.class_type_field list list
val xv_list_structure_element_ : Parsetree.structure_item list list
val xv_list_signature_element_ : Parsetree.signature_item list list
val xv_list_post_item_attribute_ : Parsetree.attributes
val xv_list_generic_and_type_declaration_type_subst_kind__ :
  Parsetree.type_declaration list
val xv_list_generic_and_type_declaration_type_kind__ :
  Parsetree.type_declaration list
val xv_list_attribute_ : Parsetree.attributes
val xv_list_and_module_declaration_ : Parsetree.module_declaration list
val xv_list_and_module_binding_ : Parsetree.module_binding list
val xv_list_and_class_type_declaration_ :
  Parsetree.class_type_declaration list
val xv_list_and_class_description_ : Parsetree.class_description list
val xv_list_and_class_declaration_ : Parsetree.class_declaration list
val xv_lident_list : string Ploc.vala Asttypes.loc list
val xv_letop_bindings :
  Parsetree.pattern * Parsetree.expression * Parsetree.binding_op list
val xv_letop_binding_body : Parsetree.pattern * Parsetree.expression
val xv_let_pattern : Parsetree.pattern
val xv_let_ident : Parsetree.pattern
val xv_let_exception_declaration : Parsetree.extension_constructor
val xv_let_bindings_no_ext_ : let_bindings
val xv_let_bindings_ext_ : let_bindings
val xv_let_binding_body_no_punning : Parsetree.pattern * Parsetree.expression
val xv_let_binding_body : Parsetree.pattern * Parsetree.expression * bool
val xv_let_binding_no_ext_ : let_bindings
val xv_let_binding_ext_ : let_bindings
val xv_labeled_simple_pattern :
  Asttypes.arg_label Ast_helper.vala *
  Parsetree.expression option Ast_helper.vala * Parsetree.pattern
val xv_labeled_simple_expr : Asttypes.arg_label * Parsetree.expression
val xv_label_var : string * Parsetree.pattern
val xv_label_longident : Longident.t
val xv_label_let_pattern : string * Parsetree.pattern
val xv_label_declarations : Parsetree.label_declaration list
val xv_label_declaration_semi : Parsetree.label_declaration
val xv_label_declaration : Parsetree.label_declaration
val xv_label : Asttypes.label
val xv_item_extension : Parsetree.extension
val xv_ioption_terminated_vala_core_type_ANTI_TYP__EQUAL__ :
  Parsetree.core_type Ast_helper.vala option
val xv_ioption_terminated_simple_expr_WITH__ : Parsetree.expression option
val xv_ioption_mk_directive_arg_toplevel_directive_argument__ :
  Parsetree.directive_argument option
val xv_ioption___anonymous_21_ : Longident.t Ast_helper.vala option
val xv_ioption_SEMI_ : unit option
val xv_ioption_BAR_ : unit option
val xv_interface : Parsetree.signature_item list Ast_helper.vala
val xv_inline_separated_nonempty_llist_STAR_atomic_type_ :
  Parsetree.core_type list
val xv_inline_reversed_separated_nonempty_llist_STAR_atomic_type_ :
  Parsetree.core_type list
val xv_inline_reversed_separated_nonempty_llist_COMMA_type_parameter_ :
  (Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) list
val xv_inline_reversed_separated_nonempty_llist_COMMA_core_type_ :
  Parsetree.core_type list
val xv_inline_reversed_separated_nonempty_llist_BAR_row_field_ :
  Parsetree.row_field list
val xv_inline_reversed_separated_nonempty_llist_AND_with_constraint_ :
  Parsetree.with_constraint list
val xv_inline_reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ :
  Parsetree.core_type list
val xv_inline_private_flag : Asttypes.private_flag
val xv_inherit_field : Parsetree.object_field
val xv_infix_operator : string
val xv_indexop_expr_4_qualified_dotop_expr_semi_list___anonymous_25_ :
  Parsetree.expression * (Longident.t Ast_helper.vala option * string) *
  paren_kind * Parsetree.expression list * Parsetree.expression option
val xv_indexop_expr_3_DOT_seq_expr___anonymous_24_ :
  Parsetree.expression * unit * paren_kind * Parsetree.expression *
  Parsetree.expression option
val xv_indexop_expr_2_qualified_dotop_expr_semi_list___anonymous_23_ :
  Parsetree.expression * (Longident.t Ast_helper.vala option * string) *
  paren_kind * Parsetree.expression list * Parsetree.expression option
val xv_indexop_expr_1_DOT_seq_expr___anonymous_22_ :
  Parsetree.expression * unit * paren_kind * Parsetree.expression *
  Parsetree.expression option
val xv_indexop_error_qualified_dotop_expr_semi_list_ : Parsetree.expression
val xv_indexop_error_DOT_seq_expr_ : Parsetree.expression
val xv_index_mod : string
val xv_include_statement_module_type_ :
  Parsetree.include_description * string Ast_helper.vala Asttypes.loc option
val xv_include_statement_module_expr_ :
  Parsetree.include_declaration * string Ast_helper.vala Asttypes.loc option
val xv_implementation : Parsetree.structure_item list Ast_helper.vala
val xv_iloption_text_def_top_def_str_exp___ : Parsetree.toplevel_phrase list
val xv_iloption_mark_rhs_docs_text_str_str_exp___ :
  Parsetree.structure_item list
val xv_ident_vala : string Ast_helper.vala
val xv_ident : Asttypes.label
val xv_generic_type_declarations_vala_nonrec_flag_ANTI_NONRECFLAG__type_kind_ :
  (Asttypes.rec_flag Ast_helper.vala *
   string Ast_helper.vala Asttypes.loc option) *
  Parsetree.type_declaration list Ast_helper.vala
val xv_generic_type_declarations_no_nonrec_flag_type_subst_kind_ :
  (Asttypes.rec_flag * string Ast_helper.vala Asttypes.loc option) *
  Parsetree.type_declaration list Ast_helper.vala
val xv_generic_type_declaration_vala_nonrec_flag_ANTI_NONRECFLAG__type_kind_ :
  (Asttypes.rec_flag Ast_helper.vala *
   string Ast_helper.vala Asttypes.loc option) *
  Parsetree.type_declaration
val xv_generic_type_declaration_no_nonrec_flag_type_subst_kind_ :
  (Asttypes.rec_flag * string Ast_helper.vala Asttypes.loc option) *
  Parsetree.type_declaration
val xv_generic_constructor_declaration_epsilon_ :
  Ast_helper.str_vala * Ast_helper.str_vala list *
  Parsetree.constructor_arguments * Parsetree.core_type option *
  Parsetree.attributes Ast_helper.vala * Location.t * Docstrings.info
val xv_generic_constructor_declaration_BAR_ :
  Ast_helper.str_vala * Ast_helper.str_vala list *
  Parsetree.constructor_arguments * Parsetree.core_type option *
  Parsetree.attributes Ast_helper.vala * Location.t * Docstrings.info
val xv_generic_and_type_declaration_type_subst_kind_ :
  Parsetree.type_declaration
val xv_generic_and_type_declaration_type_kind_ : Parsetree.type_declaration
val xv_generalized_constructor_arguments :
  Ast_helper.str_vala list * Parsetree.constructor_arguments *
  Parsetree.core_type option
val xv_functor_args : (Lexing.position * Parsetree.functor_parameter) list
val xv_functor_arg : Lexing.position * Parsetree.functor_parameter
val xv_function_type : Parsetree.core_type
val xv_fun_def : Parsetree.expression
val xv_fun_binding : Parsetree.expression
val xv_formal_class_parameters :
  (Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) list
val xv_floating_attribute : Parsetree.attribute
val xv_flatten_list_use_file_element__ : Parsetree.toplevel_phrase list
val xv_flatten_list_text_str_structure_item___ :
  Parsetree.structure_item list
val xv_flatten_list_text_cstr_class_field___ : Parsetree.class_field list
val xv_flatten_list_text_csig_class_sig_field___ :
  Parsetree.class_type_field list
val xv_flatten_list_structure_element__ : Parsetree.structure_item list
val xv_flatten_list_signature_element__ : Parsetree.signature_item list
val xv_field_semi : Parsetree.object_field
val xv_field : Parsetree.object_field
val xv_extra_str_text_str_str_exp__ : Parsetree.structure
val xv_extra_str_flatten_list_text_str_structure_item____ :
  Parsetree.structure
val xv_extra_str_append_optional_structure_standalone_expression_flatten_list_structure_element____ :
  Parsetree.structure_item list Ast_helper.vala
val xv_extra_sig_flatten_list_signature_element___ :
  Parsetree.signature_item list Ast_helper.vala
val xv_extra_rhs_tuple_type_ : Parsetree.core_type
val xv_extra_def_append_optional_use_file_standalone_expression_flatten_list_use_file_element____ :
  Parsetree.toplevel_phrase list
val xv_extra_cstr_class_fields_ : Parsetree.class_field list
val xv_extra_csig_class_sig_fields_ : Parsetree.class_type_field list
val xv_extension_constructor_rebind_epsilon_ :
  Parsetree.extension_constructor
val xv_extension_constructor_rebind_BAR_ : Parsetree.extension_constructor
val xv_extension_constructor_declaration_epsilon_ :
  Parsetree.extension_constructor
val xv_extension_constructor_declaration_BAR_ :
  Parsetree.extension_constructor
val xv_extension_constructor_epsilon_ : Parsetree.extension_constructor
val xv_extension_constructor_BAR_ : Parsetree.extension_constructor
val xv_extension : Parsetree.extension
val xv_ext_attributes :
  string Ast_helper.vala Asttypes.loc option * Parsetree.attributes
val xv_ext : string Ast_helper.vala Asttypes.loc option
val xv_expr_semi_list : Parsetree.expression list
val xv_expr_comma_list : Parsetree.expression list
val xv_expr_colon_package_type : Parsetree.expression
val xv_expr_attrs :
  Parsetree.expression_desc *
  (string Ast_helper.vala Asttypes.loc option * Parsetree.attributes)
val xv_expr_ : Parsetree.expression_desc
val xv_expr : Parsetree.expression
val xv_epsilon : unit
val xv_direction_flag : Asttypes.direction_flag
val xv_core_type_no_attr : Parsetree.core_type
val xv_core_type_declaration_type_subst_kind_ : Parsetree.type_declaration
val xv_core_type_declaration_type_kind_ : Parsetree.type_declaration
val xv_core_type : Parsetree.core_type
val xv_constructor_declarations : Parsetree.constructor_declaration list
val xv_constructor_declaration_epsilon_ : Parsetree.constructor_declaration
val xv_constructor_declaration_BAR_ : Parsetree.constructor_declaration
val xv_constructor_arguments : Parsetree.constructor_arguments
val xv_constraints :
  (Parsetree.core_type * Parsetree.core_type * Ast_helper.loc) list
val xv_constrain_field : Parsetree.core_type * Parsetree.core_type
val xv_constrain : Parsetree.core_type * Parsetree.core_type * Ast_helper.loc
val xv_constr_longident : Longident.t
val xv_constr_ident : string
val xv_constr_extra_nonprefix_ident : string
val xv_constr_extra_ident : string
val xv_constant : Parsetree.constant
val xv_clty_longident : Longident.t
val xv_class_type_declarations :
  string Ast_helper.vala Asttypes.loc option *
  Parsetree.class_type_declaration list
val xv_class_type_declaration :
  string Ast_helper.vala Asttypes.loc option *
  Parsetree.class_type_declaration
val xv_class_type : Parsetree.class_type
val xv_class_structure : Parsetree.class_structure
val xv_class_simple_expr : Parsetree.class_expr
val xv_class_signature : Parsetree.class_type
val xv_class_sig_fields : Parsetree.class_type_field list
val xv_class_sig_field : Parsetree.class_type_field
val xv_class_sig_body : Parsetree.class_signature
val xv_class_self_type : Parsetree.core_type
val xv_class_self_pattern : Parsetree.pattern
val xv_class_parameters_type_parameter_ :
  (Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)) list
val xv_class_parameters_core_type_ : Parsetree.core_type list
val xv_class_longident : Longident.t
val xv_class_fun_def : Parsetree.class_expr
val xv_class_fun_binding : Parsetree.class_expr
val xv_class_fields : Parsetree.class_field list
val xv_class_field : Parsetree.class_field
val xv_class_expr : Parsetree.class_expr
val xv_class_descriptions :
  string Ast_helper.vala Asttypes.loc option *
  Parsetree.class_description list
val xv_class_description :
  string Ast_helper.vala Asttypes.loc option * Parsetree.class_description
val xv_class_declarations :
  string Ast_helper.vala Asttypes.loc option *
  Parsetree.class_declaration list
val xv_class_declaration :
  string Ast_helper.vala Asttypes.loc option * Parsetree.class_declaration
val xv_bar_llist_extension_constructor_declaration_ :
  Parsetree.extension_constructor list
val xv_bar_llist_extension_constructor_ :
  Parsetree.extension_constructor list
val xv_bar_llist_constructor_declaration_ :
  Parsetree.constructor_declaration list
val xv_attributes : Parsetree.attributes
val xv_attribute : Parsetree.attribute
val xv_attr_id : string Ast_helper.vala Asttypes.loc
val xv_atomic_type : Parsetree.core_type
val xv_arg_label : Asttypes.arg_label
val xv_append_text_str_SEMISEMI_optional_structure_standalone_expression_ :
  Parsetree.structure_item list
val xv_append_optional_use_file_standalone_expression_flatten_list_use_file_element___ :
  Parsetree.toplevel_phrase list
val xv_append_optional_structure_standalone_expression_flatten_list_structure_element___ :
  Parsetree.structure_item list
val xv_any_longident : Longident.t
val xv_and_module_declaration : Parsetree.module_declaration
val xv_and_module_binding : Parsetree.module_binding
val xv_and_let_binding : let_binding
val xv_and_class_type_declaration : Parsetree.class_type_declaration
val xv_and_class_description : Parsetree.class_description
val xv_and_class_declaration : Parsetree.class_declaration
val xv_amper_type_list : Parsetree.core_type list
val xv_alias_type : Parsetree.core_type
val xv_additive : string
val xv_actual_type_parameters : Parsetree.core_type list
val xv_actual_class_parameters : Parsetree.core_type list
val xv___anonymous_9 : Parsetree.class_expr_desc
val xv___anonymous_8 : Parsetree.class_expr_desc
val xv___anonymous_7 : Parsetree.module_type_desc
val xv___anonymous_6 :
  Parsetree.signature_item_desc * string Ast_helper.vala Asttypes.loc option
val xv___anonymous_5 : Parsetree.signature_item_desc
val xv___anonymous_42 : string Ast_helper.vala
val xv___anonymous_41 : string Ast_helper.vala
val xv___anonymous_40 : Parsetree.core_type_desc
val xv___anonymous_4 : Parsetree.module_type_desc
val xv___anonymous_39 : Parsetree.core_type_desc
val xv___anonymous_38 : Parsetree.core_type_desc
val xv___anonymous_37 : Parsetree.core_type_desc
val xv___anonymous_36 : Parsetree.core_type_desc
val xv___anonymous_35 : Parsetree.pattern_desc
val xv___anonymous_34 : Longident.t Ast_helper.vala
val xv___anonymous_33 : Longident.t Ast_helper.vala
val xv___anonymous_32 : Parsetree.pattern_desc
val xv___anonymous_31 : Parsetree.pattern_desc
val xv___anonymous_30_pattern_no_exn_ : Parsetree.pattern_desc
val xv___anonymous_30_pattern_ : Parsetree.pattern_desc
val xv___anonymous_3 : Parsetree.module_expr_desc
val xv___anonymous_29 : Parsetree.expression_desc
val xv___anonymous_28 : Longident.t Ast_helper.vala
val xv___anonymous_27 : Longident.t Ast_helper.vala
val xv___anonymous_26 : string
val xv___anonymous_25 : Parsetree.expression option
val xv___anonymous_24 : Parsetree.expression option
val xv___anonymous_23 : Parsetree.expression option
val xv___anonymous_22 : Parsetree.expression option
val xv___anonymous_21 : Longident.t Ast_helper.vala
val xv___anonymous_20 : Parsetree.pattern_desc
val xv___anonymous_2 :
  Parsetree.structure_item_desc * string Ast_helper.vala Asttypes.loc option
val xv___anonymous_19 : Parsetree.pattern_desc
val xv___anonymous_18 : Parsetree.expression_desc
val xv___anonymous_17 : Parsetree.class_type_field_desc
val xv___anonymous_16 : Parsetree.core_type_desc
val xv___anonymous_15 : Parsetree.class_type_desc
val xv___anonymous_14 : Parsetree.class_type_desc
val xv___anonymous_13 : Parsetree.class_field_desc
val xv___anonymous_12 : Parsetree.pattern_desc
val xv___anonymous_11 : Parsetree.class_expr_desc
val xv___anonymous_10 : Parsetree.class_expr_desc
val xv___anonymous_1 : Parsetree.structure_item_desc
val xv___anonymous_0 : Parsetree.module_expr_desc
val menhir_end_marker : int
