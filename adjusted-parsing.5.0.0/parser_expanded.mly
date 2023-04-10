%{

open Asttypes
open Longident
open Parsetree
open Ast_helper
open Docstrings
open Docstrings.WithMenhir

let mkloc = Location.mkloc
let mknoloc = Location.mknoloc

let make_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

let ghost_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = true;
}

let mktyp ~loc ?attrs d = Typ.mk ~loc:(make_loc loc) ?attrs d
let mkpat ~loc d = Pat.mk ~loc:(make_loc loc) d
let mkexp ~loc d = Exp.mk ~loc:(make_loc loc) d
let mkmty ~loc ?attrs d = Mty.mk ~loc:(make_loc loc) ?attrs d
let mksig ~loc d = Sig.mk ~loc:(make_loc loc) d
let mkmod ~loc ?attrs d = Mod.mk ~loc:(make_loc loc) ?attrs d
let mkstr ~loc d = Str.mk ~loc:(make_loc loc) d
let mkclass ~loc ?attrs d = Cl.mk ~loc:(make_loc loc) ?attrs d
let mkcty ~loc ?attrs d = Cty.mk ~loc:(make_loc loc) ?attrs d

let pstr_typext (te, ext) =
  (Pstr_typext te, ext)
let pstr_primitive (vd, ext) =
  (Pstr_primitive vd, ext)
let pstr_type ((nr, ext), tys) =
  (Pstr_type (nr, tys), ext)
let pstr_exception (te, ext) =
  (Pstr_exception te, ext)
let pstr_include (body, ext) =
  (Pstr_include body, ext)
let pstr_recmodule (ext, bindings) =
  (Pstr_recmodule bindings, ext)

let psig_typext (te, ext) =
  (Psig_typext te, ext)
let psig_value (vd, ext) =
  (Psig_value vd, ext)
let psig_type ((nr, ext), tys) =
  (Psig_type (nr, tys), ext)
let psig_typesubst ((nr, ext), tys) =
  assert (nr = Recursive); (* see [no_nonrec_flag] *)
  (Psig_typesubst tys, ext)
let psig_exception (te, ext) =
  (Psig_exception te, ext)
let psig_include (body, ext) =
  (Psig_include body, ext)

let mkctf ~loc ?attrs ?docs d =
  Ctf.mk ~loc:(make_loc loc) ?attrs ?docs d
let mkcf ~loc ?attrs ?docs d =
  Cf.mk ~loc:(make_loc loc) ?attrs ?docs d

let mkrhs rhs loc = mkloc rhs (make_loc loc)
let ghrhs rhs loc = mkloc rhs (ghost_loc loc)

let push_loc x acc =
  if x.Location.loc_ghost
  then acc
  else x :: acc

let reloc_pat ~loc x =
  { x with ppat_loc = make_loc loc;
           ppat_loc_stack = push_loc x.ppat_loc x.ppat_loc_stack }
let reloc_exp ~loc x =
  { x with pexp_loc = make_loc loc;
           pexp_loc_stack = push_loc x.pexp_loc x.pexp_loc_stack }
let reloc_typ ~loc x =
  { x with ptyp_loc = make_loc loc;
           ptyp_loc_stack = push_loc x.ptyp_loc x.ptyp_loc_stack }

let mkexpvar ~loc (name : string) =
  mkexp ~loc (Pexp_ident(mkrhs (Lident name) loc))

let mkoperator =
  mkexpvar

let mkpatvar ~loc name =
  mkpat ~loc (Ppat_var (mkrhs name loc))

(*
  Ghost expressions and patterns:
  expressions and patterns that do not appear explicitly in the
  source file they have the loc_ghost flag set to true.
  Then the profiler will not try to instrument them and the
  -annot option will not try to display their type.

  Every grammar rule that generates an element with a location must
  make at most one non-ghost element, the topmost one.

  How to tell whether your location must be ghost:
  A location corresponds to a range of characters in the source file.
  If the location contains a piece of code that is syntactically
  valid (according to the documentation), and corresponds to the
  AST node, then the location must be real; in all other cases,
  it must be ghost.
*)
let ghexp ~loc d = Exp.mk ~loc:(ghost_loc loc) d
let ghpat ~loc d = Pat.mk ~loc:(ghost_loc loc) d
let ghtyp ~loc d = Typ.mk ~loc:(ghost_loc loc) d
let ghloc ~loc d = { txt = d; loc = ghost_loc loc }
let ghstr ~loc d = Str.mk ~loc:(ghost_loc loc) d
let ghsig ~loc d = Sig.mk ~loc:(ghost_loc loc) d

let mkinfix arg1 op arg2 =
  Pexp_apply(op, ([Nolabel, arg1; Nolabel, arg2]))

let neg_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus ~oploc name arg =
  match name, arg.pexp_desc with
  | "-", Pexp_constant((Pconst_integer (n,m))) ->
      Pexp_constant((Pconst_integer((neg_string n),m)))
  | ("-" | "-."), Pexp_constant((Pconst_float (f, m))) ->
     Pexp_constant((Pconst_float((neg_string f), m)))
  | _ ->
      Pexp_apply(mkoperator ~loc:oploc (("~" ^ name)), [Nolabel, arg])

let mkuplus ~oploc name arg =
  let desc = arg.pexp_desc in
  match name, desc with
  | "+", Pexp_constant((Pconst_integer _))
  | ("+" | "+."), Pexp_constant((Pconst_float _)) -> desc
  | _ ->
      Pexp_apply(mkoperator ~loc:oploc (("~" ^ name)), [Nolabel, arg])

(* TODO define an abstraction boundary between locations-as-pairs
   and locations-as-Location.t; it should be clear when we move from
   one world to the other *)

let mkexp_cons_desc consloc args =
  Pexp_construct(mkrhs ((Lident ("::"))) consloc, (Some args))
let mkexp_cons ~loc consloc args =
  mkexp ~loc (mkexp_cons_desc consloc args)

let mkpat_cons_desc consloc args =
  Ppat_construct(mkrhs ((Lident ("::"))) consloc, (Some ([], args)))
let mkpat_cons ~loc consloc args =
  mkpat ~loc (mkpat_cons_desc consloc args)

let ghexp_cons_desc consloc args =
  Pexp_construct(ghrhs ((Lident ("::"))) consloc, (Some args))
let ghpat_cons_desc consloc args =
  Ppat_construct(ghrhs ((Lident ("::"))) consloc, (Some ([], args)))

let rec mktailexp nilloc = let open Location in function
    [] ->
      let nil = ghloc ~loc:nilloc ((Lident ("[]"))) in
      Pexp_construct (nil, None), nilloc
  | e1 :: el ->
      let exp_el, el_loc = mktailexp nilloc el in
      let loc = (e1.pexp_loc.loc_start, snd el_loc) in
      let arg = ghexp ~loc (Pexp_tuple ([e1; ghexp ~loc:el_loc exp_el])) in
      ghexp_cons_desc loc arg, loc

let rec mktailpat nilloc = let open Location in function
    [] ->
      let nil = ghloc ~loc:nilloc ((Lident ("[]"))) in
      Ppat_construct (nil, None), nilloc
  | p1 :: pl ->
      let pat_pl, el_loc = mktailpat nilloc pl in
      let loc = (p1.ppat_loc.loc_start, snd el_loc) in
      let arg = ghpat ~loc (Ppat_tuple ([p1; ghpat ~loc:el_loc pat_pl])) in
      ghpat_cons_desc loc arg, loc

let mkstrexp ~loc e attrs =
  { pstr_desc = Pstr_eval (e, attrs); pstr_loc = make_loc loc }

let mkexp_constraint ~loc e (t1, t2) =
  match t1, t2 with
  | Some t, None -> mkexp ~loc (Pexp_constraint(e, t))
  | _, Some t -> mkexp ~loc (Pexp_coerce(e, t1, t))
  | None, None -> assert false

let mkexp_opt_constraint ~loc e = function
  | None -> e
  | Some constraint_ -> mkexp_constraint ~loc e constraint_

let mkpat_opt_constraint ~loc p = function
  | None -> p
  | Some typ -> mkpat ~loc (Ppat_constraint(p, typ))

let syntax_error () =
  raise Syntaxerr.Escape_error

let unclosed opening_name opening_loc closing_name closing_loc =
  raise(Syntaxerr.Error(Syntaxerr.Unclosed(make_loc opening_loc, opening_name,
                                           make_loc closing_loc, closing_name)))

let expecting loc nonterm =
    raise Syntaxerr.(Error(Expecting(make_loc loc, nonterm)))

let removed_string_set loc =
  raise(Syntaxerr.Error(Syntaxerr.Removed_string_set(make_loc loc)))

(* Using the function [not_expecting] in a semantic action means that this
   syntactic form is recognized by the parser but is in fact incorrect. This
   idiom is used in a few places to produce ad hoc syntax error messages. *)

(* This idiom should be used as little as possible, because it confuses the
   analyses performed by Menhir. Because Menhir views the semantic action as
   opaque, it believes that this syntactic form is correct. This can lead
   [make generate-parse-errors] to produce sentences that cause an early
   (unexpected) syntax error and do not achieve the desired effect. This could
   also lead a completion system to propose completions which in fact are
   incorrect. In order to avoid these problems, the productions that use
   [not_expecting] should be marked with AVOID. *)

let not_expecting loc nonterm =
    raise Syntaxerr.(Error(Not_expecting(make_loc loc, nonterm)))

(* Helper functions for desugaring array indexing operators *)
type paren_kind = Paren | Brace | Bracket

(* We classify the dimension of indices: Bigarray distinguishes
   indices of dimension 1,2,3, or more. Similarly, user-defined
   indexing operator behave differently for indices of dimension 1
   or more.
*)
type index_dim =
  | One
  | Two
  | Three
  | Many
type ('dot,'index) array_family = {

  name:
    Lexing.position * Lexing.position -> 'dot -> assign:bool -> paren_kind
  -> index_dim -> Longident.t Location.loc
  (*
    This functions computes the name of the explicit indexing operator
    associated with a sugared array indexing expression.

    For instance, for builtin arrays, if Clflags.unsafe is set,
    * [ a.[index] ]     =>  [String.unsafe_get]
    * [ a.{x,y} <- 1 ]  =>  [ Bigarray.Array2.unsafe_set]

    User-defined indexing operator follows a more local convention:
    * [ a .%(index)]     => [ (.%()) ]
    * [ a.![1;2] <- 0 ]  => [(.![;..]<-)]
    * [ a.My.Map.?(0) => [My.Map.(.?())]
  *);

  index:
    Lexing.position * Lexing.position -> paren_kind -> 'index
    -> index_dim * (arg_label * expression) list
   (*
     [index (start,stop) paren index] computes the dimension of the
     index argument and how it should be desugared when transformed
     to a list of arguments for the indexing operator.
     In particular, in both the Bigarray case and the user-defined case,
     beyond a certain dimension, multiple indices are packed into a single
     array argument:
     * [ a.(x) ]       => [ [One, [Nolabel, <<x>>] ]
     * [ a.{1,2} ]     => [ [Two, [Nolabel, <<1>>; Nolabel, <<2>>] ]
     * [ a.{1,2,3,4} ] => [ [Many, [Nolabel, <<[|1;2;3;4|]>>] ] ]
   *);

}

let bigarray_untuplify = function
    { pexp_desc = Pexp_tuple explist; pexp_loc = _ } -> explist
  | exp -> [exp]

let builtin_arraylike_name loc _ ~assign paren_kind n =
  let opname = if assign then "set" else "get" in
  let opname = if !Clflags.unsafe then "unsafe_" ^ opname else opname in
  let prefix = match paren_kind with
    | Paren -> Lident ("Array")
    | Bracket ->
        if assign then removed_string_set loc
        else Lident ("String")
    | Brace ->
       let submodule_name = match n with
         | One -> "Array1"
         | Two -> "Array2"
         | Three -> "Array3"
         | Many -> "Genarray" in
       Ldot((Lident ("Bigarray")), submodule_name) in
   ghloc ~loc (Ldot(prefix,opname))

let builtin_arraylike_index loc paren_kind index = match paren_kind with
    | Paren | Bracket -> One, [Nolabel, index]
    | Brace ->
       (* Multi-indices for bigarray are comma-separated ([a.{1,2,3,4}]) *)
       match bigarray_untuplify index with
     | [x] -> One, [Nolabel, x]
     | [x;y] -> Two, [Nolabel, x; Nolabel, y]
     | [x;y;z] -> Three, [Nolabel, x; Nolabel, y; Nolabel, z]
     | coords -> Many, [Nolabel, ghexp ~loc (Pexp_array (coords))]

let builtin_indexing_operators : (unit, expression) array_family  =
  { index = builtin_arraylike_index; name = builtin_arraylike_name }

let paren_to_strings = function
  | Paren -> "(", ")"
  | Bracket -> "[", "]"
  | Brace -> "{", "}"

let user_indexing_operator_name loc (prefix,ext) ~assign paren_kind n =
  let name =
    let assign = if assign then "<-" else "" in
    let mid = match n with
        | Many | Three | Two  -> ";.."
        | One -> "" in
    let left, right = paren_to_strings paren_kind in
    String.concat "" ["."; ext; left; mid; right; assign] in
  let lid = match prefix with
    | None -> Lident (name)
    | Some p -> Ldot(p,name) in
  ghloc ~loc lid

let user_index loc _ index =
  (* Multi-indices for user-defined operators are semicolon-separated
     ([a.%[1;2;3;4]]) *)
  match index with
    | [a] -> One, [Nolabel, a]
    | l -> Many, [Nolabel, mkexp ~loc (Pexp_array (l))]

let user_indexing_operators:
      (Longident.t option * string, expression list) array_family
  = { index = user_index; name = user_indexing_operator_name }

let mk_indexop_expr array_indexing_operator ~loc
      (array,dot,paren,index,set_expr) =
  let assign = match set_expr with None -> false | Some _ -> true in
  let n, index = array_indexing_operator.index loc paren index in
  let fn = array_indexing_operator.name loc dot ~assign paren n in
  let set_arg = match set_expr with
    | None -> []
    | Some expr -> [Nolabel, expr] in
  let args = (Nolabel,array) :: index @ set_arg in
  mkexp ~loc (Pexp_apply(ghexp ~loc (Pexp_ident fn), args))

let indexop_unclosed_error loc_s s loc_e =
  let left, right = paren_to_strings s in
  unclosed left loc_s right loc_e

let lapply ~loc p1 p2 =
  if !Clflags.applicative_functors
  then Lapply(p1, p2)
  else raise (Syntaxerr.Error(
                  Syntaxerr.Applicative_path (make_loc loc)))

(* [loc_map] could be [Location.map]. *)
let loc_map (f : 'a -> 'b) (x : 'a Location.loc) : 'b Location.loc =
  { x with txt = f x.txt }

let make_ghost x = { x with loc = { x.loc with loc_ghost = true }}

let loc_last (id : Longident.t Location.loc) : string Location.loc =
  loc_map Longident.last id

let loc_lident (id : string Location.loc) : Longident.t Location.loc =
  loc_map (fun x -> Lident (x)) id

let exp_of_longident lid =
  let lid = loc_map (fun id -> Lident ((Longident.last id))) lid in
  Exp.mk ~loc:lid.loc (Pexp_ident lid)

let exp_of_label lbl =
  Exp.mk ~loc:lbl.loc (Pexp_ident (loc_lident lbl))

let pat_of_label lbl =
  Pat.mk ~loc:lbl.loc  (Ppat_var (loc_last lbl))

let mk_newtypes ~loc newtypes exp =
  let mkexp = mkexp ~loc in
  List.fold_right (fun newtype exp -> mkexp (Pexp_newtype (newtype, exp)))
    newtypes exp

let wrap_type_annotation ~loc newtypes core_type body =
  let mkexp, ghtyp = mkexp ~loc, ghtyp ~loc in
  let mk_newtypes = mk_newtypes ~loc in
  let exp = mkexp(Pexp_constraint(body,core_type)) in
  let exp = mk_newtypes newtypes exp in
  (exp, ghtyp(Ptyp_poly(newtypes, Typ.varify_constructors newtypes core_type)))

let wrap_exp_attrs ~loc body (ext, attrs) =
  let ghexp = ghexp ~loc in
  (* todo: keep exact location for the entire attribute *)
  let body = {body with pexp_attributes = attrs @ body.pexp_attributes} in
  match ext with
  | None -> body
  | Some id -> ghexp(Pexp_extension (id, PStr ([mkstrexp ~loc (body) []])))

let mkexp_attrs ~loc d attrs =
  wrap_exp_attrs ~loc (mkexp ~loc d) attrs

let wrap_typ_attrs ~loc typ (ext, attrs) =
  (* todo: keep exact location for the entire attribute *)
  let typ = {typ with ptyp_attributes = attrs @ typ.ptyp_attributes} in
  match ext with
  | None -> typ
  | Some id -> ghtyp ~loc (Ptyp_extension (id, PTyp typ))

let wrap_pat_attrs ~loc pat (ext, attrs) =
  (* todo: keep exact location for the entire attribute *)
  let pat = {pat with ppat_attributes = attrs @ pat.ppat_attributes} in
  match ext with
  | None -> pat
  | Some id -> ghpat ~loc (Ppat_extension (id, PPat (pat, None)))

let mkpat_attrs ~loc d attrs =
  wrap_pat_attrs ~loc (mkpat ~loc d) attrs

let wrap_class_attrs ~loc:_ body attrs =
  {body with pcl_attributes = attrs @ body.pcl_attributes}
let wrap_mod_attrs ~loc:_ attrs body =
  {body with pmod_attributes = attrs @ body.pmod_attributes}
let wrap_mty_attrs ~loc:_ attrs body =
  {body with pmty_attributes = attrs @ body.pmty_attributes}

let wrap_str_ext ~loc body ext =
  match ext with
  | None -> body
  | Some id -> ghstr ~loc (Pstr_extension ((id, PStr ([body])), []))

let wrap_mkstr_ext ~loc (item, ext) =
  wrap_str_ext ~loc (mkstr ~loc item) ext

let wrap_sig_ext ~loc body ext =
  match ext with
  | None -> body
  | Some id -> ghsig ~loc (Psig_extension ((id, PSig ([body])), []))

let wrap_mksig_ext ~loc (item, ext) =
  wrap_sig_ext ~loc (mksig ~loc item) ext

let mk_quotedext ~loc (id, idloc, str, strloc, delim) =
  let exp_id = mkloc (id) idloc in
  let e = ghexp ~loc (Pexp_constant ((Pconst_string (str, strloc, delim)))) in
  (exp_id, PStr ([mkstrexp ~loc (e) []]))

let text_str pos = Str.text (rhs_text pos)
let text_sig pos = Sig.text (rhs_text pos)
let text_cstr pos = Cf.text (rhs_text pos)
let text_csig pos = Ctf.text (rhs_text pos)
let text_def pos =
  List.map (fun def -> Ptop_def ([def])) (Str.text (rhs_text pos))

let extra_text startpos endpos text items =
  match items with
  | [] ->
      let post = rhs_post_text endpos in
      let post_extras = rhs_post_extra_text endpos in
      text post @ text post_extras
  | _ :: _ ->
      let pre_extras = rhs_pre_extra_text startpos in
      let post_extras = rhs_post_extra_text endpos in
        text pre_extras @ items @ text post_extras

let extra_str p1 p2 items = extra_text p1 p2 Str.text items
let extra_sig p1 p2 items = extra_text p1 p2 Sig.text items
let extra_cstr p1 p2 items = extra_text p1 p2 Cf.text items
let extra_csig p1 p2 items = extra_text p1 p2 Ctf.text  items
let extra_def p1 p2 items =
  extra_text p1 p2
    (fun txt -> List.map (fun def -> Ptop_def ([def])) (Str.text txt))
    items

let extra_rhs_core_type ct ~pos =
  let docs = rhs_info pos in
  { ct with ptyp_attributes = add_info_attrs docs ct.ptyp_attributes }

type let_binding =
  { lb_pattern: pattern;
    lb_expression: expression;
    lb_is_pun: bool;
    lb_attributes: attributes;
    lb_docs: docs Lazy.t;
    lb_text: text Lazy.t;
    lb_loc: Location.t; }

type let_bindings =
  { lbs_bindings: let_binding list;
    lbs_rec: rec_flag;
    lbs_extension: string Asttypes.loc option }

let mklb first ~loc (p, e, is_pun) attrs =
  {
    lb_pattern = p;
    lb_expression = e;
    lb_is_pun = is_pun;
    lb_attributes = attrs;
    lb_docs = symbol_docs_lazy loc;
    lb_text = (if first then empty_text_lazy
               else symbol_text_lazy (fst loc));
    lb_loc = make_loc loc;
  }

let addlb lbs lb =
  if lb.lb_is_pun && lbs.lbs_extension = None then syntax_error ();
  { lbs with lbs_bindings = List.append ([lb]) lbs.lbs_bindings }

let mklbs ext rf lb =
  let lbs = {
    lbs_bindings = [];
    lbs_rec = rf;
    lbs_extension = ext;
  } in
  addlb lbs lb

let vb_of_lb lb =
  Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
    lb.lb_pattern lb.lb_expression

let val_of_let_bindings ~loc lbs =
  let bindings =
    (List.map
      (fun lb ->
         Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
           ~docs:(Lazy.force lb.lb_docs)
           ~text:(Lazy.force lb.lb_text)
           lb.lb_pattern lb.lb_expression))
      lbs.lbs_bindings
  in
  let str = mkstr ~loc (Pstr_value(lbs.lbs_rec, List.rev bindings)) in
  match lbs.lbs_extension with
  | None -> str
  | Some id -> ghstr ~loc (Pstr_extension((id, PStr ([str])), []))

let expr_of_let_bindings ~loc lbs body =
  let bindings =
    (List.map vb_of_lb)
      lbs.lbs_bindings
  in
    mkexp_attrs ~loc (Pexp_let(lbs.lbs_rec, List.rev bindings, body))
      (lbs.lbs_extension, [])

let class_of_let_bindings ~loc lbs body =
  let bindings =
    (List.map vb_of_lb)
      lbs.lbs_bindings
  in
    (* Our use of let_bindings(no_ext) guarantees the following: *)
    assert (lbs.lbs_extension = None);
    mkclass ~loc (Pcl_let (lbs.lbs_rec, List.rev bindings, body))

(* Alternatively, we could keep the generic module type in the Parsetree
   and extract the package type during type-checking. In that case,
   the assertions below should be turned into explicit checks. *)
let package_type_of_module_type pmty =
  let err loc s =
    raise (Syntaxerr.Error (Syntaxerr.Invalid_package_type (loc, s)))
  in
  let map_cstr = function
    | Pwith_type (lid, ptyp) ->
        let loc = ptyp.ptype_loc in
        if ptyp.ptype_params <> [] then
          err loc "parametrized types are not supported";
        if ptyp.ptype_cstrs <> [] then
          err loc "constrained types are not supported";
        if ptyp.ptype_private <> Public then
          err loc "private types are not supported";

        (* restrictions below are checked by the 'with_constraint' rule *)
        assert (ptyp.ptype_kind = Ptype_abstract);
        assert (ptyp.ptype_attributes = []);
        let ty =
          match ptyp.ptype_manifest with
          | Some ty -> ty
          | None -> assert false
        in
        (lid, ty)
    | _ ->
        err pmty.pmty_loc "only 'with type t =' constraints are supported"
  in
  match pmty with
  | {pmty_desc = Pmty_ident lid} -> (lid, [], pmty.pmty_attributes)
  | {pmty_desc = Pmty_with({pmty_desc = Pmty_ident lid}, cstrs)} ->
      (lid, (List.map map_cstr) cstrs, pmty.pmty_attributes)
  | _ ->
      err pmty.pmty_loc
        "only module type identifier and 'with type' constraints are supported"

let mk_directive_arg ~loc k =
  { pdira_desc = k;
    pdira_loc = make_loc loc;
  }

let mk_directive ~loc name arg =
  Ptop_dir {
      pdir_name = name;
      pdir_arg = arg;
      pdir_loc = make_loc loc;
    }

%}
%start implementation
%start interface
%start parse_any_longident
%start parse_constr_longident
%start parse_core_type
%start parse_expression
%start parse_mod_ext_longident
%start parse_mod_longident
%start parse_module_expr
%start parse_module_type
%start parse_mty_longident
%start parse_pattern
%start parse_val_longident
%start toplevel_phrase
%start use_file
%token AMPERAMPER
%token AMPERSAND
%token AND
%token <string> ANDOP
%token AS
%token ASSERT
%token BACKQUOTE
%token BANG
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token CLASS
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COMMA
%token <string * Location.t> COMMENT
%token CONSTRAINT
%token DO
%token <Docstrings.docstring> DOCSTRING
%token DONE
%token DOT
%token DOTDOT
%token <string> DOTOP
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EOL
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <string * char option> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token HASH
%token <string> HASHOP
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token INHERIT
%token INITIALIZER
%token <string * char option> INT
%token <string> LABEL
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETAT
%token LBRACKETATAT
%token LBRACKETATATAT
%token LBRACKETBAR
%token LBRACKETGREATER
%token LBRACKETLESS
%token LBRACKETPERCENT
%token LBRACKETPERCENTPERCENT
%token LESS
%token LESSMINUS
%token LET
%token <string> LETOP
%token <string> LIDENT
%token LPAREN
%token MATCH
%token METHOD
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token NEW
%token NONREC
%token OBJECT
%token OF
%token OPEN
%token <string> OPTLABEL
%token OR
%token PERCENT
%token PLUS
%token PLUSDOT
%token PLUSEQ
%token <string> PREFIXOP
%token PRIVATE
%token QUESTION
%token QUOTE
%token <string * Location.t * string * Location.t * string option> QUOTED_STRING_EXPR
%token <string * Location.t * string * Location.t * string option> QUOTED_STRING_ITEM
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token SIG
%token STAR
%token <string * Location.t * string option> STRING
%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH
%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI
%nonassoc LET
%nonassoc below_WITH
%nonassoc FUNCTION WITH
%nonassoc AND
%nonassoc THEN
%nonassoc ELSE
%nonassoc LESSMINUS
%right COLONEQUAL
%nonassoc AS
%left BAR
%nonassoc below_COMMA
%left COMMA
%right MINUSGREATER
%right BARBAR OR
%right AMPERAMPER AMPERSAND
%nonassoc below_EQUAL
%left EQUAL GREATER INFIXOP0 LESS
%right INFIXOP1
%nonassoc below_LBRACKETAT
%nonassoc LBRACKETAT
%right COLONCOLON
%left INFIXOP2 MINUS MINUSDOT PLUS PLUSDOT PLUSEQ
%left INFIXOP3 PERCENT STAR
%right INFIXOP4
%nonassoc prec_unary_minus prec_unary_plus
%nonassoc prec_constant_constructor
%nonassoc prec_constr_appl
%nonassoc below_HASH
%nonassoc HASH
%left HASHOP
%nonassoc below_DOT
%nonassoc DOT DOTOP
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT LBRACE LBRACELESS LBRACKET LBRACKETBAR LBRACKETPERCENT LIDENT LPAREN NEW OBJECT PREFIXOP QUOTED_STRING_EXPR STRING TRUE UIDENT
%type <Parsetree.structure> implementation
%type <Parsetree.signature> interface
%type <Longident.t> parse_any_longident
%type <Longident.t> parse_constr_longident
%type <Parsetree.core_type> parse_core_type
%type <Parsetree.expression> parse_expression
%type <Longident.t> parse_mod_ext_longident
%type <Longident.t> parse_mod_longident
%type <Parsetree.module_expr> parse_module_expr
%type <Parsetree.module_type> parse_module_type
%type <Longident.t> parse_mty_longident
%type <Parsetree.pattern> parse_pattern
%type <Longident.t> parse_val_longident
%type <Parsetree.toplevel_phrase> toplevel_phrase
%type <Parsetree.toplevel_phrase list> use_file
%%

option_BAR_:
  
    {    ( None )}
| x = BAR
    {    ( Some x )}

option_SEMI_:
  
    {    ( None )}
| x = SEMI
    {    ( Some x )}

option_preceded_AS_mkrhs_LIDENT___:
  
    {    ( None )}
| _1 = AS _1_inlined1 = LIDENT
    {let x =
  let x =
    let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
      ( x )
in
    ( Some x )}

option_preceded_COLON_core_type__:
  
    {    ( None )}
| _1 = COLON x = core_type
    {let x =     ( x ) in
    ( Some x )}

option_preceded_EQUAL_expr__:
  
    {    ( None )}
| _1 = EQUAL x = expr
    {let x =     ( x ) in
    ( Some x )}

option_preceded_EQUAL_module_type__:
  
    {    ( None )}
| _1 = EQUAL x = module_type
    {let x =     ( x ) in
    ( Some x )}

option_preceded_EQUAL_pattern__:
  
    {    ( None )}
| _1 = EQUAL x = pattern
    {let x =     ( x ) in
    ( Some x )}

option_preceded_EQUAL_seq_expr__:
  
    {    ( None )}
| _1 = EQUAL x = seq_expr
    {let x =     ( x ) in
    ( Some x )}

option_type_constraint_:
  
    {    ( None )}
| x = type_constraint
    {    ( Some x )}

list_and_class_declaration_:
  
    {    ( [] )}
| _1 = AND _1_inlined1 = list_attribute_ virt = virtual_flag params = formal_class_parameters _1_inlined2 = LIDENT body = class_fun_binding _1_inlined3 = list_post_item_attribute_ xs = list_and_class_declaration_
    {let x =
  let attrs2 =
    let _1 = _1_inlined3 in
        ( _1 )
  in
  let _endpos_attrs2_ = _endpos__1_inlined3_ in
  let id =
    let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let attrs1 =
    let _1 = _1_inlined1 in
        ( _1 )
  in
  let _endpos = _endpos_attrs2_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
    (
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc _sloc in
    let docs = symbol_docs _sloc in
    let text = symbol_text _symbolstartpos in
    Ci.mk id body ~virt ~params ~attrs ~loc ~text ~docs
  )
in
    ( x :: xs )}

list_and_class_description_:
  
    {    ( [] )}
| _1 = AND _1_inlined1 = list_attribute_ virt = virtual_flag params = formal_class_parameters _1_inlined2 = LIDENT _6 = COLON cty = class_type _1_inlined3 = list_post_item_attribute_ xs = list_and_class_description_
    {let x =
  let attrs2 =
    let _1 = _1_inlined3 in
        ( _1 )
  in
  let _endpos_attrs2_ = _endpos__1_inlined3_ in
  let id =
    let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let attrs1 =
    let _1 = _1_inlined1 in
        ( _1 )
  in
  let _endpos = _endpos_attrs2_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      (
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc _sloc in
      let docs = symbol_docs _sloc in
      let text = symbol_text _symbolstartpos in
      Ci.mk id cty ~virt ~params ~attrs ~loc ~text ~docs
    )
in
    ( x :: xs )}

list_and_class_type_declaration_:
  
    {    ( [] )}
| _1 = AND _1_inlined1 = list_attribute_ virt = virtual_flag params = formal_class_parameters _1_inlined2 = LIDENT _6 = EQUAL csig = class_signature _1_inlined3 = list_post_item_attribute_ xs = list_and_class_type_declaration_
    {let x =
  let attrs2 =
    let _1 = _1_inlined3 in
        ( _1 )
  in
  let _endpos_attrs2_ = _endpos__1_inlined3_ in
  let id =
    let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let attrs1 =
    let _1 = _1_inlined1 in
        ( _1 )
  in
  let _endpos = _endpos_attrs2_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      (
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc _sloc in
      let docs = symbol_docs _sloc in
      let text = symbol_text _symbolstartpos in
      Ci.mk id csig ~virt ~params ~attrs ~loc ~text ~docs
    )
in
    ( x :: xs )}

list_and_module_binding_:
  
    {    ( [] )}
| _1 = AND _1_inlined1 = list_attribute_ _1_inlined2 = module_name body = module_binding_body _1_inlined3 = list_post_item_attribute_ xs = list_and_module_binding_
    {let x =
  let attrs2 =
    let _1 = _1_inlined3 in
        ( _1 )
  in
  let _endpos_attrs2_ = _endpos__1_inlined3_ in
  let name =
    let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let attrs1 =
    let _1 = _1_inlined1 in
        ( _1 )
  in
  let _endpos = _endpos_attrs2_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
    (
    let loc = make_loc _sloc in
    let attrs = attrs1 @ attrs2 in
    let docs = symbol_docs _sloc in
    let text = symbol_text _symbolstartpos in
    Mb.mk name body ~attrs ~loc ~text ~docs
  )
in
    ( x :: xs )}

list_and_module_declaration_:
  
    {    ( [] )}
| _1 = AND _1_inlined1 = list_attribute_ _1_inlined2 = module_name _4 = COLON mty = module_type _1_inlined3 = list_post_item_attribute_ xs = list_and_module_declaration_
    {let x =
  let attrs2 =
    let _1 = _1_inlined3 in
        ( _1 )
  in
  let _endpos_attrs2_ = _endpos__1_inlined3_ in
  let name =
    let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let attrs1 =
    let _1 = _1_inlined1 in
        ( _1 )
  in
  let _endpos = _endpos_attrs2_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
    (
    let attrs = attrs1 @ attrs2 in
    let docs = symbol_docs _sloc in
    let loc = make_loc _sloc in
    let text = symbol_text _symbolstartpos in
    Md.mk name mty ~attrs ~loc ~text ~docs
  )
in
    ( x :: xs )}

list_attribute_:
  
    {    ( [] )}
| x = attribute xs = list_attribute_
    {    ( x :: xs )}

list_generic_and_type_declaration_type_kind__:
  
    {    ( [] )}
| _1 = AND _1_inlined1 = list_attribute_ params = type_parameters _1_inlined2 = LIDENT kind_priv_manifest = type_kind xs_inlined1 = reversed_llist_preceded_CONSTRAINT_constrain__ _1_inlined3 = list_post_item_attribute_ xs = list_generic_and_type_declaration_type_kind__
    {let x =
  let xs = xs_inlined1 in
  let attrs2 =
    let _1 = _1_inlined3 in
        ( _1 )
  in
  let _endpos_attrs2_ = _endpos__1_inlined3_ in
  let cstrs =
    let _1 =
      let xs =     ( List.rev xs ) in
          ( xs )
    in
        ( _1 )
  in
  let id =
    let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let attrs1 =
    let _1 = _1_inlined1 in
        ( _1 )
  in
  let _endpos = _endpos_attrs2_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      (
      let (kind, priv, manifest) = kind_priv_manifest in
      let docs = symbol_docs _sloc in
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc _sloc in
      let text = symbol_text _symbolstartpos in
      Type.mk id ~params ~cstrs ~kind ~priv ~manifest ~attrs ~loc ~docs ~text
    )
in
    ( x :: xs )}

list_generic_and_type_declaration_type_subst_kind__:
  
    {    ( [] )}
| _1 = AND _1_inlined1 = list_attribute_ params = type_parameters _1_inlined2 = LIDENT _1_inlined3 = COLONEQUAL _2 = nonempty_type_kind xs_inlined1 = reversed_llist_preceded_CONSTRAINT_constrain__ _1_inlined4 = list_post_item_attribute_ xs = list_generic_and_type_declaration_type_subst_kind__
    {let x =
  let xs = xs_inlined1 in
  let attrs2 =
    let _1 = _1_inlined4 in
        ( _1 )
  in
  let _endpos_attrs2_ = _endpos__1_inlined4_ in
  let cstrs =
    let _1 =
      let xs =     ( List.rev xs ) in
          ( xs )
    in
        ( _1 )
  in
  let kind_priv_manifest =       ( _2 ) in
  let id =
    let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let attrs1 =
    let _1 = _1_inlined1 in
        ( _1 )
  in
  let _endpos = _endpos_attrs2_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      (
      let (kind, priv, manifest) = kind_priv_manifest in
      let docs = symbol_docs _sloc in
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc _sloc in
      let text = symbol_text _symbolstartpos in
      Type.mk id ~params ~cstrs ~kind ~priv ~manifest ~attrs ~loc ~docs ~text
    )
in
    ( x :: xs )}

list_post_item_attribute_:
  
    {    ( [] )}
| x = post_item_attribute xs = list_post_item_attribute_
    {    ( x :: xs )}

list_signature_element_:
  
    {    ( [] )}
| _1 = SEMISEMI xs = list_signature_element_
    {let x =
  let _1 =
    let _startpos = _startpos__1_ in
      ( text_sig _startpos )
  in
        ( _1 )
in
    ( x :: xs )}
| _1 = signature_item xs = list_signature_element_
    {let x =
  let _1 =
    let _startpos = _startpos__1_ in
      ( text_sig _startpos @ [_1] )
  in
        ( _1 )
in
    ( x :: xs )}

list_structure_element_:
  
    {    ( [] )}
| _1 = SEMISEMI xs = list_structure_element_
    {let x =
  let _1 =
    let ys =
      let items =     ( [] ) in
          ( items )
    in
    let xs =
      let _startpos = _startpos__1_ in
        ( text_str _startpos )
    in
        ( xs @ ys )
  in
        ( _1 )
in
    ( x :: xs )}
| _1 = SEMISEMI e = seq_expr _1_inlined1 = list_post_item_attribute_ xs = list_structure_element_
    {let x =
  let _1 =
    let ys =
      let (_endpos__1_, _1) = (_endpos__1_inlined1_, _1_inlined1) in
      let items =
        let x =
          let _1 =
            let _1 =
              let attrs =     ( _1 ) in
              let _endpos_attrs_ = _endpos__1_ in
              let _endpos = _endpos_attrs_ in
              let _symbolstartpos = _startpos_e_ in
              let _sloc = (_symbolstartpos, _endpos) in
                  ( mkstrexp ~loc:_sloc e attrs )
            in
            let _startpos__1_ = _startpos_e_ in
            let _startpos = _startpos__1_ in
              ( text_str _startpos @ [_1] )
          in
          let _startpos__1_ = _startpos_e_ in
          let _endpos = _endpos__1_ in
          let _startpos = _startpos__1_ in
            ( mark_rhs_docs _startpos _endpos;
    _1 )
        in
            ( x )
      in
          ( items )
    in
    let xs =
      let _startpos = _startpos__1_ in
        ( text_str _startpos )
    in
        ( xs @ ys )
  in
        ( _1 )
in
    ( x :: xs )}
| _1 = structure_item xs = list_structure_element_
    {let x =
  let _1 =
    let _startpos = _startpos__1_ in
      ( text_str _startpos @ [_1] )
  in
        ( _1 )
in
    ( x :: xs )}

list_text_csig_class_sig_field__:
  
    {    ( [] )}
| _1 = class_sig_field xs = list_text_csig_class_sig_field__
    {let x =
  let _startpos = _startpos__1_ in
    ( text_csig _startpos @ [_1] )
in
    ( x :: xs )}

list_text_cstr_class_field__:
  
    {    ( [] )}
| _1 = class_field xs = list_text_cstr_class_field__
    {let x =
  let _startpos = _startpos__1_ in
    ( text_cstr _startpos @ [_1] )
in
    ( x :: xs )}

list_text_str_structure_item__:
  
    {    ( [] )}
| _1 = structure_item xs = list_text_str_structure_item__
    {let x =
  let _startpos = _startpos__1_ in
    ( text_str _startpos @ [_1] )
in
    ( x :: xs )}

list_use_file_element_:
  
    {    ( [] )}
| _1 = SEMISEMI xs = list_use_file_element_
    {let x =
  let _1 =
    let x =
      let _1 =     ( [] ) in
          ( _1 )
    in
        ( x )
  in
        ( _1 )
in
    ( x :: xs )}
| _1 = SEMISEMI e = seq_expr _1_inlined1 = list_post_item_attribute_ xs = list_use_file_element_
    {let x =
  let _1 =
    let x =
      let (_endpos__1_, _1) = (_endpos__1_inlined1_, _1_inlined1) in
      let _1 =
        let x =
          let _1 =
            let _1 =
              let attrs =     ( _1 ) in
              let _endpos_attrs_ = _endpos__1_ in
              let _endpos = _endpos_attrs_ in
              let _symbolstartpos = _startpos_e_ in
              let _sloc = (_symbolstartpos, _endpos) in
                  ( mkstrexp ~loc:_sloc e attrs )
            in
              ( Ptop_def ([_1]) )
          in
          let _startpos__1_ = _startpos_e_ in
          let _startpos = _startpos__1_ in
            ( text_def _startpos @ [_1] )
        in
            ( x )
      in
          ( _1 )
    in
        ( x )
  in
        ( _1 )
in
    ( x :: xs )}
| _1 = structure_item xs = list_use_file_element_
    {let x =
  let _1 =
    let _1 =   ( Ptop_def ([_1]) ) in
    let _startpos = _startpos__1_ in
      ( text_def _startpos @ [_1] )
  in
        ( _1 )
in
    ( x :: xs )}
| _1 = toplevel_directive xs = list_use_file_element_
    {let x =
  let _1 =
    let _1 =
      let _endpos = _endpos__1_ in
      let _startpos = _startpos__1_ in
        ( mark_rhs_docs _startpos _endpos;
    _1 )
    in
    let _startpos = _startpos__1_ in
      ( text_def _startpos @ [_1] )
  in
        ( _1 )
in
    ( x :: xs )}

nonempty_list_mkrhs_LIDENT__:
  _1 = LIDENT
    {let x =
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
    ( [ x ] )}
| _1 = LIDENT xs = nonempty_list_mkrhs_LIDENT__
    {let x =
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
    ( x :: xs )}

nonempty_list_raw_string_:
  s = STRING
    {let x =     ( let body, _, _ = s in body ) in
    ( [ x ] )}
| s = STRING xs = nonempty_list_raw_string_
    {let x =     ( let body, _, _ = s in body ) in
    ( x :: xs )}

reversed_llist_preceded_CONSTRAINT_constrain__:
  
    {    ( [] )}
| xs = reversed_llist_preceded_CONSTRAINT_constrain__ _1 = CONSTRAINT _1_inlined1 = core_type _2 = EQUAL _3 = core_type
    {let x =
  let x =
    let (_startpos__1_, _1) = (_startpos__1_inlined1_, _1_inlined1) in
    let _endpos = _endpos__3_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( _1, _3, make_loc _sloc )
  in
      ( x )
in
    ( x :: xs )}

reversed_nonempty_llist_functor_arg_:
  x = functor_arg
    {    ( [ x ] )}
| xs = reversed_nonempty_llist_functor_arg_ x = functor_arg
    {    ( x :: xs )}

reversed_nonempty_llist_labeled_simple_expr_:
  x = labeled_simple_expr
    {    ( [ x ] )}
| xs = reversed_nonempty_llist_labeled_simple_expr_ x = labeled_simple_expr
    {    ( x :: xs )}

reversed_nonempty_llist_name_tag_:
  x = name_tag
    {    ( [ x ] )}
| xs = reversed_nonempty_llist_name_tag_ x = name_tag
    {    ( x :: xs )}

reversed_nonempty_llist_typevar_:
  _1 = QUOTE _1_inlined1 = ident
    {let x =
  let _2 =
    let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
      ( _2 )
in
    ( [ x ] )}
| xs = reversed_nonempty_llist_typevar_ _1 = QUOTE _1_inlined1 = ident
    {let x =
  let _2 =
    let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
      ( _2 )
in
    ( x :: xs )}

reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_:
  _1 = alias_type
    {let xs =
  let x =     ( _1 ) in
      ( [ x ] )
in
    ( xs )}
| xs = reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ _2 = AMPERSAND _1 = alias_type
    {let xs =
  let x =     ( _1 ) in
      ( x :: xs )
in
    ( xs )}

reversed_separated_nonempty_llist_AND_with_constraint_:
  x = with_constraint
    {let xs =     ( [ x ] ) in
    ( xs )}
| xs = reversed_separated_nonempty_llist_AND_with_constraint_ _2 = AND x = with_constraint
    {let xs =     ( x :: xs ) in
    ( xs )}

reversed_separated_nonempty_llist_BAR_row_field_:
  x = row_field
    {let xs =     ( [ x ] ) in
    ( xs )}
| xs = reversed_separated_nonempty_llist_BAR_row_field_ _2 = BAR x = row_field
    {let xs =     ( x :: xs ) in
    ( xs )}

reversed_separated_nonempty_llist_COMMA_core_type_:
  x = core_type
    {let xs =     ( [ x ] ) in
    ( xs )}
| xs = reversed_separated_nonempty_llist_COMMA_core_type_ _2 = COMMA x = core_type
    {let xs =     ( x :: xs ) in
    ( xs )}

reversed_separated_nonempty_llist_COMMA_type_parameter_:
  x = type_parameter
    {let xs =     ( [ x ] ) in
    ( xs )}
| xs = reversed_separated_nonempty_llist_COMMA_type_parameter_ _2 = COMMA x = type_parameter
    {let xs =     ( x :: xs ) in
    ( xs )}

reversed_separated_nonempty_llist_STAR_atomic_type_:
  x = atomic_type
    {let xs =     ( [ x ] ) in
    ( xs )}
| xs = reversed_separated_nonempty_llist_STAR_atomic_type_ _2 = STAR x = atomic_type
    {let xs =     ( x :: xs ) in
    ( xs )}

reversed_separated_nontrivial_llist_COMMA_core_type_:
  xs = reversed_separated_nontrivial_llist_COMMA_core_type_ _2 = COMMA x = core_type
    {    ( x :: xs )}
| x1 = core_type _2 = COMMA x2 = core_type
    {    ( [ x2; x1 ] )}

reversed_separated_nontrivial_llist_COMMA_expr_:
  xs = reversed_separated_nontrivial_llist_COMMA_expr_ _2 = COMMA x = expr
    {    ( x :: xs )}
| x1 = expr _2 = COMMA x2 = expr
    {    ( [ x2; x1 ] )}

reversed_separated_nontrivial_llist_STAR_atomic_type_:
  xs = reversed_separated_nontrivial_llist_STAR_atomic_type_ _2 = STAR x = atomic_type
    {    ( x :: xs )}
| x1 = atomic_type _2 = STAR x2 = atomic_type
    {    ( [ x2; x1 ] )}

separated_or_terminated_nonempty_list_SEMI_expr_:
  x = expr
    {let _2 =     ( None ) in
    ( [x] )}
| x = expr x_inlined1 = SEMI
    {let _2 =
  let x = x_inlined1 in
      ( Some x )
in
    ( [x] )}
| x = expr _2 = SEMI xs = separated_or_terminated_nonempty_list_SEMI_expr_
    {    ( x :: xs )}

separated_or_terminated_nonempty_list_SEMI_object_expr_field_:
  _1 = LIDENT oe = option_preceded_EQUAL_expr__
    {let _2 =     ( None ) in
let x =
  let label =
    let _1 =                                                 ( _1 ) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
        ( let label, e =
          match oe with
          | None ->
              (* No expression; this is a pun. Desugar it. *)
              make_ghost label, exp_of_label (label)
          | Some e ->
              label, e
        in
        label, e )
in
    ( [x] )}
| _1 = LIDENT oe = option_preceded_EQUAL_expr__ x = SEMI
    {let _2 =     ( Some x ) in
let x =
  let label =
    let _1 =                                                 ( _1 ) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
        ( let label, e =
          match oe with
          | None ->
              (* No expression; this is a pun. Desugar it. *)
              make_ghost label, exp_of_label (label)
          | Some e ->
              label, e
        in
        label, e )
in
    ( [x] )}
| _1 = LIDENT oe = option_preceded_EQUAL_expr__ _2 = SEMI xs = separated_or_terminated_nonempty_list_SEMI_object_expr_field_
    {let x =
  let label =
    let _1 =                                                 ( _1 ) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
        ( let label, e =
          match oe with
          | None ->
              (* No expression; this is a pun. Desugar it. *)
              make_ghost label, exp_of_label (label)
          | Some e ->
              label, e
        in
        label, e )
in
    ( x :: xs )}

separated_or_terminated_nonempty_list_SEMI_pattern_:
  x = pattern
    {let _2 =     ( None ) in
    ( [x] )}
| x = pattern x_inlined1 = SEMI
    {let _2 =
  let x = x_inlined1 in
      ( Some x )
in
    ( [x] )}
| x = pattern _2 = SEMI xs = separated_or_terminated_nonempty_list_SEMI_pattern_
    {    ( x :: xs )}

separated_or_terminated_nonempty_list_SEMI_record_expr_field_:
  _1 = label_longident c = option_type_constraint_ eo = option_preceded_EQUAL_expr__
    {let _2 =     ( None ) in
let x =
  let label =
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let _startpos_label_ = _startpos__1_ in
  let _endpos = _endpos_eo_ in
  let _symbolstartpos = _startpos_label_ in
  let _sloc = (_symbolstartpos, _endpos) in
        ( let constraint_loc, label, e =
          match eo with
          | None ->
              (* No pattern; this is a pun. Desugar it. *)
              _sloc, (make_ghost label), exp_of_longident label
          | Some e ->
              (_startpos_c_, _endpos), label, e
        in
        label, mkexp_opt_constraint ~loc:constraint_loc e c )
in
    ( [x] )}
| _1 = label_longident c = option_type_constraint_ eo = option_preceded_EQUAL_expr__ x = SEMI
    {let _2 =     ( Some x ) in
let x =
  let label =
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let _startpos_label_ = _startpos__1_ in
  let _endpos = _endpos_eo_ in
  let _symbolstartpos = _startpos_label_ in
  let _sloc = (_symbolstartpos, _endpos) in
        ( let constraint_loc, label, e =
          match eo with
          | None ->
              (* No pattern; this is a pun. Desugar it. *)
              _sloc, (make_ghost label), exp_of_longident label
          | Some e ->
              (_startpos_c_, _endpos), label, e
        in
        label, mkexp_opt_constraint ~loc:constraint_loc e c )
in
    ( [x] )}
| _1 = label_longident c = option_type_constraint_ eo = option_preceded_EQUAL_expr__ _2 = SEMI xs = separated_or_terminated_nonempty_list_SEMI_record_expr_field_
    {let x =
  let label =
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let _startpos_label_ = _startpos__1_ in
  let _endpos = _endpos_eo_ in
  let _symbolstartpos = _startpos_label_ in
  let _sloc = (_symbolstartpos, _endpos) in
        ( let constraint_loc, label, e =
          match eo with
          | None ->
              (* No pattern; this is a pun. Desugar it. *)
              _sloc, (make_ghost label), exp_of_longident label
          | Some e ->
              (_startpos_c_, _endpos), label, e
        in
        label, mkexp_opt_constraint ~loc:constraint_loc e c )
in
    ( x :: xs )}

reversed_preceded_or_separated_nonempty_llist_BAR_match_case_:
  x = match_case
    {let _1 =     ( None ) in
    ( [x] )}
| x_inlined1 = BAR x = match_case
    {let _1 =
  let x = x_inlined1 in
      ( Some x )
in
    ( [x] )}
| xs = reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _2 = BAR x = match_case
    {    ( x :: xs )}

reversed_bar_llist_constructor_declaration_:
  d = generic_constructor_declaration_epsilon_
    {let x =     (
      let cid, vars, args, res, attrs, loc, info = d in
      Type.constructor cid ~vars ~args ~res ~attrs ~loc ~info
    ) in
      ( [x] )}
| d = generic_constructor_declaration_BAR_
    {let x =     (
      let cid, vars, args, res, attrs, loc, info = d in
      Type.constructor cid ~vars ~args ~res ~attrs ~loc ~info
    ) in
      ( [x] )}
| xs = reversed_bar_llist_constructor_declaration_ d = generic_constructor_declaration_BAR_
    {let x =     (
      let cid, vars, args, res, attrs, loc, info = d in
      Type.constructor cid ~vars ~args ~res ~attrs ~loc ~info
    ) in
      ( x :: xs )}

reversed_bar_llist_extension_constructor_:
  d = generic_constructor_declaration_epsilon_
    {let x =
  let _1 =     (
      let cid, vars, args, res, attrs, loc, info = d in
      Te.decl cid ~vars ~args ~res ~attrs ~loc ~info
    ) in
        ( _1 )
in
      ( [x] )}
| _1 = extension_constructor_rebind_epsilon_
    {let x =       ( _1 ) in
      ( [x] )}
| d = generic_constructor_declaration_BAR_
    {let x =
  let _1 =     (
      let cid, vars, args, res, attrs, loc, info = d in
      Te.decl cid ~vars ~args ~res ~attrs ~loc ~info
    ) in
        ( _1 )
in
      ( [x] )}
| _1 = extension_constructor_rebind_BAR_
    {let x =       ( _1 ) in
      ( [x] )}
| xs = reversed_bar_llist_extension_constructor_ d = generic_constructor_declaration_BAR_
    {let x =
  let _1 =     (
      let cid, vars, args, res, attrs, loc, info = d in
      Te.decl cid ~vars ~args ~res ~attrs ~loc ~info
    ) in
        ( _1 )
in
      ( x :: xs )}
| xs = reversed_bar_llist_extension_constructor_ _1 = extension_constructor_rebind_BAR_
    {let x =       ( _1 ) in
      ( x :: xs )}

reversed_bar_llist_extension_constructor_declaration_:
  d = generic_constructor_declaration_epsilon_
    {let x =     (
      let cid, vars, args, res, attrs, loc, info = d in
      Te.decl cid ~vars ~args ~res ~attrs ~loc ~info
    ) in
      ( [x] )}
| d = generic_constructor_declaration_BAR_
    {let x =     (
      let cid, vars, args, res, attrs, loc, info = d in
      Te.decl cid ~vars ~args ~res ~attrs ~loc ~info
    ) in
      ( [x] )}
| xs = reversed_bar_llist_extension_constructor_declaration_ d = generic_constructor_declaration_BAR_
    {let x =     (
      let cid, vars, args, res, attrs, loc, info = d in
      Te.decl cid ~vars ~args ~res ~attrs ~loc ~info
    ) in
      ( x :: xs )}

listx_SEMI_record_pat_field_UNDERSCORE_:
  _1 = label_longident octy = option_preceded_COLON_core_type__ opat = option_preceded_EQUAL_pattern__
    {let _2 =     ( None ) in
let x =
  let label =
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let _startpos_label_ = _startpos__1_ in
  let _endpos = _endpos_opat_ in
  let _symbolstartpos = _startpos_label_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( let constraint_loc, label, pat =
        match opat with
        | None ->
            (* No pattern; this is a pun. Desugar it.
               But that the pattern was there and the label reconstructed (which
               piece of AST is marked as ghost is important for warning
               emission). *)
            _sloc, make_ghost label, pat_of_label label
        | Some pat ->
            (_startpos_octy_, _endpos), label, pat
      in
      label, mkpat_opt_constraint ~loc:constraint_loc pat octy
    )
in
    ( [x], None )}
| _1 = label_longident octy = option_preceded_COLON_core_type__ opat = option_preceded_EQUAL_pattern__ x = SEMI
    {let _2 =     ( Some x ) in
let x =
  let label =
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let _startpos_label_ = _startpos__1_ in
  let _endpos = _endpos_opat_ in
  let _symbolstartpos = _startpos_label_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( let constraint_loc, label, pat =
        match opat with
        | None ->
            (* No pattern; this is a pun. Desugar it.
               But that the pattern was there and the label reconstructed (which
               piece of AST is marked as ghost is important for warning
               emission). *)
            _sloc, make_ghost label, pat_of_label label
        | Some pat ->
            (_startpos_octy_, _endpos), label, pat
      in
      label, mkpat_opt_constraint ~loc:constraint_loc pat octy
    )
in
    ( [x], None )}
| _1 = label_longident octy = option_preceded_COLON_core_type__ opat = option_preceded_EQUAL_pattern__ _2 = SEMI y = UNDERSCORE _4 = option_SEMI_
    {let x =
  let label =
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let _startpos_label_ = _startpos__1_ in
  let _endpos = _endpos_opat_ in
  let _symbolstartpos = _startpos_label_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( let constraint_loc, label, pat =
        match opat with
        | None ->
            (* No pattern; this is a pun. Desugar it.
               But that the pattern was there and the label reconstructed (which
               piece of AST is marked as ghost is important for warning
               emission). *)
            _sloc, make_ghost label, pat_of_label label
        | Some pat ->
            (_startpos_octy_, _endpos), label, pat
      in
      label, mkpat_opt_constraint ~loc:constraint_loc pat octy
    )
in
    ( [x], Some y )}
| _1 = label_longident octy = option_preceded_COLON_core_type__ opat = option_preceded_EQUAL_pattern__ _2 = SEMI tail = listx_SEMI_record_pat_field_UNDERSCORE_
    {let x =
  let label =
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let _startpos_label_ = _startpos__1_ in
  let _endpos = _endpos_opat_ in
  let _symbolstartpos = _startpos_label_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( let constraint_loc, label, pat =
        match opat with
        | None ->
            (* No pattern; this is a pun. Desugar it.
               But that the pattern was there and the label reconstructed (which
               piece of AST is marked as ghost is important for warning
               emission). *)
            _sloc, make_ghost label, pat_of_label label
        | Some pat ->
            (_startpos_octy_, _endpos), label, pat
      in
      label, mkpat_opt_constraint ~loc:constraint_loc pat octy
    )
in
    ( let xs, y = tail in
      x :: xs, y )}

implementation:
  _1 = structure _2 = EOF
    {    ( _1 )}

interface:
  _1 = signature _2 = EOF
    {    ( _1 )}

toplevel_phrase:
  e = seq_expr _1 = list_post_item_attribute_ _2 = SEMISEMI
    {let _1 =
  let _1 =
    let _1 =
      let attrs =     ( _1 ) in
      let _endpos_attrs_ = _endpos__1_ in
      let _endpos = _endpos_attrs_ in
      let _symbolstartpos = _startpos_e_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkstrexp ~loc:_sloc e attrs )
    in
    let _startpos__1_ = _startpos_e_ in
    let _startpos = _startpos__1_ in
      ( text_str _startpos @ [_1] )
  in
  let _startpos__1_ = _startpos_e_ in
  let _endpos = _endpos__1_ in
  let _startpos = _startpos__1_ in
                                ( (extra_str _startpos _endpos _1) )
in
    ( Ptop_def _1 )}
| xss = list_text_str_structure_item__ _2 = SEMISEMI
    {let _1 =
  let _1 =     ( List.flatten xss ) in
  let (_endpos__1_, _startpos__1_) = (_endpos_xss_, _startpos_xss_) in
  let _endpos = _endpos__1_ in
  let _startpos = _startpos__1_ in
                                ( (extra_str _startpos _endpos _1) )
in
    ( Ptop_def _1 )}
| _1 = toplevel_directive _2 = SEMISEMI
    {    ( _1 )}
| _1 = EOF
    {    ( raise End_of_file )}

use_file:
  xss = list_use_file_element_ _2 = EOF
    {let _1 =
  let _1 =
    let ys =     ( List.flatten xss ) in
    let xs =
      let _1 =     ( [] ) in
          ( _1 )
    in
        ( xs @ ys )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_xss_, _startpos_xss_) in
  let _endpos = _endpos__1_ in
  let _startpos = _startpos__1_ in
                                ( extra_def _startpos _endpos _1 )
in
    ( _1 )}
| e = seq_expr _1 = list_post_item_attribute_ xss = list_use_file_element_ _2 = EOF
    {let _1 =
  let _1 =
    let ys =     ( List.flatten xss ) in
    let xs =
      let _1 =
        let x =
          let _1 =
            let _1 =
              let attrs =     ( _1 ) in
              let _endpos_attrs_ = _endpos__1_ in
              let _endpos = _endpos_attrs_ in
              let _symbolstartpos = _startpos_e_ in
              let _sloc = (_symbolstartpos, _endpos) in
                  ( mkstrexp ~loc:_sloc e attrs )
            in
              ( Ptop_def ([_1]) )
          in
          let _startpos__1_ = _startpos_e_ in
          let _startpos = _startpos__1_ in
            ( text_def _startpos @ [_1] )
        in
            ( x )
      in
          ( _1 )
    in
        ( xs @ ys )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_xss_, _startpos_e_) in
  let _endpos = _endpos__1_ in
  let _startpos = _startpos__1_ in
                                ( extra_def _startpos _endpos _1 )
in
    ( _1 )}

parse_module_type:
  _1 = module_type _2 = EOF
    {    ( _1 )}

parse_module_expr:
  _1 = module_expr _2 = EOF
    {    ( _1 )}

parse_core_type:
  _1 = core_type _2 = EOF
    {    ( _1 )}

parse_expression:
  _1 = seq_expr _2 = EOF
    {    ( _1 )}

parse_pattern:
  _1 = pattern _2 = EOF
    {    ( _1 )}

parse_mty_longident:
  _1 = mty_longident _2 = EOF
    {    ( _1 )}

parse_val_longident:
  _1 = val_longident _2 = EOF
    {    ( _1 )}

parse_constr_longident:
  _1 = constr_longident _2 = EOF
    {    ( _1 )}

parse_mod_ext_longident:
  _1 = mod_ext_longident _2 = EOF
    {    ( _1 )}

parse_mod_longident:
  _1 = mod_longident _2 = EOF
    {    ( _1 )}

parse_any_longident:
  _1 = any_longident _2 = EOF
    {    ( _1 )}

functor_arg:
  _1 = LPAREN _2 = RPAREN
    {let _startpos = _startpos__1_ in
      ( _startpos, Unit )}
| _1 = LPAREN _1_inlined1 = module_name _3 = COLON mty = module_type _5 = RPAREN
    {let x =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _startpos = _startpos__1_ in
      ( _startpos, (Named (x, mty)) )}

module_name:
  x = UIDENT
    {let _1 =       ( Some x ) in
                 ( _1 )}
| _1 = UNDERSCORE
    {let _1 =       ( None ) in
                 ( _1 )}

module_expr:
  _1 = STRUCT _1_inlined1 = list_attribute_ s = structure _4 = END
    {let attrs =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos__4_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mkmod ~loc:_sloc ~attrs (Pmod_structure s) )}
| _1 = STRUCT _1_inlined1 = list_attribute_ _3 = structure _4 = error
    {let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _loc__4_ = (_startpos__4_, _endpos__4_) in
let _loc__1_ = (_startpos__1_, _endpos__1_) in
      ( unclosed "struct" _loc__1_ "end" _loc__4_ )}
| _1 = FUNCTOR _1_inlined1 = list_attribute_ _1_inlined2 = reversed_nonempty_llist_functor_arg_ _4 = MINUSGREATER me = module_expr
    {let args =
  let _1 = _1_inlined2 in
      ( _1 )
in
let attrs =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos_me_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mod_attrs ~loc:_sloc attrs (
          List.fold_left (fun acc (startpos, arg) ->
            mkmod ~loc:(startpos, _endpos) (Pmod_functor (arg, acc))
          ) me args
        ) )}
| me = paren_module_expr
    {      ( me )}
| me = module_expr attr = attribute
    {      ( Mod.attr me attr )}
| _1 = mod_longident
    {let _1 =
  let _1 =
    let x =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
            ( Pmod_ident x )
  in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkmod ~loc:_sloc _1 )
in
    ( _1 )}
| me1 = module_expr me2 = paren_module_expr
    {let _1 =
  let _1 =         ( Pmod_apply(me1, me2) ) in
  let (_endpos__1_, _startpos__1_) = (_endpos_me2_, _startpos_me1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkmod ~loc:_sloc _1 )
in
    ( _1 )}
| me1 = module_expr _2 = LPAREN _3 = RPAREN
    {let _1 =
  let _1 =
    let _endpos = _endpos__3_ in
    let _symbolstartpos = _startpos_me1_ in
    let _sloc = (_symbolstartpos, _endpos) in
            ( (* TODO review mkmod location *)
          Pmod_apply(me1, mkmod ~loc:_sloc (Pmod_structure ([]))) )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos__3_, _startpos_me1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkmod ~loc:_sloc _1 )
in
    ( _1 )}
| ex = extension
    {let _1 =
  let _1 =         ( Pmod_extension ex ) in
  let (_endpos__1_, _startpos__1_) = (_endpos_ex_, _startpos_ex_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkmod ~loc:_sloc _1 )
in
    ( _1 )}

paren_module_expr:
  _1 = LPAREN me = module_expr _3 = COLON mty = module_type _5 = RPAREN
    {let _endpos = _endpos__5_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mkmod ~loc:_sloc (Pmod_constraint(me, mty)) )}
| _1 = LPAREN _2 = module_expr _3 = COLON _4 = module_type _5 = error
    {let _loc__5_ = (_startpos__5_, _endpos__5_) in
let _loc__1_ = (_startpos__1_, _endpos__1_) in
      ( unclosed "(" _loc__1_ ")" _loc__5_ )}
| _1 = LPAREN me = module_expr _3 = RPAREN
    {      ( me (* TODO consider reloc *) )}
| _1 = LPAREN _2 = module_expr _3 = error
    {let _loc__3_ = (_startpos__3_, _endpos__3_) in
let _loc__1_ = (_startpos__1_, _endpos__1_) in
      ( unclosed "(" _loc__1_ ")" _loc__3_ )}
| _1 = LPAREN _2 = VAL _1_inlined1 = list_attribute_ e = expr _5 = RPAREN
    {let e =       ( e ) in
let attrs =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos__5_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mkmod ~loc:_sloc ~attrs (Pmod_unpack e) )}
| _1 = LPAREN _2 = VAL _1_inlined1 = list_attribute_ e = expr _2_inlined1 = COLON _1_inlined2 = module_type _5 = RPAREN
    {let e =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let ty =
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
          ( let (lid, cstrs, attrs) = package_type_of_module_type _1 in
        let descr = Ptyp_package (lid, cstrs) in
        mktyp ~loc:_sloc ~attrs descr )
  in
  let _endpos_ty_ = _endpos__1_ in
  let _endpos = _endpos_ty_ in
  let _startpos = _startpos_e_ in
  let _loc = (_startpos, _endpos) in
        ( ghexp ~loc:_loc (Pexp_constraint (e, ty)) )
in
let attrs =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos__5_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mkmod ~loc:_sloc ~attrs (Pmod_unpack e) )}
| _1 = LPAREN _2 = VAL _1_inlined1 = list_attribute_ e = expr _2_inlined1 = COLON _1_inlined2 = module_type _4 = COLONGREATER _1_inlined3 = module_type _5 = RPAREN
    {let e =
  let (_endpos__1_inlined1_, _startpos__1_inlined1_, _endpos__1_, _startpos__1_, _1_inlined1, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined3, _1_inlined2) in
  let ty2 =
    let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
          ( let (lid, cstrs, attrs) = package_type_of_module_type _1 in
        let descr = Ptyp_package (lid, cstrs) in
        mktyp ~loc:_sloc ~attrs descr )
  in
  let _endpos_ty2_ = _endpos__1_inlined1_ in
  let ty1 =
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
          ( let (lid, cstrs, attrs) = package_type_of_module_type _1 in
        let descr = Ptyp_package (lid, cstrs) in
        mktyp ~loc:_sloc ~attrs descr )
  in
  let _endpos = _endpos_ty2_ in
  let _startpos = _startpos_e_ in
  let _loc = (_startpos, _endpos) in
        ( ghexp ~loc:_loc (Pexp_coerce (e, (Some ty1), ty2)) )
in
let attrs =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos__5_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mkmod ~loc:_sloc ~attrs (Pmod_unpack e) )}
| _1 = LPAREN _2 = VAL _1_inlined1 = list_attribute_ e = expr _2_inlined1 = COLONGREATER _1_inlined2 = module_type _5 = RPAREN
    {let e =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let ty2 =
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
          ( let (lid, cstrs, attrs) = package_type_of_module_type _1 in
        let descr = Ptyp_package (lid, cstrs) in
        mktyp ~loc:_sloc ~attrs descr )
  in
  let _endpos_ty2_ = _endpos__1_ in
  let _endpos = _endpos_ty2_ in
  let _startpos = _startpos_e_ in
  let _loc = (_startpos, _endpos) in
        ( ghexp ~loc:_loc (Pexp_coerce (e, None, ty2)) )
in
let attrs =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos__5_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mkmod ~loc:_sloc ~attrs (Pmod_unpack e) )}
| _1 = LPAREN _2 = VAL _1_inlined1 = list_attribute_ _4 = expr _5 = COLON _6 = error
    {let _3 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _loc__6_ = (_startpos__6_, _endpos__6_) in
let _loc__1_ = (_startpos__1_, _endpos__1_) in
      ( unclosed "(" _loc__1_ ")" _loc__6_ )}
| _1 = LPAREN _2 = VAL _1_inlined1 = list_attribute_ _4 = expr _5 = COLONGREATER _6 = error
    {let _3 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _loc__6_ = (_startpos__6_, _endpos__6_) in
let _loc__1_ = (_startpos__1_, _endpos__1_) in
      ( unclosed "(" _loc__1_ ")" _loc__6_ )}
| _1 = LPAREN _2 = VAL _1_inlined1 = list_attribute_ _4 = expr _5 = error
    {let _3 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _loc__5_ = (_startpos__5_, _endpos__5_) in
let _loc__1_ = (_startpos__1_, _endpos__1_) in
      ( unclosed "(" _loc__1_ ")" _loc__5_ )}

structure:
  xss = list_structure_element_
    {let _1 =
  let _1 =
    let ys =     ( List.flatten xss ) in
    let xs =
      let items =     ( [] ) in
          ( items )
    in
        ( xs @ ys )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_xss_, _startpos_xss_) in
  let _endpos = _endpos__1_ in
  let _startpos = _startpos__1_ in
                                ( (extra_str _startpos _endpos _1) )
in
  ( _1 )}
| e = seq_expr _1 = list_post_item_attribute_ xss = list_structure_element_
    {let _1 =
  let _1 =
    let ys =     ( List.flatten xss ) in
    let xs =
      let items =
        let x =
          let _1 =
            let _1 =
              let attrs =     ( _1 ) in
              let _endpos_attrs_ = _endpos__1_ in
              let _endpos = _endpos_attrs_ in
              let _symbolstartpos = _startpos_e_ in
              let _sloc = (_symbolstartpos, _endpos) in
                  ( mkstrexp ~loc:_sloc e attrs )
            in
            let _startpos__1_ = _startpos_e_ in
            let _startpos = _startpos__1_ in
              ( text_str _startpos @ [_1] )
          in
          let _startpos__1_ = _startpos_e_ in
          let _endpos = _endpos__1_ in
          let _startpos = _startpos__1_ in
            ( mark_rhs_docs _startpos _endpos;
    _1 )
        in
            ( x )
      in
          ( items )
    in
        ( xs @ ys )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_xss_, _startpos_e_) in
  let _endpos = _endpos__1_ in
  let _startpos = _startpos__1_ in
                                ( (extra_str _startpos _endpos _1) )
in
  ( _1 )}

structure_item:
  _1 = let_bindings_ext_
    {let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( val_of_let_bindings ~loc:_sloc _1 )}
| _1 = item_extension _1_inlined1 = list_post_item_attribute_
    {let _1 =
  let _1 =
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
    let _endpos__2_ = _endpos__1_inlined1_ in
    let _endpos = _endpos__2_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
            ( let docs = symbol_docs _sloc in
          Pstr_extension (_1, add_docs_attrs docs _2) )
  in
  let _endpos__1_ = _endpos__1_inlined1_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkstr ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = floating_attribute
    {let _1 =
  let _1 =         ( Pstr_attribute _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkstr ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = primitive_declaration
    {let _1 =
  let _1 =         ( pstr_primitive _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mkstr_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = value_description
    {let _1 =
  let _1 =         ( pstr_primitive _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mkstr_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = TYPE ext = ext _1_inlined1 = list_attribute_ params = type_parameters _1_inlined2 = LIDENT kind_priv_manifest = type_kind xs = reversed_llist_preceded_CONSTRAINT_constrain__ _1_inlined3 = list_post_item_attribute_ bs = list_generic_and_type_declaration_type_kind__
    {let _1 =
  let _1 =
    let _1 =
      let _1 =
        let _1 =
          let a =
            let attrs2 =
              let _1 = _1_inlined3 in
                  ( _1 )
            in
            let _endpos_attrs2_ = _endpos__1_inlined3_ in
            let cstrs =
              let _1 =
                let xs =     ( List.rev xs ) in
                    ( xs )
              in
                  ( _1 )
            in
            let id =
              let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
              let _endpos = _endpos__1_ in
              let _symbolstartpos = _startpos__1_ in
              let _sloc = (_symbolstartpos, _endpos) in
                  ( mkrhs _1 _sloc )
            in
            let flag =                                                 ( Recursive ) in
            let attrs1 =
              let _1 = _1_inlined1 in
                  ( _1 )
            in
            let _endpos = _endpos_attrs2_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
                (
      let (kind, priv, manifest) = kind_priv_manifest in
      let docs = symbol_docs _sloc in
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc _sloc in
      (flag, ext),
      Type.mk id ~params ~cstrs ~kind ~priv ~manifest ~attrs ~loc ~docs
    )
          in
              ( let (x, b) = a in x, b :: bs )
        in
          ( _1 )
      in
          ( _1 )
    in
            ( pstr_type _1 )
  in
  let _endpos__1_ = _endpos_bs_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mkstr_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = TYPE ext = ext _1_inlined1 = list_attribute_ _1_inlined2 = NONREC params = type_parameters _1_inlined3 = LIDENT kind_priv_manifest = type_kind xs = reversed_llist_preceded_CONSTRAINT_constrain__ _1_inlined4 = list_post_item_attribute_ bs = list_generic_and_type_declaration_type_kind__
    {let _1 =
  let _1 =
    let _1 =
      let _1 =
        let _1 =
          let a =
            let attrs2 =
              let _1 = _1_inlined4 in
                  ( _1 )
            in
            let _endpos_attrs2_ = _endpos__1_inlined4_ in
            let cstrs =
              let _1 =
                let xs =     ( List.rev xs ) in
                    ( xs )
              in
                  ( _1 )
            in
            let id =
              let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
              let _endpos = _endpos__1_ in
              let _symbolstartpos = _startpos__1_ in
              let _sloc = (_symbolstartpos, _endpos) in
                  ( mkrhs _1 _sloc )
            in
            let flag =                                                 ( Nonrecursive ) in
            let attrs1 =
              let _1 = _1_inlined1 in
                  ( _1 )
            in
            let _endpos = _endpos_attrs2_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
                (
      let (kind, priv, manifest) = kind_priv_manifest in
      let docs = symbol_docs _sloc in
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc _sloc in
      (flag, ext),
      Type.mk id ~params ~cstrs ~kind ~priv ~manifest ~attrs ~loc ~docs
    )
          in
              ( let (x, b) = a in x, b :: bs )
        in
          ( _1 )
      in
          ( _1 )
    in
            ( pstr_type _1 )
  in
  let _endpos__1_ = _endpos_bs_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mkstr_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = TYPE ext = ext _1_inlined1 = list_attribute_ params = type_parameters _1_inlined2 = type_longident _7 = PLUSEQ priv = private_flag xs = reversed_bar_llist_extension_constructor_ _1_inlined3 = list_post_item_attribute_
    {let _1 =
  let _1 =
    let _1 =
      let _1 =
        let attrs2 =
          let _1 = _1_inlined3 in
              ( _1 )
        in
        let _endpos_attrs2_ = _endpos__1_inlined3_ in
        let cs =     ( List.rev xs ) in
        let tid =
          let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
          let _endpos = _endpos__1_ in
          let _symbolstartpos = _startpos__1_ in
          let _sloc = (_symbolstartpos, _endpos) in
              ( mkrhs _1 _sloc )
        in
        let _4 =                 ( Recursive ) in
        let attrs1 =
          let _1 = _1_inlined1 in
              ( _1 )
        in
        let _endpos = _endpos_attrs2_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( let docs = symbol_docs _sloc in
      let attrs = attrs1 @ attrs2 in
      Te.mk tid cs ~params ~priv ~attrs ~docs,
      ext )
      in
          ( _1 )
    in
            ( pstr_typext _1 )
  in
  let _endpos__1_ = _endpos__1_inlined3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mkstr_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = TYPE ext = ext _1_inlined1 = list_attribute_ _1_inlined2 = NONREC params = type_parameters _1_inlined3 = type_longident _7 = PLUSEQ priv = private_flag xs = reversed_bar_llist_extension_constructor_ _1_inlined4 = list_post_item_attribute_
    {let _1 =
  let _1 =
    let _1 =
      let _1 =
        let attrs2 =
          let _1 = _1_inlined4 in
              ( _1 )
        in
        let _endpos_attrs2_ = _endpos__1_inlined4_ in
        let cs =     ( List.rev xs ) in
        let tid =
          let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
          let _endpos = _endpos__1_ in
          let _symbolstartpos = _startpos__1_ in
          let _sloc = (_symbolstartpos, _endpos) in
              ( mkrhs _1 _sloc )
        in
        let _4 =
          let (_endpos__1_, _startpos__1_) = (_endpos__1_inlined2_, _startpos__1_inlined2_) in
          let _endpos = _endpos__1_ in
          let _startpos = _startpos__1_ in
          let _loc = (_startpos, _endpos) in
                          ( not_expecting _loc "nonrec flag" )
        in
        let attrs1 =
          let _1 = _1_inlined1 in
              ( _1 )
        in
        let _endpos = _endpos_attrs2_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( let docs = symbol_docs _sloc in
      let attrs = attrs1 @ attrs2 in
      Te.mk tid cs ~params ~priv ~attrs ~docs,
      ext )
      in
          ( _1 )
    in
            ( pstr_typext _1 )
  in
  let _endpos__1_ = _endpos__1_inlined4_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mkstr_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = str_exception_declaration
    {let _1 =
  let _1 =         ( pstr_exception _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mkstr_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = MODULE ext = ext _1_inlined1 = list_attribute_ _1_inlined2 = module_name body = module_binding_body _1_inlined3 = list_post_item_attribute_
    {let _1 =
  let _1 =
    let _1 =
      let attrs2 =
        let _1 = _1_inlined3 in
            ( _1 )
      in
      let _endpos_attrs2_ = _endpos__1_inlined3_ in
      let name =
        let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
        let _endpos = _endpos__1_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( mkrhs _1 _sloc )
      in
      let attrs1 =
        let _1 = _1_inlined1 in
            ( _1 )
      in
      let _endpos = _endpos_attrs2_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( let docs = symbol_docs _sloc in
      let loc = make_loc _sloc in
      let attrs = attrs1 @ attrs2 in
      let body = Mb.mk name body ~attrs ~loc ~docs in
      Pstr_module body, ext )
    in
            ( _1 )
  in
  let _endpos__1_ = _endpos__1_inlined3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mkstr_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = MODULE ext = ext _1_inlined1 = list_attribute_ _4 = REC _1_inlined2 = module_name body = module_binding_body _1_inlined3 = list_post_item_attribute_ bs = list_and_module_binding_
    {let _1 =
  let _1 =
    let _1 =
      let _1 =
        let a =
          let attrs2 =
            let _1 = _1_inlined3 in
                ( _1 )
          in
          let _endpos_attrs2_ = _endpos__1_inlined3_ in
          let name =
            let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
            let _endpos = _endpos__1_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
                ( mkrhs _1 _sloc )
          in
          let attrs1 =
            let _1 = _1_inlined1 in
                ( _1 )
          in
          let _endpos = _endpos_attrs2_ in
          let _symbolstartpos = _startpos__1_ in
          let _sloc = (_symbolstartpos, _endpos) in
            (
    let loc = make_loc _sloc in
    let attrs = attrs1 @ attrs2 in
    let docs = symbol_docs _sloc in
    ext,
    Mb.mk name body ~attrs ~loc ~docs
  )
        in
            ( let (x, b) = a in x, b :: bs )
      in
          ( let (a,b) = _1 in a, b )
    in
            ( pstr_recmodule _1 )
  in
  let _endpos__1_ = _endpos_bs_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mkstr_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = module_type_declaration
    {let _1 =
  let _1 =         ( let (body, ext) = _1 in (Pstr_modtype body, ext) ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mkstr_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = open_declaration
    {let _1 =
  let _1 =         ( let (body, ext) = _1 in (Pstr_open body, ext) ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mkstr_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = CLASS ext = ext _1_inlined1 = list_attribute_ virt = virtual_flag params = formal_class_parameters _1_inlined2 = LIDENT body = class_fun_binding _1_inlined3 = list_post_item_attribute_ bs = list_and_class_declaration_
    {let _1 =
  let _1 =
    let _1 =
      let _1 =
        let a =
          let attrs2 =
            let _1 = _1_inlined3 in
                ( _1 )
          in
          let _endpos_attrs2_ = _endpos__1_inlined3_ in
          let id =
            let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
            let _endpos = _endpos__1_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
                ( mkrhs _1 _sloc )
          in
          let attrs1 =
            let _1 = _1_inlined1 in
                ( _1 )
          in
          let _endpos = _endpos_attrs2_ in
          let _symbolstartpos = _startpos__1_ in
          let _sloc = (_symbolstartpos, _endpos) in
            (
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc _sloc in
    let docs = symbol_docs _sloc in
    ext,
    Ci.mk id body ~virt ~params ~attrs ~loc ~docs
  )
        in
            ( let (x, b) = a in x, b :: bs )
      in
          ( let (a,b) = _1 in a, b )
    in
            ( let (ext, l) = _1 in (Pstr_class l, ext) )
  in
  let _endpos__1_ = _endpos_bs_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mkstr_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = class_type_declarations
    {let _1 =
  let _1 =         ( let (ext, l) = _1 in (Pstr_class_type l, ext) ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mkstr_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = INCLUDE ext = ext _1_inlined1 = list_attribute_ thing = module_expr _1_inlined2 = list_post_item_attribute_
    {let _1 =
  let _1 =
    let _1 =
      let attrs2 =
        let _1 = _1_inlined2 in
            ( _1 )
      in
      let _endpos_attrs2_ = _endpos__1_inlined2_ in
      let attrs1 =
        let _1 = _1_inlined1 in
            ( _1 )
      in
      let _endpos = _endpos_attrs2_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
        (
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc _sloc in
    let docs = symbol_docs _sloc in
    Incl.mk thing ~attrs ~loc ~docs, ext
  )
    in
            ( pstr_include _1 )
  in
  let _endpos__1_ = _endpos__1_inlined2_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mkstr_ext ~loc:_sloc _1 )
in
    ( _1 )}

module_binding_body:
  _1 = EQUAL me = module_expr
    {      ( me )}
| _1 = COLON mty = module_type _3 = EQUAL me = module_expr
    {let _1 =
  let _1 =         ( Pmod_constraint(me, mty) ) in
  let _endpos__1_ = _endpos_me_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkmod ~loc:_sloc _1 )
in
    ( _1 )}
| arg_and_pos = functor_arg body = module_binding_body
    {let _1 =
  let _1 =         ( let (_, arg) = arg_and_pos in
          Pmod_functor(arg, body) ) in
  let (_endpos__1_, _startpos__1_) = (_endpos_body_, _startpos_arg_and_pos_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkmod ~loc:_sloc _1 )
in
    ( _1 )}

module_type_declaration:
  _1 = MODULE _2 = TYPE ext = ext _1_inlined1 = list_attribute_ _1_inlined2 = ident typ = option_preceded_EQUAL_module_type__ _1_inlined3 = list_post_item_attribute_
    {let attrs2 =
  let _1 = _1_inlined3 in
      ( _1 )
in
let _endpos_attrs2_ = _endpos__1_inlined3_ in
let id =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let attrs1 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos_attrs2_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
  (
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc _sloc in
    let docs = symbol_docs _sloc in
    Mtd.mk id ~typ:(typ) ~attrs ~loc ~docs, ext
  )}

open_declaration:
  _1 = OPEN ext = ext _1_inlined1 = list_attribute_ me = module_expr _1_inlined2 = list_post_item_attribute_
    {let attrs2 =
  let _1 = _1_inlined2 in
      ( _1 )
in
let _endpos_attrs2_ = _endpos__1_inlined2_ in
let attrs1 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let override =                                                 ( Fresh ) in
let _endpos = _endpos_attrs2_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
  (
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc _sloc in
    let docs = symbol_docs _sloc in
    Opn.mk me ~override ~attrs ~loc ~docs, ext
  )}
| _1 = OPEN _1_inlined1 = BANG ext = ext _1_inlined2 = list_attribute_ me = module_expr _1_inlined3 = list_post_item_attribute_
    {let attrs2 =
  let _1 = _1_inlined3 in
      ( _1 )
in
let _endpos_attrs2_ = _endpos__1_inlined3_ in
let attrs1 =
  let _1 = _1_inlined2 in
      ( _1 )
in
let override =                                                 ( Override ) in
let _endpos = _endpos_attrs2_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
  (
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc _sloc in
    let docs = symbol_docs _sloc in
    Opn.mk me ~override ~attrs ~loc ~docs, ext
  )}

open_description:
  _1 = OPEN ext = ext _1_inlined1 = list_attribute_ _1_inlined2 = mod_ext_longident _1_inlined3 = list_post_item_attribute_
    {let attrs2 =
  let _1 = _1_inlined3 in
      ( _1 )
in
let _endpos_attrs2_ = _endpos__1_inlined3_ in
let id =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let attrs1 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let override =                                                 ( Fresh ) in
let _endpos = _endpos_attrs2_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
  (
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc _sloc in
    let docs = symbol_docs _sloc in
    Opn.mk id ~override ~attrs ~loc ~docs, ext
  )}
| _1 = OPEN _1_inlined1 = BANG ext = ext _1_inlined2 = list_attribute_ _1_inlined3 = mod_ext_longident _1_inlined4 = list_post_item_attribute_
    {let attrs2 =
  let _1 = _1_inlined4 in
      ( _1 )
in
let _endpos_attrs2_ = _endpos__1_inlined4_ in
let id =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let attrs1 =
  let _1 = _1_inlined2 in
      ( _1 )
in
let override =                                                 ( Override ) in
let _endpos = _endpos_attrs2_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
  (
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc _sloc in
    let docs = symbol_docs _sloc in
    Opn.mk id ~override ~attrs ~loc ~docs, ext
  )}

module_type:
  _1 = SIG _1_inlined1 = list_attribute_ s = signature _4 = END
    {let attrs =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos__4_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mkmty ~loc:_sloc ~attrs (Pmty_signature s) )}
| _1 = SIG _1_inlined1 = list_attribute_ _3 = signature _4 = error
    {let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _loc__4_ = (_startpos__4_, _endpos__4_) in
let _loc__1_ = (_startpos__1_, _endpos__1_) in
      ( unclosed "sig" _loc__1_ "end" _loc__4_ )}
| _1 = FUNCTOR _1_inlined1 = list_attribute_ _1_inlined2 = reversed_nonempty_llist_functor_arg_ _4 = MINUSGREATER mty = module_type %prec below_WITH
    {let args =
  let _1 = _1_inlined2 in
      ( _1 )
in
let attrs =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos_mty_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mty_attrs ~loc:_sloc attrs (
          List.fold_left (fun acc (startpos, arg) ->
            mkmty ~loc:(startpos, _endpos) (Pmty_functor (arg, acc))
          ) mty args
        ) )}
| _1 = MODULE _2 = TYPE _3 = OF _1_inlined1 = list_attribute_ _5 = module_expr %prec below_LBRACKETAT
    {let _4 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos__5_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mkmty ~loc:_sloc ~attrs:_4 (Pmty_typeof _5) )}
| _1 = LPAREN _2 = module_type _3 = RPAREN
    {      ( _2 )}
| _1 = LPAREN _2 = module_type _3 = error
    {let _loc__3_ = (_startpos__3_, _endpos__3_) in
let _loc__1_ = (_startpos__1_, _endpos__1_) in
      ( unclosed "(" _loc__1_ ")" _loc__3_ )}
| _1 = module_type _2 = attribute
    {      ( Mty.attr _1 _2 )}
| _1 = mty_longident
    {let _1 =
  let _1 =
    let _1 =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
            ( Pmty_ident _1 )
  in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkmty ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = module_type _2 = MINUSGREATER _3 = module_type %prec below_WITH
    {let _1 =
  let _1 =         ( Pmty_functor((Named (mknoloc (None), _1)), _3) ) in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkmty ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = module_type _2 = WITH xs = reversed_separated_nonempty_llist_AND_with_constraint_
    {let _1 =
  let _1 =
    let _3 =
      let xs =     ( List.rev xs ) in
          ( xs )
    in
            ( Pmty_with(_1, _3) )
  in
  let _endpos__1_ = _endpos_xs_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkmty ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = LPAREN _2 = MODULE _1_inlined1 = mod_longident _4 = RPAREN
    {let _1 =
  let _1 =
    let _3 =
      let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
            ( Pmty_alias _3 )
  in
  let _endpos__1_ = _endpos__4_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkmty ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = extension
    {let _1 =
  let _1 =         ( Pmty_extension _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkmty ~loc:_sloc _1 )
in
    ( _1 )}

signature:
  xss = list_signature_element_
    {let _1 =
  let _1 =     ( List.flatten xss ) in
  let (_endpos__1_, _startpos__1_) = (_endpos_xss_, _startpos_xss_) in
  let _endpos = _endpos__1_ in
  let _startpos = _startpos__1_ in
                                ( (extra_sig _startpos _endpos _1) )
in
    ( _1 )}

signature_item:
  _1 = item_extension _1_inlined1 = list_post_item_attribute_
    {let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos__2_ = _endpos__1_inlined1_ in
let _endpos = _endpos__2_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let docs = symbol_docs _sloc in
        mksig ~loc:_sloc (Psig_extension (_1, (add_docs_attrs docs _2))) )}
| _1 = floating_attribute
    {let _1 =
  let _1 =         ( Psig_attribute _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mksig ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = value_description
    {let _1 =
  let _1 =         ( psig_value _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = primitive_declaration
    {let _1 =
  let _1 =         ( psig_value _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = TYPE ext = ext _1_inlined1 = list_attribute_ params = type_parameters _1_inlined2 = LIDENT kind_priv_manifest = type_kind xs = reversed_llist_preceded_CONSTRAINT_constrain__ _1_inlined3 = list_post_item_attribute_ bs = list_generic_and_type_declaration_type_kind__
    {let _1 =
  let _1 =
    let _1 =
      let _1 =
        let _1 =
          let a =
            let attrs2 =
              let _1 = _1_inlined3 in
                  ( _1 )
            in
            let _endpos_attrs2_ = _endpos__1_inlined3_ in
            let cstrs =
              let _1 =
                let xs =     ( List.rev xs ) in
                    ( xs )
              in
                  ( _1 )
            in
            let id =
              let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
              let _endpos = _endpos__1_ in
              let _symbolstartpos = _startpos__1_ in
              let _sloc = (_symbolstartpos, _endpos) in
                  ( mkrhs _1 _sloc )
            in
            let flag =                                                 ( Recursive ) in
            let attrs1 =
              let _1 = _1_inlined1 in
                  ( _1 )
            in
            let _endpos = _endpos_attrs2_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
                (
      let (kind, priv, manifest) = kind_priv_manifest in
      let docs = symbol_docs _sloc in
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc _sloc in
      (flag, ext),
      Type.mk id ~params ~cstrs ~kind ~priv ~manifest ~attrs ~loc ~docs
    )
          in
              ( let (x, b) = a in x, b :: bs )
        in
          ( _1 )
      in
          ( _1 )
    in
            ( psig_type _1 )
  in
  let _endpos__1_ = _endpos_bs_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = TYPE ext = ext _1_inlined1 = list_attribute_ _1_inlined2 = NONREC params = type_parameters _1_inlined3 = LIDENT kind_priv_manifest = type_kind xs = reversed_llist_preceded_CONSTRAINT_constrain__ _1_inlined4 = list_post_item_attribute_ bs = list_generic_and_type_declaration_type_kind__
    {let _1 =
  let _1 =
    let _1 =
      let _1 =
        let _1 =
          let a =
            let attrs2 =
              let _1 = _1_inlined4 in
                  ( _1 )
            in
            let _endpos_attrs2_ = _endpos__1_inlined4_ in
            let cstrs =
              let _1 =
                let xs =     ( List.rev xs ) in
                    ( xs )
              in
                  ( _1 )
            in
            let id =
              let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
              let _endpos = _endpos__1_ in
              let _symbolstartpos = _startpos__1_ in
              let _sloc = (_symbolstartpos, _endpos) in
                  ( mkrhs _1 _sloc )
            in
            let flag =                                                 ( Nonrecursive ) in
            let attrs1 =
              let _1 = _1_inlined1 in
                  ( _1 )
            in
            let _endpos = _endpos_attrs2_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
                (
      let (kind, priv, manifest) = kind_priv_manifest in
      let docs = symbol_docs _sloc in
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc _sloc in
      (flag, ext),
      Type.mk id ~params ~cstrs ~kind ~priv ~manifest ~attrs ~loc ~docs
    )
          in
              ( let (x, b) = a in x, b :: bs )
        in
          ( _1 )
      in
          ( _1 )
    in
            ( psig_type _1 )
  in
  let _endpos__1_ = _endpos_bs_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = TYPE ext = ext _1_inlined1 = list_attribute_ params = type_parameters _1_inlined2 = LIDENT _1_inlined3 = COLONEQUAL _2 = nonempty_type_kind xs = reversed_llist_preceded_CONSTRAINT_constrain__ _1_inlined4 = list_post_item_attribute_ bs = list_generic_and_type_declaration_type_subst_kind__
    {let _1 =
  let _1 =
    let _1 =
      let _1 =
        let _1 =
          let a =
            let attrs2 =
              let _1 = _1_inlined4 in
                  ( _1 )
            in
            let _endpos_attrs2_ = _endpos__1_inlined4_ in
            let cstrs =
              let _1 =
                let xs =     ( List.rev xs ) in
                    ( xs )
              in
                  ( _1 )
            in
            let kind_priv_manifest =       ( _2 ) in
            let id =
              let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
              let _endpos = _endpos__1_ in
              let _symbolstartpos = _startpos__1_ in
              let _sloc = (_symbolstartpos, _endpos) in
                  ( mkrhs _1 _sloc )
            in
            let flag =                 ( Recursive ) in
            let attrs1 =
              let _1 = _1_inlined1 in
                  ( _1 )
            in
            let _endpos = _endpos_attrs2_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
                (
      let (kind, priv, manifest) = kind_priv_manifest in
      let docs = symbol_docs _sloc in
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc _sloc in
      (flag, ext),
      Type.mk id ~params ~cstrs ~kind ~priv ~manifest ~attrs ~loc ~docs
    )
          in
              ( let (x, b) = a in x, b :: bs )
        in
          ( _1 )
      in
          ( _1 )
    in
            ( psig_typesubst _1 )
  in
  let _endpos__1_ = _endpos_bs_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = TYPE ext = ext _1_inlined1 = list_attribute_ _1_inlined2 = NONREC params = type_parameters _1_inlined3 = LIDENT _1_inlined4 = COLONEQUAL _2 = nonempty_type_kind xs = reversed_llist_preceded_CONSTRAINT_constrain__ _1_inlined5 = list_post_item_attribute_ bs = list_generic_and_type_declaration_type_subst_kind__
    {let _1 =
  let _1 =
    let _1 =
      let _1 =
        let _1 =
          let a =
            let attrs2 =
              let _1 = _1_inlined5 in
                  ( _1 )
            in
            let _endpos_attrs2_ = _endpos__1_inlined5_ in
            let cstrs =
              let _1 =
                let xs =     ( List.rev xs ) in
                    ( xs )
              in
                  ( _1 )
            in
            let kind_priv_manifest =       ( _2 ) in
            let id =
              let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
              let _endpos = _endpos__1_ in
              let _symbolstartpos = _startpos__1_ in
              let _sloc = (_symbolstartpos, _endpos) in
                  ( mkrhs _1 _sloc )
            in
            let flag =
              let (_endpos__1_, _startpos__1_) = (_endpos__1_inlined2_, _startpos__1_inlined2_) in
              let _endpos = _endpos__1_ in
              let _startpos = _startpos__1_ in
              let _loc = (_startpos, _endpos) in
                              ( not_expecting _loc "nonrec flag" )
            in
            let attrs1 =
              let _1 = _1_inlined1 in
                  ( _1 )
            in
            let _endpos = _endpos_attrs2_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
                (
      let (kind, priv, manifest) = kind_priv_manifest in
      let docs = symbol_docs _sloc in
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc _sloc in
      (flag, ext),
      Type.mk id ~params ~cstrs ~kind ~priv ~manifest ~attrs ~loc ~docs
    )
          in
              ( let (x, b) = a in x, b :: bs )
        in
          ( _1 )
      in
          ( _1 )
    in
            ( psig_typesubst _1 )
  in
  let _endpos__1_ = _endpos_bs_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = TYPE ext = ext _1_inlined1 = list_attribute_ params = type_parameters _1_inlined2 = type_longident _7 = PLUSEQ priv = private_flag xs = reversed_bar_llist_extension_constructor_declaration_ _1_inlined3 = list_post_item_attribute_
    {let _1 =
  let _1 =
    let _1 =
      let _1 =
        let attrs2 =
          let _1 = _1_inlined3 in
              ( _1 )
        in
        let _endpos_attrs2_ = _endpos__1_inlined3_ in
        let cs =     ( List.rev xs ) in
        let tid =
          let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
          let _endpos = _endpos__1_ in
          let _symbolstartpos = _startpos__1_ in
          let _sloc = (_symbolstartpos, _endpos) in
              ( mkrhs _1 _sloc )
        in
        let _4 =                 ( Recursive ) in
        let attrs1 =
          let _1 = _1_inlined1 in
              ( _1 )
        in
        let _endpos = _endpos_attrs2_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( let docs = symbol_docs _sloc in
      let attrs = attrs1 @ attrs2 in
      Te.mk tid cs ~params ~priv ~attrs ~docs,
      ext )
      in
          ( _1 )
    in
            ( psig_typext _1 )
  in
  let _endpos__1_ = _endpos__1_inlined3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = TYPE ext = ext _1_inlined1 = list_attribute_ _1_inlined2 = NONREC params = type_parameters _1_inlined3 = type_longident _7 = PLUSEQ priv = private_flag xs = reversed_bar_llist_extension_constructor_declaration_ _1_inlined4 = list_post_item_attribute_
    {let _1 =
  let _1 =
    let _1 =
      let _1 =
        let attrs2 =
          let _1 = _1_inlined4 in
              ( _1 )
        in
        let _endpos_attrs2_ = _endpos__1_inlined4_ in
        let cs =     ( List.rev xs ) in
        let tid =
          let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
          let _endpos = _endpos__1_ in
          let _symbolstartpos = _startpos__1_ in
          let _sloc = (_symbolstartpos, _endpos) in
              ( mkrhs _1 _sloc )
        in
        let _4 =
          let (_endpos__1_, _startpos__1_) = (_endpos__1_inlined2_, _startpos__1_inlined2_) in
          let _endpos = _endpos__1_ in
          let _startpos = _startpos__1_ in
          let _loc = (_startpos, _endpos) in
                          ( not_expecting _loc "nonrec flag" )
        in
        let attrs1 =
          let _1 = _1_inlined1 in
              ( _1 )
        in
        let _endpos = _endpos_attrs2_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( let docs = symbol_docs _sloc in
      let attrs = attrs1 @ attrs2 in
      Te.mk tid cs ~params ~priv ~attrs ~docs,
      ext )
      in
          ( _1 )
    in
            ( psig_typext _1 )
  in
  let _endpos__1_ = _endpos__1_inlined4_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = sig_exception_declaration
    {let _1 =
  let _1 =         ( psig_exception _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = MODULE ext = ext _1_inlined1 = list_attribute_ _1_inlined2 = module_name body = module_declaration_body _1_inlined3 = list_post_item_attribute_
    {let _1 =
  let _1 =
    let _1 =
      let attrs2 =
        let _1 = _1_inlined3 in
            ( _1 )
      in
      let _endpos_attrs2_ = _endpos__1_inlined3_ in
      let name =
        let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
        let _endpos = _endpos__1_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( mkrhs _1 _sloc )
      in
      let attrs1 =
        let _1 = _1_inlined1 in
            ( _1 )
      in
      let _endpos = _endpos_attrs2_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
        (
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc _sloc in
    let docs = symbol_docs _sloc in
    Md.mk name body ~attrs ~loc ~docs, ext
  )
    in
            ( let (body, ext) = _1 in (Psig_module body, ext) )
  in
  let _endpos__1_ = _endpos__1_inlined3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = MODULE ext = ext _1_inlined1 = list_attribute_ _1_inlined2 = module_name _5 = EQUAL _1_inlined3 = mod_longident _1_inlined4 = list_post_item_attribute_
    {let _1 =
  let _1 =
    let _1 =
      let attrs2 =
        let _1 = _1_inlined4 in
            ( _1 )
      in
      let _endpos_attrs2_ = _endpos__1_inlined4_ in
      let body =
        let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
        let id =
          let _endpos = _endpos__1_ in
          let _symbolstartpos = _startpos__1_ in
          let _sloc = (_symbolstartpos, _endpos) in
              ( mkrhs _1 _sloc )
        in
        let (_endpos_id_, _startpos_id_) = (_endpos__1_, _startpos__1_) in
        let _endpos = _endpos_id_ in
        let _symbolstartpos = _startpos_id_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( Mty.alias ~loc:(make_loc _sloc) id )
      in
      let name =
        let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
        let _endpos = _endpos__1_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( mkrhs _1 _sloc )
      in
      let attrs1 =
        let _1 = _1_inlined1 in
            ( _1 )
      in
      let _endpos = _endpos_attrs2_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
        (
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc _sloc in
    let docs = symbol_docs _sloc in
    Md.mk name body ~attrs ~loc ~docs, ext
  )
    in
            ( let (body, ext) = _1 in (Psig_module body, ext) )
  in
  let _endpos__1_ = _endpos__1_inlined4_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = module_subst
    {let _1 =
  let _1 =         ( let (body, ext) = _1 in (Psig_modsubst body, ext) ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = MODULE ext = ext _1_inlined1 = list_attribute_ _4 = REC _1_inlined2 = module_name _6 = COLON mty = module_type _1_inlined3 = list_post_item_attribute_ bs = list_and_module_declaration_
    {let _1 =
  let _1 =
    let _1 =
      let _1 =
        let a =
          let attrs2 =
            let _1 = _1_inlined3 in
                ( _1 )
          in
          let _endpos_attrs2_ = _endpos__1_inlined3_ in
          let name =
            let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
            let _endpos = _endpos__1_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
                ( mkrhs _1 _sloc )
          in
          let attrs1 =
            let _1 = _1_inlined1 in
                ( _1 )
          in
          let _endpos = _endpos_attrs2_ in
          let _symbolstartpos = _startpos__1_ in
          let _sloc = (_symbolstartpos, _endpos) in
            (
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc _sloc in
    let docs = symbol_docs _sloc in
    ext, Md.mk name mty ~attrs ~loc ~docs
  )
        in
            ( let (x, b) = a in x, b :: bs )
      in
          ( let a,b = _1 in a, b )
    in
            ( let (ext, l) = _1 in (Psig_recmodule l, ext) )
  in
  let _endpos__1_ = _endpos_bs_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = module_type_declaration
    {let _1 =
  let _1 =         ( let (body, ext) = _1 in (Psig_modtype body, ext) ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = module_type_subst
    {let _1 =
  let _1 =         ( let (body, ext) = _1 in (Psig_modtypesubst body, ext) ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = open_description
    {let _1 =
  let _1 =         ( let (body, ext) = _1 in (Psig_open body, ext) ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = INCLUDE ext = ext _1_inlined1 = list_attribute_ thing = module_type _1_inlined2 = list_post_item_attribute_
    {let _1 =
  let _1 =
    let _1 =
      let attrs2 =
        let _1 = _1_inlined2 in
            ( _1 )
      in
      let _endpos_attrs2_ = _endpos__1_inlined2_ in
      let attrs1 =
        let _1 = _1_inlined1 in
            ( _1 )
      in
      let _endpos = _endpos_attrs2_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
        (
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc _sloc in
    let docs = symbol_docs _sloc in
    Incl.mk thing ~attrs ~loc ~docs, ext
  )
    in
            ( psig_include _1 )
  in
  let _endpos__1_ = _endpos__1_inlined2_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = CLASS ext = ext _1_inlined1 = list_attribute_ virt = virtual_flag params = formal_class_parameters _1_inlined2 = LIDENT _7 = COLON cty = class_type _1_inlined3 = list_post_item_attribute_ bs = list_and_class_description_
    {let _1 =
  let _1 =
    let _1 =
      let _1 =
        let a =
          let attrs2 =
            let _1 = _1_inlined3 in
                ( _1 )
          in
          let _endpos_attrs2_ = _endpos__1_inlined3_ in
          let id =
            let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
            let _endpos = _endpos__1_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
                ( mkrhs _1 _sloc )
          in
          let attrs1 =
            let _1 = _1_inlined1 in
                ( _1 )
          in
          let _endpos = _endpos_attrs2_ in
          let _symbolstartpos = _startpos__1_ in
          let _sloc = (_symbolstartpos, _endpos) in
              (
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc _sloc in
      let docs = symbol_docs _sloc in
      ext,
      Ci.mk id cty ~virt ~params ~attrs ~loc ~docs
    )
        in
            ( let (x, b) = a in x, b :: bs )
      in
          ( let (a,b) = _1 in a, b )
    in
            ( let (ext, l) = _1 in (Psig_class l, ext) )
  in
  let _endpos__1_ = _endpos_bs_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = class_type_declarations
    {let _1 =
  let _1 =         ( let (ext, l) = _1 in (Psig_class_type l, ext) ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_mksig_ext ~loc:_sloc _1 )
in
    ( _1 )}

module_declaration_body:
  _1 = COLON mty = module_type
    {      ( mty )}
| arg_and_pos = functor_arg body = module_declaration_body
    {let _1 =
  let _1 =         ( let (_, arg) = arg_and_pos in
          Pmty_functor(arg, body) ) in
  let (_endpos__1_, _startpos__1_) = (_endpos_body_, _startpos_arg_and_pos_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkmty ~loc:_sloc _1 )
in
    ( _1 )}

module_subst:
  _1 = MODULE ext = ext _1_inlined1 = list_attribute_ _1_inlined2 = UIDENT _5 = COLONEQUAL _1_inlined3 = mod_ext_longident _1_inlined4 = list_post_item_attribute_
    {let attrs2 =
  let _1 = _1_inlined4 in
      ( _1 )
in
let _endpos_attrs2_ = _endpos__1_inlined4_ in
let body =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let uid =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let attrs1 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos_attrs2_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
  (
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc _sloc in
    let docs = symbol_docs _sloc in
    Ms.mk uid body ~attrs ~loc ~docs, ext
  )}
| _1 = MODULE _2 = ext _1_inlined1 = list_attribute_ _1_inlined2 = UIDENT _5 = COLONEQUAL _6 = error
    {let _4 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _3 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _loc__6_ = (_startpos__6_, _endpos__6_) in
    ( expecting _loc__6_ "module path" )}

module_type_subst:
  _1 = MODULE _2 = TYPE ext = ext _1_inlined1 = list_attribute_ _1_inlined2 = ident _6 = COLONEQUAL typ = module_type _1_inlined3 = list_post_item_attribute_
    {let attrs2 =
  let _1 = _1_inlined3 in
      ( _1 )
in
let _endpos_attrs2_ = _endpos__1_inlined3_ in
let id =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let attrs1 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos_attrs2_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
  (
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc _sloc in
    let docs = symbol_docs _sloc in
    Mtd.mk id ~typ:((Some typ)) ~attrs ~loc ~docs, ext
  )}

class_fun_binding:
  _1 = EQUAL _2 = class_expr
    {      ( _2 )}
| _1 = COLON _2 = class_type _3 = EQUAL _4 = class_expr
    {let _1 =
  let _1 =         ( Pcl_constraint(_4, _2) ) in
  let _endpos__1_ = _endpos__4_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkclass ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = labeled_simple_pattern _2 = class_fun_binding
    {let _1 =
  let _1 =       ( let (l,o,p) = _1 in Pcl_fun(l, o, p, _2) ) in
  let _endpos__1_ = _endpos__2_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkclass ~loc:_sloc _1 )
in
      ( _1 )}

formal_class_parameters:
  
    {let params =       ( [] ) in
    ( params )}
| _1 = LBRACKET xs = reversed_separated_nonempty_llist_COMMA_type_parameter_ _3 = RBRACKET
    {let params =
  let params =
    let xs =     ( List.rev xs ) in
        ( xs )
  in
        ( params )
in
    ( params )}

class_expr:
  _1 = class_simple_expr
    {      ( _1 )}
| _1 = FUN _1_inlined1 = list_attribute_ _3 = class_fun_def
    {let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos__3_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_class_attrs ~loc:_sloc _3 _2 )}
| _1 = let_bindings_no_ext_ _2 = IN _3 = class_expr
    {let _endpos = _endpos__3_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( class_of_let_bindings ~loc:_sloc _1 _3 )}
| _1 = LET _2 = OPEN _1_inlined1 = list_attribute_ _1_inlined2 = mod_longident _6 = IN _7 = class_expr
    {let _5 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos__5_ = _endpos__1_inlined2_ in
let _4 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _3 =                                                 ( Fresh ) in
let _endpos = _endpos__7_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let loc = (_startpos__2_, _endpos__5_) in
        let od = Opn.mk ~override:_3 ~loc:(make_loc loc) _5 in
        mkclass ~loc:_sloc ~attrs:_4 (Pcl_open(od, _7)) )}
| _1 = LET _2 = OPEN _1_inlined1 = BANG _1_inlined2 = list_attribute_ _1_inlined3 = mod_longident _6 = IN _7 = class_expr
    {let _5 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos__5_ = _endpos__1_inlined3_ in
let _4 =
  let _1 = _1_inlined2 in
      ( _1 )
in
let _3 =                                                 ( Override ) in
let _endpos = _endpos__7_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let loc = (_startpos__2_, _endpos__5_) in
        let od = Opn.mk ~override:_3 ~loc:(make_loc loc) _5 in
        mkclass ~loc:_sloc ~attrs:_4 (Pcl_open(od, _7)) )}
| _1 = class_expr _2 = attribute
    {      ( Cl.attr _1 _2 )}
| _1 = class_simple_expr xs = reversed_nonempty_llist_labeled_simple_expr_
    {let _1 =
  let _1 =
    let _2 =
      let xs =     ( List.rev xs ) in
          ( xs )
    in
            ( Pcl_apply(_1, (List.map (fun (a,b) -> (a, b))) _2) )
  in
  let _endpos__1_ = _endpos_xs_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkclass ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = extension
    {let _1 =
  let _1 =         ( Pcl_extension _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkclass ~loc:_sloc _1 )
in
      ( _1 )}

class_simple_expr:
  _1 = LPAREN _2 = class_expr _3 = RPAREN
    {      ( _2 )}
| _1 = LPAREN _2 = class_expr _3 = error
    {let _loc__3_ = (_startpos__3_, _endpos__3_) in
let _loc__1_ = (_startpos__1_, _endpos__1_) in
      ( unclosed "(" _loc__1_ ")" _loc__3_ )}
| _1 = class_longident
    {let _1 =
  let _1 =
    let cid =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
    let tys =
      let tys =       ( [] ) in
          ( tys )
    in
            ( Pcl_constr(cid, tys) )
  in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkclass ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LBRACKET xs = reversed_separated_nonempty_llist_COMMA_core_type_ _3 = RBRACKET _1_inlined1 = class_longident
    {let _1 =
  let _1 =
    let cid =
      let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
    let tys =
      let tys =
        let params =
          let xs =     ( List.rev xs ) in
              ( xs )
        in
              ( params )
      in
          ( tys )
    in
            ( Pcl_constr(cid, tys) )
  in
  let _endpos__1_ = _endpos__1_inlined1_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkclass ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = OBJECT _1_inlined1 = list_attribute_ _1_inlined2 = class_self_pattern xss = list_text_cstr_class_field__ _4 = error
    {let _1 =
  let _1 =
    let _3 =
      let _1 = _1_inlined2 in
      let _2 =
        let _1 =
          let _1 =     ( List.flatten xss ) in
              ( _1 )
        in
        let (_endpos__1_, _startpos__1_) = (_endpos_xss_, _startpos_xss_) in
        let _endpos = _endpos__1_ in
        let _startpos = _startpos__1_ in
                                       ( extra_cstr _startpos _endpos _1 )
      in
             ( Cstr.mk _1 _2 )
    in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
    let _loc__4_ = (_startpos__4_, _endpos__4_) in
    let _loc__1_ = (_startpos__1_, _endpos__1_) in
            ( unclosed "object" _loc__1_ "end" _loc__4_ )
  in
  let _endpos__1_ = _endpos__4_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkclass ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LPAREN _2 = class_expr _3 = COLON _4 = class_type _5 = RPAREN
    {let _1 =
  let _1 =         ( Pcl_constraint(_2, _4) ) in
  let _endpos__1_ = _endpos__5_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkclass ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LPAREN _2 = class_expr _3 = COLON _4 = class_type _5 = error
    {let _1 =
  let _1 =
    let _loc__5_ = (_startpos__5_, _endpos__5_) in
    let _loc__1_ = (_startpos__1_, _endpos__1_) in
            ( unclosed "(" _loc__1_ ")" _loc__5_ )
  in
  let _endpos__1_ = _endpos__5_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkclass ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = OBJECT _1_inlined1 = list_attribute_ _1_inlined2 = class_self_pattern xss = list_text_cstr_class_field__ _4 = END
    {let _3 =
  let _1 = _1_inlined2 in
  let _2 =
    let _1 =
      let _1 =     ( List.flatten xss ) in
          ( _1 )
    in
    let (_endpos__1_, _startpos__1_) = (_endpos_xss_, _startpos_xss_) in
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
                                   ( extra_cstr _startpos _endpos _1 )
  in
         ( Cstr.mk _1 _2 )
in
let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos__4_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mkclass ~loc:_sloc ~attrs:_2 (Pcl_structure _3) )}

class_fun_def:
  _1 = labeled_simple_pattern _2 = MINUSGREATER e = class_expr
    {let _1 =
  let _1 =       ( let (l,o,p) = _1 in Pcl_fun(l, o, p, e) ) in
  let _endpos__1_ = _endpos_e_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkclass ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = labeled_simple_pattern e = class_fun_def
    {let _1 =
  let _1 =       ( let (l,o,p) = _1 in Pcl_fun(l, o, p, e) ) in
  let _endpos__1_ = _endpos_e_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkclass ~loc:_sloc _1 )
in
    ( _1 )}

class_self_pattern:
  _1 = LPAREN _2 = pattern _3 = RPAREN
    {let _endpos = _endpos__3_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( reloc_pat ~loc:_sloc _2 )}
| _1 = LPAREN _2 = pattern _3 = COLON _4 = core_type _5 = RPAREN
    {let _1 =
  let _1 =       ( Ppat_constraint(_2, _4) ) in
  let _endpos__1_ = _endpos__5_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| 
    {let _endpos = _endpos__0_ in
let _symbolstartpos = _endpos in
let _sloc = (_symbolstartpos, _endpos) in
      ( ghpat ~loc:_sloc Ppat_any )}

class_field:
  _1 = INHERIT _1_inlined1 = list_attribute_ _4 = class_expr self = option_preceded_AS_mkrhs_LIDENT___ _1_inlined2 = list_post_item_attribute_
    {let _6 =
  let _1 = _1_inlined2 in
      ( _1 )
in
let _endpos__6_ = _endpos__1_inlined2_ in
let _3 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _2 =                                                 ( Fresh ) in
let _endpos = _endpos__6_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let docs = symbol_docs _sloc in
        mkcf ~loc:_sloc (Pcf_inherit (_2, _4, self)) ~attrs:(_3@_6) ~docs )}
| _1 = INHERIT _1_inlined1 = BANG _1_inlined2 = list_attribute_ _4 = class_expr self = option_preceded_AS_mkrhs_LIDENT___ _1_inlined3 = list_post_item_attribute_
    {let _6 =
  let _1 = _1_inlined3 in
      ( _1 )
in
let _endpos__6_ = _endpos__1_inlined3_ in
let _3 =
  let _1 = _1_inlined2 in
      ( _1 )
in
let _2 =                                                 ( Override ) in
let _endpos = _endpos__6_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let docs = symbol_docs _sloc in
        mkcf ~loc:_sloc (Pcf_inherit (_2, _4, self)) ~attrs:(_3@_6) ~docs )}
| _1 = VAL _2 = value _1_inlined1 = list_post_item_attribute_
    {let _3 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos__3_ = _endpos__1_inlined1_ in
let _endpos = _endpos__3_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let v, attrs = _2 in
        let docs = symbol_docs _sloc in
        mkcf ~loc:_sloc (Pcf_val v) ~attrs:(attrs@_3) ~docs )}
| _1 = METHOD _2 = method_ _1_inlined1 = list_post_item_attribute_
    {let _3 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos__3_ = _endpos__1_inlined1_ in
let _endpos = _endpos__3_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let meth, attrs = _2 in
        let docs = symbol_docs _sloc in
        mkcf ~loc:_sloc (Pcf_method meth) ~attrs:(attrs@_3) ~docs )}
| _1 = CONSTRAINT _1_inlined1 = list_attribute_ _3 = constrain_field _1_inlined2 = list_post_item_attribute_
    {let _4 =
  let _1 = _1_inlined2 in
      ( _1 )
in
let _endpos__4_ = _endpos__1_inlined2_ in
let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos__4_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let docs = symbol_docs _sloc in
        mkcf ~loc:_sloc (Pcf_constraint _3) ~attrs:(_2@_4) ~docs )}
| _1 = INITIALIZER _1_inlined1 = list_attribute_ _3 = seq_expr _1_inlined2 = list_post_item_attribute_
    {let _4 =
  let _1 = _1_inlined2 in
      ( _1 )
in
let _endpos__4_ = _endpos__1_inlined2_ in
let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos__4_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let docs = symbol_docs _sloc in
        mkcf ~loc:_sloc (Pcf_initializer _3) ~attrs:(_2@_4) ~docs )}
| _1 = item_extension _1_inlined1 = list_post_item_attribute_
    {let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos__2_ = _endpos__1_inlined1_ in
let _endpos = _endpos__2_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let docs = symbol_docs _sloc in
        mkcf ~loc:_sloc (Pcf_extension _1) ~attrs:_2 ~docs )}
| _1 = floating_attribute
    {let _1 =
  let _1 =       ( Pcf_attribute _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkcf ~loc:_sloc _1 )
in
      ( _1 )}

value:
  _1 = list_attribute_ mutable_ = virtual_with_mutable_flag _1_inlined1 = LIDENT _5 = COLON ty = core_type
    {let label =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _1 =                                                 ( _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let attrs =     ( _1 ) in
let _1 =                                                 ( Fresh ) in
      ( (label, mutable_, Cfk_virtual ty), attrs )}
| _1 = list_attribute_ _3 = mutable_flag _1_inlined1 = LIDENT _5 = EQUAL _6 = seq_expr
    {let _4 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _1 =                                                 ( _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _2 =     ( _1 ) in
let _1 =                                                 ( Fresh ) in
      ( (_4, _3, Cfk_concrete (_1, _6)), _2 )}
| _1 = BANG _1_inlined1 = list_attribute_ _3 = mutable_flag _1_inlined2 = LIDENT _5 = EQUAL _6 = seq_expr
    {let _4 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _1 =                                                 ( _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _1 =                                                 ( Override ) in
      ( (_4, _3, Cfk_concrete (_1, _6)), _2 )}
| _1 = list_attribute_ _3 = mutable_flag _1_inlined1 = LIDENT _5 = type_constraint _6 = EQUAL _7 = seq_expr
    {let _4 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _1 =                                                 ( _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _startpos__4_ = _startpos__1_inlined1_ in
let _2 =     ( _1 ) in
let (_endpos__2_, _startpos__2_) = (_endpos__1_, _startpos__1_) in
let _1 =                                                 ( Fresh ) in
let (_endpos__1_, _startpos__1_) = (_endpos__0_, _endpos__0_) in
let _endpos = _endpos__7_ in
let _symbolstartpos = if _startpos__1_ != _endpos__1_ then
  _startpos__1_
else
  if _startpos__2_ != _endpos__2_ then
    _startpos__2_
  else
    if _startpos__3_ != _endpos__3_ then
      _startpos__3_
    else
      _startpos__4_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let e = mkexp_constraint ~loc:_sloc _7 _5 in
        (_4, _3, Cfk_concrete (_1, e)), _2
      )}
| _1 = BANG _1_inlined1 = list_attribute_ _3 = mutable_flag _1_inlined2 = LIDENT _5 = type_constraint _6 = EQUAL _7 = seq_expr
    {let _4 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _1 =                                                 ( _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _startpos__4_ = _startpos__1_inlined2_ in
let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let (_endpos__2_, _startpos__2_) = (_endpos__1_inlined1_, _startpos__1_inlined1_) in
let _1 =                                                 ( Override ) in
let _endpos = _endpos__7_ in
let _symbolstartpos = if _startpos__1_ != _endpos__1_ then
  _startpos__1_
else
  if _startpos__2_ != _endpos__2_ then
    _startpos__2_
  else
    if _startpos__3_ != _endpos__3_ then
      _startpos__3_
    else
      _startpos__4_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let e = mkexp_constraint ~loc:_sloc _7 _5 in
        (_4, _3, Cfk_concrete (_1, e)), _2
      )}

method_:
  _1 = list_attribute_ private_ = virtual_with_private_flag _1_inlined1 = LIDENT _5 = COLON _1_inlined2 = possibly_poly_core_type_
    {let ty =
  let _1 = _1_inlined2 in
      ( _1 )
in
let label =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _1 =                                                 ( _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let attrs =     ( _1 ) in
let _1 =                                                 ( Fresh ) in
      ( (label, private_, Cfk_virtual ty), attrs )}
| _1 = list_attribute_ _3 = private_flag _1_inlined1 = LIDENT _5 = strict_binding
    {let _4 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _1 =                                                 ( _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _2 =     ( _1 ) in
let _1 =                                                 ( Fresh ) in
      ( let e = _5 in
        let loc = Location.(e.pexp_loc.loc_start, e.pexp_loc.loc_end) in
        (_4, _3,
        Cfk_concrete (_1, ghexp ~loc (Pexp_poly (e, None)))), _2 )}
| _1 = BANG _1_inlined1 = list_attribute_ _3 = private_flag _1_inlined2 = LIDENT _5 = strict_binding
    {let _4 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _1 =                                                 ( _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _1 =                                                 ( Override ) in
      ( let e = _5 in
        let loc = Location.(e.pexp_loc.loc_start, e.pexp_loc.loc_end) in
        (_4, _3,
        Cfk_concrete (_1, ghexp ~loc (Pexp_poly (e, None)))), _2 )}
| _1 = list_attribute_ _3 = private_flag _1_inlined1 = LIDENT _5 = COLON _1_inlined2 = possibly_poly_core_type_ _7 = EQUAL _8 = seq_expr
    {let _6 =
  let _1 = _1_inlined2 in
      ( _1 )
in
let _startpos__6_ = _startpos__1_inlined2_ in
let _4 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _1 =                                                 ( _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _2 =     ( _1 ) in
let _1 =                                                 ( Fresh ) in
      ( let poly_exp =
          let loc = (_startpos__6_, _endpos__8_) in
          ghexp ~loc (Pexp_poly(_8, Some _6)) in
        (_4, _3, Cfk_concrete (_1, poly_exp)), _2 )}
| _1 = BANG _1_inlined1 = list_attribute_ _3 = private_flag _1_inlined2 = LIDENT _5 = COLON _1_inlined3 = possibly_poly_core_type_ _7 = EQUAL _8 = seq_expr
    {let _6 =
  let _1 = _1_inlined3 in
      ( _1 )
in
let _startpos__6_ = _startpos__1_inlined3_ in
let _4 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _1 =                                                 ( _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _1 =                                                 ( Override ) in
      ( let poly_exp =
          let loc = (_startpos__6_, _endpos__8_) in
          ghexp ~loc (Pexp_poly(_8, Some _6)) in
        (_4, _3, Cfk_concrete (_1, poly_exp)), _2 )}
| _1 = list_attribute_ _3 = private_flag _1_inlined1 = LIDENT _5 = COLON _6 = TYPE xs = nonempty_list_mkrhs_LIDENT__ _8 = DOT _9 = core_type _10 = EQUAL _11 = seq_expr
    {let _7 =     ( xs ) in
let _startpos__7_ = _startpos_xs_ in
let _4 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _1 =                                                 ( _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _startpos__4_ = _startpos__1_inlined1_ in
let _2 =     ( _1 ) in
let (_endpos__2_, _startpos__2_) = (_endpos__1_, _startpos__1_) in
let _1 =                                                 ( Fresh ) in
let (_endpos__1_, _startpos__1_) = (_endpos__0_, _endpos__0_) in
let _endpos = _endpos__11_ in
let _symbolstartpos = if _startpos__1_ != _endpos__1_ then
  _startpos__1_
else
  if _startpos__2_ != _endpos__2_ then
    _startpos__2_
  else
    if _startpos__3_ != _endpos__3_ then
      _startpos__3_
    else
      _startpos__4_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let poly_exp_loc = (_startpos__7_, _endpos__11_) in
        let poly_exp =
          let exp, poly =
            (* it seems odd to use the global ~loc here while poly_exp_loc
               is tighter, but this is what ocamlyacc does;
               TODO improve parser.mly *)
            wrap_type_annotation ~loc:_sloc _7 _9 _11 in
          ghexp ~loc:poly_exp_loc (Pexp_poly(exp, Some poly)) in
        (_4, _3,
        Cfk_concrete (_1, poly_exp)), _2 )}
| _1 = BANG _1_inlined1 = list_attribute_ _3 = private_flag _1_inlined2 = LIDENT _5 = COLON _6 = TYPE xs = nonempty_list_mkrhs_LIDENT__ _8 = DOT _9 = core_type _10 = EQUAL _11 = seq_expr
    {let _7 =     ( xs ) in
let _startpos__7_ = _startpos_xs_ in
let _4 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _1 =                                                 ( _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _startpos__4_ = _startpos__1_inlined2_ in
let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let (_endpos__2_, _startpos__2_) = (_endpos__1_inlined1_, _startpos__1_inlined1_) in
let _1 =                                                 ( Override ) in
let _endpos = _endpos__11_ in
let _symbolstartpos = if _startpos__1_ != _endpos__1_ then
  _startpos__1_
else
  if _startpos__2_ != _endpos__2_ then
    _startpos__2_
  else
    if _startpos__3_ != _endpos__3_ then
      _startpos__3_
    else
      _startpos__4_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let poly_exp_loc = (_startpos__7_, _endpos__11_) in
        let poly_exp =
          let exp, poly =
            (* it seems odd to use the global ~loc here while poly_exp_loc
               is tighter, but this is what ocamlyacc does;
               TODO improve parser.mly *)
            wrap_type_annotation ~loc:_sloc _7 _9 _11 in
          ghexp ~loc:poly_exp_loc (Pexp_poly(exp, Some poly)) in
        (_4, _3,
        Cfk_concrete (_1, poly_exp)), _2 )}

class_type:
  _1 = class_signature
    {      ( _1 )}
| label = optlabel domain = tuple_type _3 = MINUSGREATER codomain = class_type
    {let _1 =
  let _1 =
    let label =       ( Optional label ) in
            ( Pcty_arrow(label, domain, codomain) )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_codomain_, _startpos_label_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkcty ~loc:_sloc _1 )
in
      ( _1 )}
| label = LIDENT _2 = COLON domain = tuple_type _3 = MINUSGREATER codomain = class_type
    {let _1 =
  let _1 =
    let label =       ( Labelled label ) in
            ( Pcty_arrow(label, domain, codomain) )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_codomain_, _startpos_label_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkcty ~loc:_sloc _1 )
in
      ( _1 )}
| domain = tuple_type _3 = MINUSGREATER codomain = class_type
    {let _1 =
  let _1 =
    let label =       ( Nolabel ) in
            ( Pcty_arrow(label, domain, codomain) )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_codomain_, _startpos_domain_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkcty ~loc:_sloc _1 )
in
      ( _1 )}

class_signature:
  _1 = clty_longident
    {let _1 =
  let _1 =
    let cid =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
    let tys =
      let tys =       ( [] ) in
          ( tys )
    in
            ( Pcty_constr (cid, tys) )
  in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkcty ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LBRACKET xs = reversed_separated_nonempty_llist_COMMA_core_type_ _3 = RBRACKET _1_inlined1 = clty_longident
    {let _1 =
  let _1 =
    let cid =
      let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
    let tys =
      let tys =
        let params =
          let xs =     ( List.rev xs ) in
              ( xs )
        in
              ( params )
      in
          ( tys )
    in
            ( Pcty_constr (cid, tys) )
  in
  let _endpos__1_ = _endpos__1_inlined1_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkcty ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = extension
    {let _1 =
  let _1 =         ( Pcty_extension _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkcty ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = OBJECT _1_inlined1 = list_attribute_ _1_inlined2 = class_self_type xss = list_text_csig_class_sig_field__ _4 = END
    {let _3 =
  let _1 = _1_inlined2 in
  let _2 =
    let _1 =
      let _1 =     ( List.flatten xss ) in
          ( _1 )
    in
    let (_endpos__1_, _startpos__1_) = (_endpos_xss_, _startpos_xss_) in
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
                                   ( extra_csig _startpos _endpos _1 )
  in
        ( Csig.mk _1 _2 )
in
let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos__4_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mkcty ~loc:_sloc ~attrs:_2 (Pcty_signature _3) )}
| _1 = OBJECT _1_inlined1 = list_attribute_ _1_inlined2 = class_self_type xss = list_text_csig_class_sig_field__ _4 = error
    {let _3 =
  let _1 = _1_inlined2 in
  let _2 =
    let _1 =
      let _1 =     ( List.flatten xss ) in
          ( _1 )
    in
    let (_endpos__1_, _startpos__1_) = (_endpos_xss_, _startpos_xss_) in
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
                                   ( extra_csig _startpos _endpos _1 )
  in
        ( Csig.mk _1 _2 )
in
let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _loc__4_ = (_startpos__4_, _endpos__4_) in
let _loc__1_ = (_startpos__1_, _endpos__1_) in
      ( unclosed "object" _loc__1_ "end" _loc__4_ )}
| _1 = class_signature _2 = attribute
    {      ( Cty.attr _1 _2 )}
| _1 = LET _2 = OPEN _1_inlined1 = list_attribute_ _1_inlined2 = mod_longident _6 = IN _7 = class_signature
    {let _5 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos__5_ = _endpos__1_inlined2_ in
let _4 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _3 =                                                 ( Fresh ) in
let _endpos = _endpos__7_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let loc = (_startpos__2_, _endpos__5_) in
        let od = Opn.mk ~override:_3 ~loc:(make_loc loc) _5 in
        mkcty ~loc:_sloc ~attrs:_4 (Pcty_open(od, _7)) )}
| _1 = LET _2 = OPEN _1_inlined1 = BANG _1_inlined2 = list_attribute_ _1_inlined3 = mod_longident _6 = IN _7 = class_signature
    {let _5 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos__5_ = _endpos__1_inlined3_ in
let _4 =
  let _1 = _1_inlined2 in
      ( _1 )
in
let _3 =                                                 ( Override ) in
let _endpos = _endpos__7_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let loc = (_startpos__2_, _endpos__5_) in
        let od = Opn.mk ~override:_3 ~loc:(make_loc loc) _5 in
        mkcty ~loc:_sloc ~attrs:_4 (Pcty_open(od, _7)) )}

class_self_type:
  _1 = LPAREN _2 = core_type _3 = RPAREN
    {      ( _2 )}
| 
    {let _1 =
  let _1 =                       ( Ptyp_any ) in
  let _endpos__1_ = _endpos__0_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _endpos in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
      ( _1 )}

class_sig_field:
  _1 = INHERIT _1_inlined1 = list_attribute_ _3 = class_signature _1_inlined2 = list_post_item_attribute_
    {let _4 =
  let _1 = _1_inlined2 in
      ( _1 )
in
let _endpos__4_ = _endpos__1_inlined2_ in
let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos__4_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let docs = symbol_docs _sloc in
        mkctf ~loc:_sloc (Pctf_inherit _3) ~attrs:(_2@_4) ~docs )}
| _1 = VAL _1_inlined1 = list_attribute_ flags = mutable_virtual_flags _1_inlined2 = LIDENT _3 = COLON ty = core_type _1_inlined3 = list_post_item_attribute_
    {let _4 =
  let _1 = _1_inlined3 in
      ( _1 )
in
let _endpos__4_ = _endpos__1_inlined3_ in
let _3 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let label =
    let _1 =                                                 ( _1 ) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
    (
    let mut, virt = flags in
    label, mut, virt, ty
  )
in
let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos__4_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let docs = symbol_docs _sloc in
        mkctf ~loc:_sloc (Pctf_val _3) ~attrs:(_2@_4) ~docs )}
| _1 = METHOD _1_inlined1 = list_attribute_ _3 = private_virtual_flags _1_inlined2 = LIDENT _5 = COLON _1_inlined3 = possibly_poly_core_type_ _1_inlined4 = list_post_item_attribute_
    {let _7 =
  let _1 = _1_inlined4 in
      ( _1 )
in
let _endpos__7_ = _endpos__1_inlined4_ in
let _6 =
  let _1 = _1_inlined3 in
      ( _1 )
in
let _4 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _1 =                                                 ( _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos__7_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let (p, v) = _3 in
        let docs = symbol_docs _sloc in
        mkctf ~loc:_sloc (Pctf_method (_4, p, v, _6)) ~attrs:(_2@_7) ~docs )}
| _1 = CONSTRAINT _1_inlined1 = list_attribute_ _3 = constrain_field _1_inlined2 = list_post_item_attribute_
    {let _4 =
  let _1 = _1_inlined2 in
      ( _1 )
in
let _endpos__4_ = _endpos__1_inlined2_ in
let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos__4_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let docs = symbol_docs _sloc in
        mkctf ~loc:_sloc (Pctf_constraint _3) ~attrs:(_2@_4) ~docs )}
| _1 = item_extension _1_inlined1 = list_post_item_attribute_
    {let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos__2_ = _endpos__1_inlined1_ in
let _endpos = _endpos__2_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let docs = symbol_docs _sloc in
        mkctf ~loc:_sloc (Pctf_extension _1) ~attrs:_2 ~docs )}
| _1 = floating_attribute
    {let _1 =
  let _1 =       ( Pctf_attribute _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkctf ~loc:_sloc _1 )
in
      ( _1 )}

constrain_field:
  _1 = core_type _2 = EQUAL _3 = core_type
    {    ( _1, _3 )}

class_type_declarations:
  _1 = CLASS _2 = TYPE ext = ext _1_inlined1 = list_attribute_ virt = virtual_flag params = formal_class_parameters _1_inlined2 = LIDENT _8 = EQUAL csig = class_signature _1_inlined3 = list_post_item_attribute_ bs = list_and_class_type_declaration_
    {let _1 =
  let a =
    let attrs2 =
      let _1 = _1_inlined3 in
          ( _1 )
    in
    let _endpos_attrs2_ = _endpos__1_inlined3_ in
    let id =
      let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
    let attrs1 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
    let _endpos = _endpos_attrs2_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        (
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc _sloc in
      let docs = symbol_docs _sloc in
      ext,
      Ci.mk id csig ~virt ~params ~attrs ~loc ~docs
    )
  in
      ( let (x, b) = a in x, b :: bs )
in
    ( let (a,b) =  _1 in a,b )}

seq_expr:
  _1 = expr %prec below_SEMI
    {                                  ( _1 )}
| _1 = expr _2 = SEMI
    {                                  ( _1 )}
| _1 = expr _2 = SEMI _3 = seq_expr
    {let _1 =
  let _1 =     ( Pexp_sequence(_1, _3) ) in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = expr _2 = SEMI _3 = PERCENT _4 = attr_id _5 = seq_expr
    {let _endpos = _endpos__5_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( let seq = mkexp ~loc:_sloc (Pexp_sequence (_1, _5)) in
      let payload = PStr ([mkstrexp ~loc:_sloc (seq) []]) in
      mkexp ~loc:_sloc (Pexp_extension (_4, payload)) )}

labeled_simple_pattern:
  _1 = QUESTION _2 = LPAREN _3 = label_let_pattern _1_inlined1 = option_preceded_EQUAL_seq_expr__ _5 = RPAREN
    {let _4 =
  let _1 = _1_inlined1 in
      ( _1 )
in
      ( ((Optional ((fst _3))), _4, snd _3) )}
| _1 = QUESTION _1_inlined1 = LIDENT
    {let _2 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _1 =
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
        ( (_1.Location.txt, mkpat ~loc:_sloc (Ppat_var (_1))) )
in
      ( ((Optional ((fst _2))), None, snd _2) )}
| _1 = OPTLABEL _2 = LPAREN _3 = let_pattern _1_inlined1 = option_preceded_EQUAL_seq_expr__ _5 = RPAREN
    {let _4 =
  let _1 = _1_inlined1 in
      ( _1 )
in
      ( ((Optional (_1)), _4, _3) )}
| _1 = OPTLABEL _2 = pattern_var
    {      ( ((Optional (_1)), None, _2) )}
| _1 = TILDE _2 = LPAREN _3 = label_let_pattern _4 = RPAREN
    {      ( ((Labelled ((fst _3))), None, snd _3) )}
| _1 = TILDE _1_inlined1 = LIDENT
    {let _2 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _1 =
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
        ( (_1.Location.txt, mkpat ~loc:_sloc (Ppat_var (_1))) )
in
      ( ((Labelled ((fst _2))), None, snd _2) )}
| _1 = LABEL _2 = simple_pattern
    {      ( ((Labelled (_1)), None, _2) )}
| _1 = simple_pattern
    {      ( (Nolabel, None, _1) )}

pattern_var:
  _1 = LIDENT
    {let _1 =
  let _1 =
    let _1 =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
                            ( Ppat_var _1 )
  in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = UNDERSCORE
    {let _1 =
  let _1 =                         ( Ppat_any ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
    ( _1 )}

label_let_pattern:
  _1 = LIDENT
    {let x =
  let _1 =
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
        ( (_1.Location.txt, mkpat ~loc:_sloc (Ppat_var (_1))) )
in
      ( x )}
| _1 = LIDENT _2 = COLON cty = core_type
    {let x =
  let _1 =
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
        ( (_1.Location.txt, mkpat ~loc:_sloc (Ppat_var (_1))) )
in
let _startpos_x_ = _startpos__1_ in
let _endpos = _endpos_cty_ in
let _symbolstartpos = _startpos_x_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let lab, pat = x in
        lab,
        mkpat ~loc:_sloc (Ppat_constraint (pat, cty)) )}

let_pattern:
  _1 = pattern
    {      ( _1 )}
| _1 = pattern _2 = COLON _3 = core_type
    {let _1 =
  let _1 =       ( Ppat_constraint(_1, _3) ) in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}

expr:
  _1 = simple_expr %prec below_HASH
    {      ( _1 )}
| _1 = LET _2 = MODULE _1_inlined1 = ext _1_inlined2 = list_attribute_ _1_inlined3 = module_name _5 = module_binding_body _6 = IN _7 = seq_expr
    {let _1 =
  let _4 =
    let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let _3 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
        ( Pexp_letmodule(_4, _5, _7), _3 )
in
let _endpos__1_ = _endpos__7_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let desc, attrs = _1 in
        mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = LET _2 = EXCEPTION _1_inlined1 = ext _1_inlined2 = list_attribute_ _1_inlined3 = constr_ident _2_inlined1 = generalized_constructor_arguments _1_inlined4 = list_attribute_ _5 = IN _6 = seq_expr
    {let _1 =
  let _4 =
    let (_endpos__1_inlined1_, _endpos__1_, _startpos__1_, _1_inlined1, _2, _1) = (_endpos__1_inlined4_, _endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined4, _2_inlined1, _1_inlined3) in
    let _3 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
    let _endpos__3_ = _endpos__1_inlined1_ in
    let _1 =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
    let _endpos = _endpos__3_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
          ( let vars, args, res = _2 in
        Te.decl _1 ~vars ~args ~res ~attrs:_3 ~loc:(make_loc _sloc) )
  in
  let _3 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
        ( Pexp_letexception(_4, _6), _3 )
in
let _endpos__1_ = _endpos__6_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let desc, attrs = _1 in
        mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = LET _2 = OPEN _1_inlined1 = ext _1_inlined2 = list_attribute_ _5 = module_expr _6 = IN _7 = seq_expr
    {let _1 =
  let _4 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
  let _3 =                                                 ( Fresh ) in
        ( let open_loc = make_loc (_startpos__2_, _endpos__5_) in
        let od = Opn.mk _5 ~override:_3 ~loc:open_loc in
        Pexp_open(od, _7), _4 )
in
let _endpos__1_ = _endpos__7_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let desc, attrs = _1 in
        mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = LET _2 = OPEN _1_inlined1 = BANG _1_inlined2 = ext _1_inlined3 = list_attribute_ _5 = module_expr _6 = IN _7 = seq_expr
    {let _1 =
  let _4 =
    let (_1_inlined1, _1) = (_1_inlined3, _1_inlined2) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
  let _3 =                                                 ( Override ) in
        ( let open_loc = make_loc (_startpos__2_, _endpos__5_) in
        let od = Opn.mk _5 ~override:_3 ~loc:open_loc in
        Pexp_open(od, _7), _4 )
in
let _endpos__1_ = _endpos__7_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let desc, attrs = _1 in
        mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = FUNCTION _1_inlined1 = ext _1_inlined2 = list_attribute_ xs = reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {let _1 =
  let _3 =
    let xs =
      let xs =     ( List.rev xs ) in
          ( xs )
    in
        ( xs )
  in
  let _2 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
        ( Pexp_function _3, _2 )
in
let _endpos__1_ = _endpos_xs_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let desc, attrs = _1 in
        mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = FUN _1_inlined1 = ext _1_inlined2 = list_attribute_ _3 = labeled_simple_pattern _4 = fun_def
    {let _1 =
  let _2 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
        ( let (l,o,p) = _3 in
        Pexp_fun(l, o, p, _4), _2 )
in
let _endpos__1_ = _endpos__4_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let desc, attrs = _1 in
        mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = FUN _1_inlined1 = ext _1_inlined2 = list_attribute_ _3 = LPAREN _4 = TYPE xs = nonempty_list_mkrhs_LIDENT__ _6 = RPAREN _7 = fun_def
    {let _1 =
  let _5 =     ( xs ) in
  let _2 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
  let _endpos = _endpos__7_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
        ( (mk_newtypes ~loc:_sloc _5 _7).pexp_desc, _2 )
in
let _endpos__1_ = _endpos__7_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let desc, attrs = _1 in
        mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = MATCH _1_inlined1 = ext _1_inlined2 = list_attribute_ _3 = seq_expr _4 = WITH xs = reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {let _1 =
  let _5 =
    let xs =
      let xs =     ( List.rev xs ) in
          ( xs )
    in
        ( xs )
  in
  let _2 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
        ( Pexp_match(_3, _5), _2 )
in
let _endpos__1_ = _endpos_xs_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let desc, attrs = _1 in
        mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = TRY _1_inlined1 = ext _1_inlined2 = list_attribute_ _3 = seq_expr _4 = WITH xs = reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {let _1 =
  let _5 =
    let xs =
      let xs =     ( List.rev xs ) in
          ( xs )
    in
        ( xs )
  in
  let _2 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
        ( Pexp_try(_3, _5), _2 )
in
let _endpos__1_ = _endpos_xs_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let desc, attrs = _1 in
        mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = TRY _1_inlined1 = ext _1_inlined2 = list_attribute_ _3 = seq_expr _4 = WITH _5 = error
    {let _1 =
  let _2 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
        ( syntax_error() )
in
let _endpos__1_ = _endpos__5_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let desc, attrs = _1 in
        mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = IF _1_inlined1 = ext _1_inlined2 = list_attribute_ _3 = seq_expr _4 = THEN _5 = expr _6 = ELSE _7 = expr
    {let _1 =
  let _2 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
        ( Pexp_ifthenelse(_3, _5, (Some _7)), _2 )
in
let _endpos__1_ = _endpos__7_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let desc, attrs = _1 in
        mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = IF _1_inlined1 = ext _1_inlined2 = list_attribute_ _3 = seq_expr _4 = THEN _5 = expr
    {let _1 =
  let _2 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
        ( Pexp_ifthenelse(_3, _5, None), _2 )
in
let _endpos__1_ = _endpos__5_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let desc, attrs = _1 in
        mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = WHILE _1_inlined1 = ext _1_inlined2 = list_attribute_ _3 = seq_expr _4 = DO _5 = seq_expr _6 = DONE
    {let _1 =
  let _2 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
        ( Pexp_while(_3, _5), _2 )
in
let _endpos__1_ = _endpos__6_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let desc, attrs = _1 in
        mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = FOR _1_inlined1 = ext _1_inlined2 = list_attribute_ _3 = pattern _4 = EQUAL _5 = seq_expr _6 = direction_flag _7 = seq_expr _8 = DO _9 = seq_expr _10 = DONE
    {let _1 =
  let _2 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
        ( Pexp_for(_3, _5, _7, _6, _9), _2 )
in
let _endpos__1_ = _endpos__10_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let desc, attrs = _1 in
        mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = ASSERT _1_inlined1 = ext _1_inlined2 = list_attribute_ _3 = simple_expr %prec below_HASH
    {let _1 =
  let _2 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
        ( Pexp_assert _3, _2 )
in
let _endpos__1_ = _endpos__3_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let desc, attrs = _1 in
        mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = LAZY _1_inlined1 = ext _1_inlined2 = list_attribute_ _3 = simple_expr %prec below_HASH
    {let _1 =
  let _2 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
        ( Pexp_lazy _3, _2 )
in
let _endpos__1_ = _endpos__3_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let desc, attrs = _1 in
        mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = simple_expr xs = reversed_nonempty_llist_labeled_simple_expr_
    {let _1 =
  let _1 =
    let _2 =
      let xs =     ( List.rev xs ) in
          ( xs )
    in
          ( Pexp_apply(_1, (List.map (fun (a,b) -> (a, b))) _2) )
  in
  let _endpos__1_ = _endpos_xs_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| xs = reversed_separated_nontrivial_llist_COMMA_expr_ %prec below_COMMA
    {let _1 =
  let _1 =
    let _1 =
      let es =
        let xs =     ( List.rev xs ) in
            ( xs )
      in
          ( es )
    in
          ( Pexp_tuple((_1)) )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_xs_, _startpos_xs_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = constr_longident _2 = simple_expr %prec below_HASH
    {let _1 =
  let _1 =
    let _1 =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
          ( Pexp_construct(_1, (Some _2)) )
  in
  let _endpos__1_ = _endpos__2_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = name_tag _2 = simple_expr %prec below_HASH
    {let _1 =
  let _1 =       ( Pexp_variant(_1, (Some _2)) ) in
  let _endpos__1_ = _endpos__2_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr op = INFIXOP0 e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                   ( op ) in
      let (_endpos__1_, _startpos__1_) = (_endpos_op_, _startpos_op_) in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr op = INFIXOP1 e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                   ( op ) in
      let (_endpos__1_, _startpos__1_) = (_endpos_op_, _startpos_op_) in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr op = INFIXOP2 e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                   ( op ) in
      let (_endpos__1_, _startpos__1_) = (_endpos_op_, _startpos_op_) in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr op = INFIXOP3 e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                   ( op ) in
      let (_endpos__1_, _startpos__1_) = (_endpos_op_, _startpos_op_) in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr op = INFIXOP4 e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                   ( op ) in
      let (_endpos__1_, _startpos__1_) = (_endpos_op_, _startpos_op_) in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr _1 = PLUS e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                    ("+") in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr _1 = PLUSDOT e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                   ("+.") in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr _1 = PLUSEQ e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                   ("+=") in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr _1 = MINUS e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                    ("-") in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr _1 = MINUSDOT e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                   ("-.") in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr _1 = STAR e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                    ("*") in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr _1 = PERCENT e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                    ("%") in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr _1 = EQUAL e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                    ("=") in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr _1 = LESS e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                    ("<") in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr _1 = GREATER e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                    (">") in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr _1 = OR e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                   ("or") in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr _1 = BARBAR e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                   ("||") in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr _1 = AMPERSAND e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                    ("&") in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr _1 = AMPERAMPER e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                   ("&&") in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| e1 = expr _1 = COLONEQUAL e2 = expr
    {let _1 =
  let _1 =
    let op =
      let _1 =                   (":=") in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix e1 op e2 )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_e2_, _startpos_e1_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = subtractive _2 = expr %prec prec_unary_minus
    {let _1 =
  let _1 =
    let _loc__1_ = (_startpos__1_, _endpos__1_) in
          ( mkuminus ~oploc:_loc__1_ _1 _2 )
  in
  let _endpos__1_ = _endpos__2_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = additive _2 = expr %prec prec_unary_plus
    {let _1 =
  let _1 =
    let _loc__1_ = (_startpos__1_, _endpos__1_) in
          ( mkuplus ~oploc:_loc__1_ _1 _2 )
  in
  let _endpos__1_ = _endpos__2_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = let_bindings_ext_ _2 = IN _3 = seq_expr
    {let _endpos = _endpos__3_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( expr_of_let_bindings ~loc:_sloc _1 _3 )}
| _1 = LETOP bindings = letop_bindings _3 = IN body = seq_expr
    {let pbop_op =
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _startpos_pbop_op_ = _startpos__1_ in
let _endpos = _endpos_body_ in
let _symbolstartpos = _startpos_pbop_op_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let (pbop_pat, pbop_exp, rev_ands) = bindings in
        let ands = List.rev rev_ands in
        let pbop_loc = make_loc _sloc in
        let let_ = {pbop_op; pbop_pat; pbop_exp; pbop_loc} in
        mkexp ~loc:_sloc (Pexp_letop{ let_=let_; ands = ands; body}) )}
| _1 = expr _2 = COLONCOLON _3 = expr
    {let _endpos = _endpos__3_ in
let _symbolstartpos = _startpos__1_ in
let _loc__2_ = (_startpos__2_, _endpos__2_) in
let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp_cons ~loc:_sloc _loc__2_ (ghexp ~loc:_sloc (Pexp_tuple([_1;_3]))) )}
| _1 = LIDENT _2 = LESSMINUS _3 = expr
    {let _1 =
  let _1 =                                                 ( _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos = _endpos__3_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc (Pexp_setinstvar(_1, _3)) )}
| _1 = simple_expr _2 = DOT _1_inlined1 = label_longident _4 = LESSMINUS _5 = expr
    {let _3 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos = _endpos__5_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc (Pexp_setfield(_1, _3, _5)) )}
| array = simple_expr d = DOT _3 = LPAREN i = seq_expr _5 = RPAREN _1 = LESSMINUS v = expr
    {let _1 =
  let r =                                                  (Some v) in
      ( array, d, Paren,   i, r )
in
let (_endpos__1_, _startpos__1_) = (_endpos_v_, _startpos_array_) in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mk_indexop_expr builtin_indexing_operators ~loc:_sloc _1 )}
| array = simple_expr d = DOT _3 = LBRACE i = seq_expr _5 = RBRACE _1 = LESSMINUS v = expr
    {let _1 =
  let r =                                                  (Some v) in
      ( array, d, Brace,   i, r )
in
let (_endpos__1_, _startpos__1_) = (_endpos_v_, _startpos_array_) in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mk_indexop_expr builtin_indexing_operators ~loc:_sloc _1 )}
| array = simple_expr d = DOT _3 = LBRACKET i = seq_expr _5 = RBRACKET _1 = LESSMINUS v = expr
    {let _1 =
  let r =                                                  (Some v) in
      ( array, d, Bracket, i, r )
in
let (_endpos__1_, _startpos__1_) = (_endpos_v_, _startpos_array_) in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mk_indexop_expr builtin_indexing_operators ~loc:_sloc _1 )}
| array = simple_expr _2 = DOTOP _3 = LPAREN es = separated_or_terminated_nonempty_list_SEMI_expr_ _5 = RPAREN _1 = LESSMINUS v = expr
    {let _1 =
  let r =                                                                    (Some v) in
  let i =     ( es ) in
  let d =
    let _1 =     ( None ) in
                                                                   ( _1, _2 )
  in
      ( array, d, Paren,   i, r )
in
let (_endpos__1_, _startpos__1_) = (_endpos_v_, _startpos_array_) in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mk_indexop_expr user_indexing_operators ~loc:_sloc _1 )}
| array = simple_expr _1 = DOT _2_inlined1 = mod_longident _2 = DOTOP _3 = LPAREN es = separated_or_terminated_nonempty_list_SEMI_expr_ _5 = RPAREN _1_inlined1 = LESSMINUS v = expr
    {let _1 =
  let r =                                                                    (Some v) in
  let i =     ( es ) in
  let d =
    let _1 =
      let _2 = _2_inlined1 in
      let x =                                                    (_2) in
          ( Some x )
    in
                                                                   ( _1, _2 )
  in
      ( array, d, Paren,   i, r )
in
let (_endpos__1_, _startpos__1_) = (_endpos_v_, _startpos_array_) in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mk_indexop_expr user_indexing_operators ~loc:_sloc _1 )}
| array = simple_expr _2 = DOTOP _3 = LBRACE es = separated_or_terminated_nonempty_list_SEMI_expr_ _5 = RBRACE _1 = LESSMINUS v = expr
    {let _1 =
  let r =                                                                    (Some v) in
  let i =     ( es ) in
  let d =
    let _1 =     ( None ) in
                                                                   ( _1, _2 )
  in
      ( array, d, Brace,   i, r )
in
let (_endpos__1_, _startpos__1_) = (_endpos_v_, _startpos_array_) in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mk_indexop_expr user_indexing_operators ~loc:_sloc _1 )}
| array = simple_expr _1 = DOT _2_inlined1 = mod_longident _2 = DOTOP _3 = LBRACE es = separated_or_terminated_nonempty_list_SEMI_expr_ _5 = RBRACE _1_inlined1 = LESSMINUS v = expr
    {let _1 =
  let r =                                                                    (Some v) in
  let i =     ( es ) in
  let d =
    let _1 =
      let _2 = _2_inlined1 in
      let x =                                                    (_2) in
          ( Some x )
    in
                                                                   ( _1, _2 )
  in
      ( array, d, Brace,   i, r )
in
let (_endpos__1_, _startpos__1_) = (_endpos_v_, _startpos_array_) in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mk_indexop_expr user_indexing_operators ~loc:_sloc _1 )}
| array = simple_expr _2 = DOTOP _3 = LBRACKET es = separated_or_terminated_nonempty_list_SEMI_expr_ _5 = RBRACKET _1 = LESSMINUS v = expr
    {let _1 =
  let r =                                                                    (Some v) in
  let i =     ( es ) in
  let d =
    let _1 =     ( None ) in
                                                                   ( _1, _2 )
  in
      ( array, d, Bracket, i, r )
in
let (_endpos__1_, _startpos__1_) = (_endpos_v_, _startpos_array_) in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mk_indexop_expr user_indexing_operators ~loc:_sloc _1 )}
| array = simple_expr _1 = DOT _2_inlined1 = mod_longident _2 = DOTOP _3 = LBRACKET es = separated_or_terminated_nonempty_list_SEMI_expr_ _5 = RBRACKET _1_inlined1 = LESSMINUS v = expr
    {let _1 =
  let r =                                                                    (Some v) in
  let i =     ( es ) in
  let d =
    let _1 =
      let _2 = _2_inlined1 in
      let x =                                                    (_2) in
          ( Some x )
    in
                                                                   ( _1, _2 )
  in
      ( array, d, Bracket, i, r )
in
let (_endpos__1_, _startpos__1_) = (_endpos_v_, _startpos_array_) in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mk_indexop_expr user_indexing_operators ~loc:_sloc _1 )}
| _1 = expr _2 = attribute
    {      ( Exp.attr _1 _2 )}
| _1 = UNDERSCORE
    {let _loc__1_ = (_startpos__1_, _endpos__1_) in
     ( not_expecting _loc__1_ "wildcard \"_\"" )}

simple_expr:
  _1 = LPAREN _2 = seq_expr _3 = RPAREN
    {let _endpos = _endpos__3_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( reloc_exp ~loc:_sloc _2 )}
| _1 = LPAREN _2 = seq_expr _3 = error
    {let _loc__3_ = (_startpos__3_, _endpos__3_) in
let _loc__1_ = (_startpos__1_, _endpos__1_) in
      ( unclosed "(" _loc__1_ ")" _loc__3_ )}
| _1 = LPAREN _2 = seq_expr _3 = type_constraint _4 = RPAREN
    {let _endpos = _endpos__4_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp_constraint ~loc:_sloc _2 _3 )}
| array = simple_expr d = DOT _3 = LPAREN i = seq_expr _5 = RPAREN
    {let _1 =
  let r =                                 ( None ) in
      ( array, d, Paren,   i, r )
in
let (_endpos__1_, _startpos__1_) = (_endpos__5_, _startpos_array_) in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mk_indexop_expr builtin_indexing_operators ~loc:_sloc _1 )}
| array = simple_expr d = DOT _3 = LBRACE i = seq_expr _5 = RBRACE
    {let _1 =
  let r =                                 ( None ) in
      ( array, d, Brace,   i, r )
in
let (_endpos__1_, _startpos__1_) = (_endpos__5_, _startpos_array_) in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mk_indexop_expr builtin_indexing_operators ~loc:_sloc _1 )}
| array = simple_expr d = DOT _3 = LBRACKET i = seq_expr _5 = RBRACKET
    {let _1 =
  let r =                                 ( None ) in
      ( array, d, Bracket, i, r )
in
let (_endpos__1_, _startpos__1_) = (_endpos__5_, _startpos_array_) in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mk_indexop_expr builtin_indexing_operators ~loc:_sloc _1 )}
| array = simple_expr _2 = DOTOP _3 = LPAREN es = separated_or_terminated_nonempty_list_SEMI_expr_ _5 = RPAREN
    {let _1 =
  let r =                                                   ( None ) in
  let i =     ( es ) in
  let d =
    let _1 =     ( None ) in
                                                                   ( _1, _2 )
  in
      ( array, d, Paren,   i, r )
in
let (_endpos__1_, _startpos__1_) = (_endpos__5_, _startpos_array_) in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mk_indexop_expr user_indexing_operators ~loc:_sloc _1 )}
| array = simple_expr _1 = DOT _2_inlined1 = mod_longident _2 = DOTOP _3 = LPAREN es = separated_or_terminated_nonempty_list_SEMI_expr_ _5 = RPAREN
    {let _1 =
  let r =                                                   ( None ) in
  let i =     ( es ) in
  let d =
    let _1 =
      let _2 = _2_inlined1 in
      let x =                                                    (_2) in
          ( Some x )
    in
                                                                   ( _1, _2 )
  in
      ( array, d, Paren,   i, r )
in
let (_endpos__1_, _startpos__1_) = (_endpos__5_, _startpos_array_) in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mk_indexop_expr user_indexing_operators ~loc:_sloc _1 )}
| array = simple_expr _2 = DOTOP _3 = LBRACE es = separated_or_terminated_nonempty_list_SEMI_expr_ _5 = RBRACE
    {let _1 =
  let r =                                                   ( None ) in
  let i =     ( es ) in
  let d =
    let _1 =     ( None ) in
                                                                   ( _1, _2 )
  in
      ( array, d, Brace,   i, r )
in
let (_endpos__1_, _startpos__1_) = (_endpos__5_, _startpos_array_) in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mk_indexop_expr user_indexing_operators ~loc:_sloc _1 )}
| array = simple_expr _1 = DOT _2_inlined1 = mod_longident _2 = DOTOP _3 = LBRACE es = separated_or_terminated_nonempty_list_SEMI_expr_ _5 = RBRACE
    {let _1 =
  let r =                                                   ( None ) in
  let i =     ( es ) in
  let d =
    let _1 =
      let _2 = _2_inlined1 in
      let x =                                                    (_2) in
          ( Some x )
    in
                                                                   ( _1, _2 )
  in
      ( array, d, Brace,   i, r )
in
let (_endpos__1_, _startpos__1_) = (_endpos__5_, _startpos_array_) in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mk_indexop_expr user_indexing_operators ~loc:_sloc _1 )}
| array = simple_expr _2 = DOTOP _3 = LBRACKET es = separated_or_terminated_nonempty_list_SEMI_expr_ _5 = RBRACKET
    {let _1 =
  let r =                                                   ( None ) in
  let i =     ( es ) in
  let d =
    let _1 =     ( None ) in
                                                                   ( _1, _2 )
  in
      ( array, d, Bracket, i, r )
in
let (_endpos__1_, _startpos__1_) = (_endpos__5_, _startpos_array_) in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mk_indexop_expr user_indexing_operators ~loc:_sloc _1 )}
| array = simple_expr _1 = DOT _2_inlined1 = mod_longident _2 = DOTOP _3 = LBRACKET es = separated_or_terminated_nonempty_list_SEMI_expr_ _5 = RBRACKET
    {let _1 =
  let r =                                                   ( None ) in
  let i =     ( es ) in
  let d =
    let _1 =
      let _2 = _2_inlined1 in
      let x =                                                    (_2) in
          ( Some x )
    in
                                                                   ( _1, _2 )
  in
      ( array, d, Bracket, i, r )
in
let (_endpos__1_, _startpos__1_) = (_endpos__5_, _startpos_array_) in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mk_indexop_expr user_indexing_operators ~loc:_sloc _1 )}
| _1 = simple_expr _2 = DOT _p = LPAREN _4 = seq_expr _e = error
    {let _1 =
  let _loc__p_ = (_startpos__p_, _endpos__p_) in
  let _loc__e_ = (_startpos__e_, _endpos__e_) in
      ( indexop_unclosed_error _loc__p_  Paren _loc__e_ )
in
                                  ( _1 )}
| _1 = simple_expr _2 = DOT _p = LBRACE _4 = seq_expr _e = error
    {let _1 =
  let _loc__p_ = (_startpos__p_, _endpos__p_) in
  let _loc__e_ = (_startpos__e_, _endpos__e_) in
      ( indexop_unclosed_error _loc__p_ Brace _loc__e_ )
in
                                  ( _1 )}
| _1 = simple_expr _2 = DOT _p = LBRACKET _4 = seq_expr _e = error
    {let _1 =
  let _loc__p_ = (_startpos__p_, _endpos__p_) in
  let _loc__e_ = (_startpos__e_, _endpos__e_) in
      ( indexop_unclosed_error _loc__p_ Bracket _loc__e_ )
in
                                  ( _1 )}
| _1 = simple_expr _2 = DOTOP _p = LPAREN es = separated_or_terminated_nonempty_list_SEMI_expr_ _e = error
    {let _1 =
  let _4 =     ( es ) in
  let _2 =
    let _1 =     ( None ) in
                                                                   ( _1, _2 )
  in
  let _loc__p_ = (_startpos__p_, _endpos__p_) in
  let _loc__e_ = (_startpos__e_, _endpos__e_) in
      ( indexop_unclosed_error _loc__p_  Paren _loc__e_ )
in
                                                    ( _1 )}
| _1 = simple_expr _1_inlined1 = DOT _2_inlined1 = mod_longident _2 = DOTOP _p = LPAREN es = separated_or_terminated_nonempty_list_SEMI_expr_ _e = error
    {let _1 =
  let _4 =     ( es ) in
  let _2 =
    let _1 =
      let _2 = _2_inlined1 in
      let x =                                                    (_2) in
          ( Some x )
    in
                                                                   ( _1, _2 )
  in
  let _loc__p_ = (_startpos__p_, _endpos__p_) in
  let _loc__e_ = (_startpos__e_, _endpos__e_) in
      ( indexop_unclosed_error _loc__p_  Paren _loc__e_ )
in
                                                    ( _1 )}
| _1 = simple_expr _2 = DOTOP _p = LBRACE es = separated_or_terminated_nonempty_list_SEMI_expr_ _e = error
    {let _1 =
  let _4 =     ( es ) in
  let _2 =
    let _1 =     ( None ) in
                                                                   ( _1, _2 )
  in
  let _loc__p_ = (_startpos__p_, _endpos__p_) in
  let _loc__e_ = (_startpos__e_, _endpos__e_) in
      ( indexop_unclosed_error _loc__p_ Brace _loc__e_ )
in
                                                    ( _1 )}
| _1 = simple_expr _1_inlined1 = DOT _2_inlined1 = mod_longident _2 = DOTOP _p = LBRACE es = separated_or_terminated_nonempty_list_SEMI_expr_ _e = error
    {let _1 =
  let _4 =     ( es ) in
  let _2 =
    let _1 =
      let _2 = _2_inlined1 in
      let x =                                                    (_2) in
          ( Some x )
    in
                                                                   ( _1, _2 )
  in
  let _loc__p_ = (_startpos__p_, _endpos__p_) in
  let _loc__e_ = (_startpos__e_, _endpos__e_) in
      ( indexop_unclosed_error _loc__p_ Brace _loc__e_ )
in
                                                    ( _1 )}
| _1 = simple_expr _2 = DOTOP _p = LBRACKET es = separated_or_terminated_nonempty_list_SEMI_expr_ _e = error
    {let _1 =
  let _4 =     ( es ) in
  let _2 =
    let _1 =     ( None ) in
                                                                   ( _1, _2 )
  in
  let _loc__p_ = (_startpos__p_, _endpos__p_) in
  let _loc__e_ = (_startpos__e_, _endpos__e_) in
      ( indexop_unclosed_error _loc__p_ Bracket _loc__e_ )
in
                                                    ( _1 )}
| _1 = simple_expr _1_inlined1 = DOT _2_inlined1 = mod_longident _2 = DOTOP _p = LBRACKET es = separated_or_terminated_nonempty_list_SEMI_expr_ _e = error
    {let _1 =
  let _4 =     ( es ) in
  let _2 =
    let _1 =
      let _2 = _2_inlined1 in
      let x =                                                    (_2) in
          ( Some x )
    in
                                                                   ( _1, _2 )
  in
  let _loc__p_ = (_startpos__p_, _endpos__p_) in
  let _loc__e_ = (_startpos__e_, _endpos__e_) in
      ( indexop_unclosed_error _loc__p_ Bracket _loc__e_ )
in
                                                    ( _1 )}
| _1 = BEGIN ext = ext _1_inlined1 = list_attribute_ e = seq_expr _5 = END
    {let _1 =
  let attrs =
    let _1 = _1_inlined1 in
        ( _1 )
  in
        ( e.pexp_desc, (ext, attrs @ e.pexp_attributes) )
in
let _endpos__1_ = _endpos__5_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( let desc, attrs = _1 in
      mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = BEGIN _1_inlined1 = ext _1_inlined2 = list_attribute_ _3 = END
    {let _1 =
  let _2 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
  let _endpos = _endpos__3_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
        ( Pexp_construct (mkloc ((Lident ("()"))) (make_loc _sloc), None), _2 )
in
let _endpos__1_ = _endpos__3_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( let desc, attrs = _1 in
      mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = BEGIN _1_inlined1 = ext _1_inlined2 = list_attribute_ _3 = seq_expr _4 = error
    {let _1 =
  let _2 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
  let _loc__4_ = (_startpos__4_, _endpos__4_) in
  let _loc__1_ = (_startpos__1_, _endpos__1_) in
        ( unclosed "begin" _loc__1_ "end" _loc__4_ )
in
let _endpos__1_ = _endpos__4_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( let desc, attrs = _1 in
      mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = NEW _1_inlined1 = ext _1_inlined2 = list_attribute_ _1_inlined3 = class_longident
    {let _1 =
  let _3 =
    let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let _2 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
        ( Pexp_new(_3), _2 )
in
let _endpos__1_ = _endpos__1_inlined3_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( let desc, attrs = _1 in
      mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = LPAREN _2 = MODULE _1_inlined1 = ext _1_inlined2 = list_attribute_ _4 = module_expr _5 = RPAREN
    {let _1 =
  let _3 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
        ( Pexp_pack _4, _3 )
in
let _endpos__1_ = _endpos__5_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( let desc, attrs = _1 in
      mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = LPAREN _2 = MODULE _1_inlined1 = ext _1_inlined2 = list_attribute_ _4 = module_expr _5 = COLON _1_inlined3 = module_type _7 = RPAREN
    {let _1 =
  let _6 =
    let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
          ( let (lid, cstrs, attrs) = package_type_of_module_type _1 in
        let descr = Ptyp_package (lid, cstrs) in
        mktyp ~loc:_sloc ~attrs descr )
  in
  let _3 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
  let _endpos = _endpos__7_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
        ( Pexp_constraint (ghexp ~loc:_sloc (Pexp_pack _4), _6), _3 )
in
let _endpos__1_ = _endpos__7_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( let desc, attrs = _1 in
      mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = LPAREN _2 = MODULE _1_inlined1 = ext _1_inlined2 = list_attribute_ _4 = module_expr _5 = COLON _6 = error
    {let _1 =
  let _3 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
  let _loc__6_ = (_startpos__6_, _endpos__6_) in
  let _loc__1_ = (_startpos__1_, _endpos__1_) in
        ( unclosed "(" _loc__1_ ")" _loc__6_ )
in
let _endpos__1_ = _endpos__6_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( let desc, attrs = _1 in
      mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = OBJECT _1_inlined1 = ext _1_inlined2 = list_attribute_ _1_inlined3 = class_self_pattern xss = list_text_cstr_class_field__ _4 = END
    {let _1 =
  let _3 =
    let _1 = _1_inlined3 in
    let _2 =
      let _1 =
        let _1 =     ( List.flatten xss ) in
            ( _1 )
      in
      let (_endpos__1_, _startpos__1_) = (_endpos_xss_, _startpos_xss_) in
      let _endpos = _endpos__1_ in
      let _startpos = _startpos__1_ in
                                     ( extra_cstr _startpos _endpos _1 )
    in
           ( Cstr.mk _1 _2 )
  in
  let _2 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
        ( Pexp_object _3, _2 )
in
let _endpos__1_ = _endpos__4_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( let desc, attrs = _1 in
      mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = OBJECT _1_inlined1 = ext _1_inlined2 = list_attribute_ _1_inlined3 = class_self_pattern xss = list_text_cstr_class_field__ _4 = error
    {let _1 =
  let _3 =
    let _1 = _1_inlined3 in
    let _2 =
      let _1 =
        let _1 =     ( List.flatten xss ) in
            ( _1 )
      in
      let (_endpos__1_, _startpos__1_) = (_endpos_xss_, _startpos_xss_) in
      let _endpos = _endpos__1_ in
      let _startpos = _startpos__1_ in
                                     ( extra_cstr _startpos _endpos _1 )
    in
           ( Cstr.mk _1 _2 )
  in
  let _2 =
    let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
    let _2 =
      let _1 = _1_inlined1 in
          ( _1 )
    in
                        ( _1, _2 )
  in
  let _loc__4_ = (_startpos__4_, _endpos__4_) in
  let _loc__1_ = (_startpos__1_, _endpos__1_) in
        ( unclosed "object" _loc__1_ "end" _loc__4_ )
in
let _endpos__1_ = _endpos__4_ in
let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( let desc, attrs = _1 in
      mkexp_attrs ~loc:_sloc desc attrs )}
| _1 = val_longident
    {let _1 =
  let _1 =
    let _1 =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
          ( Pexp_ident (_1) )
  in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = constant
    {let _1 =
  let _1 =       ( Pexp_constant _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = constr_longident %prec prec_constant_constructor
    {let _1 =
  let _1 =
    let _1 =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
          ( Pexp_construct(_1, None) )
  in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = name_tag %prec prec_constant_constructor
    {let _1 =
  let _1 =       ( Pexp_variant(_1, None) ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = PREFIXOP _2 = simple_expr
    {let _1 =
  let _1 =
    let _1 =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( Pexp_apply(_1, [Nolabel,_2]) )
  in
  let _endpos__1_ = _endpos__2_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = BANG _2 = simple_expr
    {let _1 =
  let _1 =
    let _1 =
      let _1 =             ("!") in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( Pexp_apply(_1, [Nolabel,_2]) )
  in
  let _endpos__1_ = _endpos__2_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LBRACELESS xs = separated_or_terminated_nonempty_list_SEMI_object_expr_field_ _3 = GREATERRBRACE
    {let _1 =
  let _1 =
    let _2 =     ( xs ) in
          ( Pexp_override _2 )
  in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LBRACELESS xs = separated_or_terminated_nonempty_list_SEMI_object_expr_field_ _3 = error
    {let _1 =
  let _1 =
    let _2 =     ( xs ) in
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
    let _loc__1_ = (_startpos__1_, _endpos__1_) in
          ( unclosed "{<" _loc__1_ ">}" _loc__3_ )
  in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LBRACELESS _2 = GREATERRBRACE
    {let _1 =
  let _1 =       ( Pexp_override ([]) ) in
  let _endpos__1_ = _endpos__2_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = simple_expr _2 = DOT _1_inlined1 = label_longident
    {let _1 =
  let _1 =
    let _3 =
      let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
          ( Pexp_field(_1, _3) )
  in
  let _endpos__1_ = _endpos__1_inlined1_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _3 = LPAREN _4 = seq_expr _5 = RPAREN
    {let _1 =
  let _1 =
    let od =
      let _1 =
        let _endpos = _endpos__1_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( mkrhs _1 _sloc )
      in
      let _loc__1_ = (_startpos__1_, _endpos__1_) in
        ( let loc = make_loc _loc__1_ in
    let me = Mod.ident ~loc _1 in
    Opn.mk ~loc me )
    in
          ( Pexp_open(od, _4) )
  in
  let _endpos__1_ = _endpos__5_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _3 = LBRACELESS xs = separated_or_terminated_nonempty_list_SEMI_object_expr_field_ _5 = GREATERRBRACE
    {let _1 =
  let _1 =
    let _4 =     ( xs ) in
    let od =
      let _1 =
        let _endpos = _endpos__1_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( mkrhs _1 _sloc )
      in
      let _loc__1_ = (_startpos__1_, _endpos__1_) in
        ( let loc = make_loc _loc__1_ in
    let me = Mod.ident ~loc _1 in
    Opn.mk ~loc me )
    in
    let _startpos_od_ = _startpos__1_ in
    let _endpos = _endpos__5_ in
    let _symbolstartpos = _startpos_od_ in
    let _sloc = (_symbolstartpos, _endpos) in
          ( (* TODO: review the location of Pexp_override *)
        Pexp_open(od, mkexp ~loc:_sloc (Pexp_override _4)) )
  in
  let _endpos__1_ = _endpos__5_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _3 = LBRACELESS xs = separated_or_terminated_nonempty_list_SEMI_object_expr_field_ _5 = error
    {let _1 =
  let _1 =
    let _4 =     ( xs ) in
    let _loc__5_ = (_startpos__5_, _endpos__5_) in
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
          ( unclosed "{<" _loc__3_ ">}" _loc__5_ )
  in
  let _endpos__1_ = _endpos__5_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = simple_expr _2 = HASH _1_inlined1 = LIDENT
    {let _1 =
  let _1 =
    let _3 =
      let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
      let _1 =                                                 ( _1 ) in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
          ( Pexp_send(_1, _3) )
  in
  let _endpos__1_ = _endpos__1_inlined1_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = simple_expr _1_inlined1 = HASHOP _3 = simple_expr
    {let _1 =
  let _1 =
    let _2 =
      let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
         ( mkoperator ~loc:_sloc (_1) )
    in
          ( mkinfix _1 _2 _3 )
  in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = extension
    {let _1 =
  let _1 =       ( Pexp_extension _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _1_inlined1 = LPAREN _2_inlined1 = RPAREN
    {let _1 =
  let _1 =
    let _3 =
      let (_endpos__2_, _startpos__1_) = (_endpos__2_inlined1_, _startpos__1_inlined1_) in
      let _1 =                                                     ((Lident ("()"))) in
      let _endpos__1_ = _endpos__2_ in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
    let (_endpos__3_, _startpos__3_) = (_endpos__2_inlined1_, _startpos__1_inlined1_) in
    let od =
      let _1 =
        let _endpos = _endpos__1_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( mkrhs _1 _sloc )
      in
      let _loc__1_ = (_startpos__1_, _endpos__1_) in
        ( let loc = make_loc _loc__1_ in
    let me = Mod.ident ~loc _1 in
    Opn.mk ~loc me )
    in
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
          ( Pexp_open(od, mkexp ~loc:(_loc__3_) (Pexp_construct(_3, None))) )
  in
  let _endpos__1_ = _endpos__2_inlined1_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _3 = LPAREN _4 = seq_expr _5 = error
    {let _1 =
  let _1 =
    let _loc__5_ = (_startpos__5_, _endpos__5_) in
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
          ( unclosed "(" _loc__3_ ")" _loc__5_ )
  in
  let _endpos__1_ = _endpos__5_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LBRACE _2 = record_expr_content _3 = RBRACE
    {let _1 =
  let _1 =       ( let (exten, fields) = _2 in
        Pexp_record(fields, exten) ) in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LBRACE _2 = record_expr_content _3 = error
    {let _1 =
  let _1 =
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
    let _loc__1_ = (_startpos__1_, _endpos__1_) in
          ( unclosed "{" _loc__1_ "}" _loc__3_ )
  in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _3 = LBRACE _4 = record_expr_content _5 = RBRACE
    {let _1 =
  let _1 =
    let od =
      let _1 =
        let _endpos = _endpos__1_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( mkrhs _1 _sloc )
      in
      let _loc__1_ = (_startpos__1_, _endpos__1_) in
        ( let loc = make_loc _loc__1_ in
    let me = Mod.ident ~loc _1 in
    Opn.mk ~loc me )
    in
    let _endpos = _endpos__5_ in
          ( let (exten, fields) = _4 in
        Pexp_open(od, mkexp ~loc:(_startpos__3_, _endpos)
                        (Pexp_record(fields, exten))) )
  in
  let _endpos__1_ = _endpos__5_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _3 = LBRACE _4 = record_expr_content _5 = error
    {let _1 =
  let _1 =
    let _loc__5_ = (_startpos__5_, _endpos__5_) in
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
          ( unclosed "{" _loc__3_ "}" _loc__5_ )
  in
  let _endpos__1_ = _endpos__5_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LBRACKETBAR es = separated_or_terminated_nonempty_list_SEMI_expr_ _3 = BARRBRACKET
    {let _1 =
  let _1 =
    let _2 =     ( es ) in
          ( Pexp_array(_2) )
  in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LBRACKETBAR es = separated_or_terminated_nonempty_list_SEMI_expr_ _3 = error
    {let _1 =
  let _1 =
    let _2 =     ( es ) in
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
    let _loc__1_ = (_startpos__1_, _endpos__1_) in
          ( unclosed "[|" _loc__1_ "|]" _loc__3_ )
  in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LBRACKETBAR _2 = BARRBRACKET
    {let _1 =
  let _1 =       ( Pexp_array ([]) ) in
  let _endpos__1_ = _endpos__2_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _3 = LBRACKETBAR es = separated_or_terminated_nonempty_list_SEMI_expr_ _5 = BARRBRACKET
    {let _1 =
  let _1 =
    let _4 =     ( es ) in
    let od =
      let _1 =
        let _endpos = _endpos__1_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( mkrhs _1 _sloc )
      in
      let _loc__1_ = (_startpos__1_, _endpos__1_) in
        ( let loc = make_loc _loc__1_ in
    let me = Mod.ident ~loc _1 in
    Opn.mk ~loc me )
    in
    let _endpos = _endpos__5_ in
          ( Pexp_open(od, mkexp ~loc:(_startpos__3_, _endpos) (Pexp_array(_4))) )
  in
  let _endpos__1_ = _endpos__5_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _3 = LBRACKETBAR _4 = BARRBRACKET
    {let _1 =
  let _1 =
    let od =
      let _1 =
        let _endpos = _endpos__1_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( mkrhs _1 _sloc )
      in
      let _loc__1_ = (_startpos__1_, _endpos__1_) in
        ( let loc = make_loc _loc__1_ in
    let me = Mod.ident ~loc _1 in
    Opn.mk ~loc me )
    in
    let _endpos = _endpos__4_ in
          ( (* TODO: review the location of Pexp_array *)
        Pexp_open(od, mkexp ~loc:(_startpos__3_, _endpos) (Pexp_array ([]))) )
  in
  let _endpos__1_ = _endpos__4_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _3 = LBRACKETBAR es = separated_or_terminated_nonempty_list_SEMI_expr_ _5 = error
    {let _1 =
  let _1 =
    let _4 =     ( es ) in
    let _loc__5_ = (_startpos__5_, _endpos__5_) in
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
          ( unclosed "[|" _loc__3_ "|]" _loc__5_ )
  in
  let _endpos__1_ = _endpos__5_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LBRACKET es = separated_or_terminated_nonempty_list_SEMI_expr_ _3 = RBRACKET
    {let _1 =
  let _1 =
    let _2 =     ( es ) in
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
          ( fst (mktailexp _loc__3_ _2) )
  in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LBRACKET es = separated_or_terminated_nonempty_list_SEMI_expr_ _3 = error
    {let _1 =
  let _1 =
    let _2 =     ( es ) in
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
    let _loc__1_ = (_startpos__1_, _endpos__1_) in
          ( unclosed "[" _loc__1_ "]" _loc__3_ )
  in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _3 = LBRACKET es = separated_or_terminated_nonempty_list_SEMI_expr_ _5 = RBRACKET
    {let _1 =
  let _1 =
    let _4 =     ( es ) in
    let od =
      let _1 =
        let _endpos = _endpos__1_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( mkrhs _1 _sloc )
      in
      let _loc__1_ = (_startpos__1_, _endpos__1_) in
        ( let loc = make_loc _loc__1_ in
    let me = Mod.ident ~loc _1 in
    Opn.mk ~loc me )
    in
    let _endpos = _endpos__5_ in
    let _loc__5_ = (_startpos__5_, _endpos__5_) in
          ( let list_exp =
          (* TODO: review the location of list_exp *)
          let tail_exp, _tail_loc = mktailexp _loc__5_ _4 in
          mkexp ~loc:(_startpos__3_, _endpos) tail_exp in
        Pexp_open(od, list_exp) )
  in
  let _endpos__1_ = _endpos__5_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _1_inlined1 = LBRACKET _2_inlined1 = RBRACKET
    {let _1 =
  let _1 =
    let _3 =
      let (_endpos__2_, _startpos__1_) = (_endpos__2_inlined1_, _startpos__1_inlined1_) in
      let _1 =                                                         ((Lident ("[]"))) in
      let _endpos__1_ = _endpos__2_ in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
    let (_endpos__3_, _startpos__3_) = (_endpos__2_inlined1_, _startpos__1_inlined1_) in
    let od =
      let _1 =
        let _endpos = _endpos__1_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( mkrhs _1 _sloc )
      in
      let _loc__1_ = (_startpos__1_, _endpos__1_) in
        ( let loc = make_loc _loc__1_ in
    let me = Mod.ident ~loc _1 in
    Opn.mk ~loc me )
    in
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
          ( Pexp_open(od, mkexp ~loc:_loc__3_ (Pexp_construct(_3, None))) )
  in
  let _endpos__1_ = _endpos__2_inlined1_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _3 = LBRACKET es = separated_or_terminated_nonempty_list_SEMI_expr_ _5 = error
    {let _1 =
  let _1 =
    let _4 =     ( es ) in
    let _loc__5_ = (_startpos__5_, _endpos__5_) in
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
          ( unclosed "[" _loc__3_ "]" _loc__5_ )
  in
  let _endpos__1_ = _endpos__5_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _3 = LPAREN _4 = MODULE _1_inlined1 = ext _1_inlined2 = list_attribute_ _6 = module_expr _7 = COLON _1_inlined3 = module_type _9 = RPAREN
    {let _1 =
  let _1 =
    let _8 =
      let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
            ( let (lid, cstrs, attrs) = package_type_of_module_type _1 in
        let descr = Ptyp_package (lid, cstrs) in
        mktyp ~loc:_sloc ~attrs descr )
    in
    let _5 =
      let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
      let _2 =
        let _1 = _1_inlined1 in
            ( _1 )
      in
                          ( _1, _2 )
    in
    let od =
      let _1 =
        let _endpos = _endpos__1_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( mkrhs _1 _sloc )
      in
      let _loc__1_ = (_startpos__1_, _endpos__1_) in
        ( let loc = make_loc _loc__1_ in
    let me = Mod.ident ~loc _1 in
    Opn.mk ~loc me )
    in
    let _startpos_od_ = _startpos__1_ in
    let _endpos = _endpos__9_ in
    let _symbolstartpos = _startpos_od_ in
    let _sloc = (_symbolstartpos, _endpos) in
          ( let modexp =
          mkexp_attrs ~loc:(_startpos__3_, _endpos)
            (Pexp_constraint (ghexp ~loc:_sloc (Pexp_pack _6), _8)) _5 in
        Pexp_open(od, modexp) )
  in
  let _endpos__1_ = _endpos__9_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _3 = LPAREN _4 = MODULE _1_inlined1 = ext _1_inlined2 = list_attribute_ _6 = module_expr _7 = COLON _8 = error
    {let _1 =
  let _1 =
    let _5 =
      let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
      let _2 =
        let _1 = _1_inlined1 in
            ( _1 )
      in
                          ( _1, _2 )
    in
    let _loc__8_ = (_startpos__8_, _endpos__8_) in
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
          ( unclosed "(" _loc__3_ ")" _loc__8_ )
  in
  let _endpos__1_ = _endpos__8_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}

labeled_simple_expr:
  _1 = simple_expr %prec below_HASH
    {      ( (Nolabel, _1) )}
| _1 = LABEL _2 = simple_expr %prec below_HASH
    {      ( (Labelled (_1), _2) )}
| _1 = TILDE label = LIDENT
    {let _loc_label_ = (_startpos_label_, _endpos_label_) in
      ( let loc = _loc_label_ in
        (Labelled (label), mkexpvar ~loc (label)) )}
| _1 = TILDE _2 = LPAREN label = LIDENT ty = type_constraint _5 = RPAREN
    {let _endpos = _endpos__5_ in
let _loc_label_ = (_startpos_label_, _endpos_label_) in
      ( (Labelled (label), mkexp_constraint ~loc:(_startpos__2_, _endpos)
                           (mkexpvar ~loc:_loc_label_ (label)) ty) )}
| _1 = QUESTION label = LIDENT
    {let _loc_label_ = (_startpos_label_, _endpos_label_) in
      ( let loc = _loc_label_ in
        (Optional (label), mkexpvar ~loc (label)) )}
| _1 = OPTLABEL _2 = simple_expr %prec below_HASH
    {      ( (Optional (_1), _2) )}

let_binding_body_no_punning:
  _1 = val_ident _2 = strict_binding
    {let _1 =
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
                ( mkpatvar ~loc:_sloc _1 )
in
      ( (_1, _2) )}
| _1 = val_ident _2 = type_constraint _3 = EQUAL _4 = seq_expr
    {let _1 =
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
                ( mkpatvar ~loc:_sloc _1 )
in
let _endpos = _endpos__4_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let v = _1 in (* PR#7344 *)
        let t =
          match _2 with
            Some t, None -> t
          | _, Some t -> t
          | _ -> assert false
        in
        let loc = Location.(t.ptyp_loc.loc_start, t.ptyp_loc.loc_end) in
        let typ = ghtyp ~loc (Ptyp_poly([],t)) in
        let patloc = (_startpos__1_, _endpos__2_) in
        (ghpat ~loc:patloc (Ppat_constraint(v, typ)),
         mkexp_constraint ~loc:_sloc _4 _2) )}
| _1 = val_ident _2 = COLON xs = reversed_nonempty_llist_typevar_ _2_inlined1 = DOT _3 = core_type _4 = EQUAL _5 = seq_expr
    {let _3 =
  let _1 =
    let _1 =
      let xs =     ( List.rev xs ) in
          ( xs )
    in
        ( _1 )
  in
      ( Ptyp_poly(_1, _3) )
in
let _startpos__3_ = _startpos_xs_ in
let _1 =
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
                ( mkpatvar ~loc:_sloc _1 )
in
let _loc__3_ = (_startpos__3_, _endpos__3_) in
      ( let patloc = (_startpos__1_, _endpos__3_) in
        (ghpat ~loc:patloc
           (Ppat_constraint(_1, ghtyp ~loc:(_loc__3_) _3)),
         _5) )}
| _1 = val_ident _2 = COLON _3 = TYPE xs = nonempty_list_mkrhs_LIDENT__ _5 = DOT _6 = core_type _7 = EQUAL _8 = seq_expr
    {let _4 =     ( xs ) in
let _1 =
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
                ( mkpatvar ~loc:_sloc _1 )
in
let _endpos = _endpos__8_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let exp, poly =
          wrap_type_annotation ~loc:_sloc _4 _6 _8 in
        let loc = (_startpos__1_, _endpos__6_) in
        (ghpat ~loc (Ppat_constraint(_1, poly)), exp) )}
| _1 = pattern_no_exn _2 = EQUAL _3 = seq_expr
    {      ( (_1, _3) )}
| _1 = simple_pattern_not_ident _2 = COLON _3 = core_type _4 = EQUAL _5 = seq_expr
    {      ( let loc = (_startpos__1_, _endpos__3_) in
        (ghpat ~loc (Ppat_constraint(_1, _3)), _5) )}

let_binding_body:
  _1 = let_binding_body_no_punning
    {      ( let p,e = _1 in (p,e,false) )}
| _1 = val_ident %prec below_HASH
    {let _endpos = _endpos__1_ in
let _startpos = _startpos__1_ in
let _loc = (_startpos, _endpos) in
      ( (mkpatvar ~loc:_loc _1, mkexpvar ~loc:_loc _1, true) )}

let_bindings_ext_:
  _1 = LET ext = ext _1_inlined1 = list_attribute_ rec_flag = rec_flag body = let_binding_body _1_inlined2 = list_post_item_attribute_
    {let _1 =
  let attrs2 =
    let _1 = _1_inlined2 in
        ( _1 )
  in
  let _endpos_attrs2_ = _endpos__1_inlined2_ in
  let attrs1 =
    let _1 = _1_inlined1 in
        ( _1 )
  in
  let _endpos = _endpos_attrs2_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      (
      let attrs = attrs1 @ attrs2 in
      mklbs ext rec_flag (mklb ~loc:_sloc true body attrs)
    )
in
                                                ( _1 )}
| _1 = let_bindings_ext_ _2 = and_let_binding
    {                                                ( addlb _1 _2 )}

let_bindings_no_ext_:
  _1 = LET _1_inlined1 = list_attribute_ rec_flag = rec_flag body = let_binding_body _1_inlined2 = list_post_item_attribute_
    {let _1 =
  let attrs2 =
    let _1 = _1_inlined2 in
        ( _1 )
  in
  let _endpos_attrs2_ = _endpos__1_inlined2_ in
  let attrs1 =
    let _1 = _1_inlined1 in
        ( _1 )
  in
  let ext =                     ( None ) in
  let _endpos = _endpos_attrs2_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      (
      let attrs = attrs1 @ attrs2 in
      mklbs ext rec_flag (mklb ~loc:_sloc true body attrs)
    )
in
                                                ( _1 )}
| _1 = LET _1_inlined1 = PERCENT _2 = attr_id _1_inlined2 = list_attribute_ rec_flag = rec_flag body = let_binding_body _1_inlined3 = list_post_item_attribute_
    {let _1 =
  let attrs2 =
    let _1 = _1_inlined3 in
        ( _1 )
  in
  let _endpos_attrs2_ = _endpos__1_inlined3_ in
  let attrs1 =
    let _1 = _1_inlined2 in
        ( _1 )
  in
  let ext =
    let _startpos__1_ = _startpos__1_inlined1_ in
    let _endpos = _endpos__2_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
                        ( not_expecting _loc "extension" )
  in
  let _endpos = _endpos_attrs2_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      (
      let attrs = attrs1 @ attrs2 in
      mklbs ext rec_flag (mklb ~loc:_sloc true body attrs)
    )
in
                                                ( _1 )}
| _1 = let_bindings_no_ext_ _2 = and_let_binding
    {                                                ( addlb _1 _2 )}

and_let_binding:
  _1 = AND _1_inlined1 = list_attribute_ body = let_binding_body _1_inlined2 = list_post_item_attribute_
    {let attrs2 =
  let _1 = _1_inlined2 in
      ( _1 )
in
let _endpos_attrs2_ = _endpos__1_inlined2_ in
let attrs1 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos_attrs2_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    (
      let attrs = attrs1 @ attrs2 in
      mklb ~loc:_sloc false body attrs
    )}

letop_binding_body:
  _1 = val_ident exp = strict_binding
    {let pat =
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
                ( mkpatvar ~loc:_sloc _1 )
in
      ( (pat, exp) )}
| _1 = val_ident
    {let _endpos = _endpos__1_ in
let _startpos = _startpos__1_ in
let _loc = (_startpos, _endpos) in
      ( (mkpatvar ~loc:_loc _1, mkexpvar ~loc:_loc _1) )}
| pat = simple_pattern _2 = COLON typ = core_type _4 = EQUAL exp = seq_expr
    {      ( let loc = (_startpos_pat_, _endpos_typ_) in
        (ghpat ~loc (Ppat_constraint(pat, typ)), exp) )}
| pat = pattern_no_exn _2 = EQUAL exp = seq_expr
    {      ( (pat, exp) )}

letop_bindings:
  body = letop_binding_body
    {      ( let let_pat, let_exp = body in
        let_pat, let_exp, [] )}
| bindings = letop_bindings _1 = ANDOP body = letop_binding_body
    {let pbop_op =
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos = _endpos_body_ in
let _symbolstartpos = _startpos_bindings_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let let_pat, let_exp, rev_ands = bindings in
        let pbop_pat, pbop_exp = body in
        let pbop_loc = make_loc _sloc in
        let and_ = {pbop_op; pbop_pat; pbop_exp; pbop_loc} in
        let_pat, let_exp, and_ :: rev_ands )}

fun_binding:
  _1 = strict_binding
    {      ( _1 )}
| _1 = type_constraint _2 = EQUAL _3 = seq_expr
    {let _endpos = _endpos__3_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp_constraint ~loc:_sloc _3 _1 )}

strict_binding:
  _1 = EQUAL _2 = seq_expr
    {      ( _2 )}
| _1 = labeled_simple_pattern _2 = fun_binding
    {let _endpos = _endpos__2_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let (l, o, p) = _1 in ghexp ~loc:_sloc (Pexp_fun(l, o, p, _2)) )}
| _1 = LPAREN _2 = TYPE xs = nonempty_list_mkrhs_LIDENT__ _4 = RPAREN _5 = fun_binding
    {let _3 =     ( xs ) in
let _endpos = _endpos__5_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mk_newtypes ~loc:_sloc _3 _5 )}

match_case:
  _1 = pattern _2 = MINUSGREATER _3 = seq_expr
    {      ( Exp.case _1 ~guard:(None) _3 )}
| _1 = pattern _2 = WHEN _3 = seq_expr _4 = MINUSGREATER _5 = seq_expr
    {      ( Exp.case _1 ~guard:((Some (_3))) _5 )}
| _1 = pattern _2 = MINUSGREATER _3 = DOT
    {let _loc__3_ = (_startpos__3_, _endpos__3_) in
      ( Exp.case _1 ~guard:(None) ((Exp.unreachable ~loc:(make_loc _loc__3_) ())) )}

fun_def:
  _1 = MINUSGREATER _2 = seq_expr
    {      ( _2 )}
| _1 = COLON _2 = atomic_type _3 = MINUSGREATER _4 = seq_expr
    {let _1 =
  let _1 =       ( Pexp_constraint (_4, _2) ) in
  let _endpos__1_ = _endpos__4_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkexp ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = labeled_simple_pattern _2 = fun_def
    {let _endpos = _endpos__2_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      (
       let (l,o,p) = _1 in
       ghexp ~loc:_sloc (Pexp_fun(l, o, p, _2))
      )}
| _1 = LPAREN _2 = TYPE xs = nonempty_list_mkrhs_LIDENT__ _4 = RPAREN _5 = fun_def
    {let _3 =     ( xs ) in
let _endpos = _endpos__5_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mk_newtypes ~loc:_sloc _3 _5 )}

record_expr_content:
  fields = separated_or_terminated_nonempty_list_SEMI_record_expr_field_
    {let eo =     ( None ) in
    ( eo, fields )}
| x = simple_expr _2 = WITH fields = separated_or_terminated_nonempty_list_SEMI_record_expr_field_
    {let eo =
  let x =     ( x ) in
      ( Some x )
in
    ( eo, fields )}

type_constraint:
  _1 = COLON _2 = core_type
    {                                                ( (Some _2, None) )}
| _1 = COLON _2 = core_type _3 = COLONGREATER _4 = core_type
    {                                                ( (Some _2, Some _4) )}
| _1 = COLONGREATER _2 = core_type
    {                                                ( (None, Some _2) )}
| _1 = COLON _2 = error
    {                                                ( syntax_error() )}
| _1 = COLONGREATER _2 = error
    {                                                ( syntax_error() )}

pattern:
  _1 = pattern _2 = COLONCOLON _3 = pattern
    {let _1 =
  let _endpos = _endpos__3_ in
  let _symbolstartpos = _startpos__1_ in
  let _loc__2_ = (_startpos__2_, _endpos__2_) in
  let _sloc = (_symbolstartpos, _endpos) in
        ( mkpat_cons ~loc:_sloc _loc__2_ (ghpat ~loc:_sloc (Ppat_tuple ([_1;_3]))) )
in
      ( _1 )}
| _1 = pattern _2 = attribute
    {let _1 =       ( Pat.attr _1 _2 ) in
      ( _1 )}
| _1 = pattern_gen
    {let _1 =       ( _1 ) in
      ( _1 )}
| _1 = pattern _2 = AS _1_inlined1 = val_ident
    {let _1 =
  let _1 =
    let _1 =
      let _3 =
        let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
        let _endpos = _endpos__1_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( mkrhs _1 _sloc )
      in
              ( Ppat_alias(_1, _3) )
    in
    let _endpos__1_ = _endpos__1_inlined1_ in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkpat ~loc:_sloc _1 )
  in
      ( _1 )
in
      ( _1 )}
| _1 = pattern _2 = AS _3 = error
    {let _1 =
  let _1 =
    let _1 =
      let _loc__3_ = (_startpos__3_, _endpos__3_) in
              ( expecting _loc__3_ "identifier" )
    in
    let _endpos__1_ = _endpos__3_ in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkpat ~loc:_sloc _1 )
  in
      ( _1 )
in
      ( _1 )}
| _1 = pattern_comma_list_pattern_ %prec below_COMMA
    {let _1 =
  let _1 =
    let _1 =         ( Ppat_tuple((List.rev _1)) ) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkpat ~loc:_sloc _1 )
  in
      ( _1 )
in
      ( _1 )}
| _1 = pattern _2 = COLONCOLON _3 = error
    {let _1 =
  let _1 =
    let _1 =
      let _loc__3_ = (_startpos__3_, _endpos__3_) in
              ( expecting _loc__3_ "pattern" )
    in
    let _endpos__1_ = _endpos__3_ in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkpat ~loc:_sloc _1 )
  in
      ( _1 )
in
      ( _1 )}
| _1 = pattern _2 = BAR _3 = pattern
    {let _1 =
  let _1 =
    let _1 =         ( Ppat_or(_1, _3) ) in
    let _endpos__1_ = _endpos__3_ in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkpat ~loc:_sloc _1 )
  in
      ( _1 )
in
      ( _1 )}
| _1 = pattern _2 = BAR _3 = error
    {let _1 =
  let _1 =
    let _1 =
      let _loc__3_ = (_startpos__3_, _endpos__3_) in
              ( expecting _loc__3_ "pattern" )
    in
    let _endpos__1_ = _endpos__3_ in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkpat ~loc:_sloc _1 )
  in
      ( _1 )
in
      ( _1 )}
| _1 = EXCEPTION _1_inlined1 = ext _1_inlined2 = list_attribute_ _3 = pattern %prec prec_constr_appl
    {let _2 =
  let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
  let _2 =
    let _1 = _1_inlined1 in
        ( _1 )
  in
                      ( _1, _2 )
in
let _endpos = _endpos__3_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat_attrs ~loc:_sloc (Ppat_exception _3) _2)}

pattern_no_exn:
  _1 = pattern_no_exn _2 = COLONCOLON _3 = pattern
    {let _1 =
  let _endpos = _endpos__3_ in
  let _symbolstartpos = _startpos__1_ in
  let _loc__2_ = (_startpos__2_, _endpos__2_) in
  let _sloc = (_symbolstartpos, _endpos) in
        ( mkpat_cons ~loc:_sloc _loc__2_ (ghpat ~loc:_sloc (Ppat_tuple ([_1;_3]))) )
in
      ( _1 )}
| _1 = pattern_no_exn _2 = attribute
    {let _1 =       ( Pat.attr _1 _2 ) in
      ( _1 )}
| _1 = pattern_gen
    {let _1 =       ( _1 ) in
      ( _1 )}
| _1 = pattern_no_exn _2 = AS _1_inlined1 = val_ident
    {let _1 =
  let _1 =
    let _1 =
      let _3 =
        let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
        let _endpos = _endpos__1_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
            ( mkrhs _1 _sloc )
      in
              ( Ppat_alias(_1, _3) )
    in
    let _endpos__1_ = _endpos__1_inlined1_ in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkpat ~loc:_sloc _1 )
  in
      ( _1 )
in
      ( _1 )}
| _1 = pattern_no_exn _2 = AS _3 = error
    {let _1 =
  let _1 =
    let _1 =
      let _loc__3_ = (_startpos__3_, _endpos__3_) in
              ( expecting _loc__3_ "identifier" )
    in
    let _endpos__1_ = _endpos__3_ in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkpat ~loc:_sloc _1 )
  in
      ( _1 )
in
      ( _1 )}
| _1 = pattern_comma_list_pattern_no_exn_ %prec below_COMMA
    {let _1 =
  let _1 =
    let _1 =         ( Ppat_tuple((List.rev _1)) ) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkpat ~loc:_sloc _1 )
  in
      ( _1 )
in
      ( _1 )}
| _1 = pattern_no_exn _2 = COLONCOLON _3 = error
    {let _1 =
  let _1 =
    let _1 =
      let _loc__3_ = (_startpos__3_, _endpos__3_) in
              ( expecting _loc__3_ "pattern" )
    in
    let _endpos__1_ = _endpos__3_ in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkpat ~loc:_sloc _1 )
  in
      ( _1 )
in
      ( _1 )}
| _1 = pattern_no_exn _2 = BAR _3 = pattern
    {let _1 =
  let _1 =
    let _1 =         ( Ppat_or(_1, _3) ) in
    let _endpos__1_ = _endpos__3_ in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkpat ~loc:_sloc _1 )
  in
      ( _1 )
in
      ( _1 )}
| _1 = pattern_no_exn _2 = BAR _3 = error
    {let _1 =
  let _1 =
    let _1 =
      let _loc__3_ = (_startpos__3_, _endpos__3_) in
              ( expecting _loc__3_ "pattern" )
    in
    let _endpos__1_ = _endpos__3_ in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkpat ~loc:_sloc _1 )
  in
      ( _1 )
in
      ( _1 )}

pattern_gen:
  _1 = simple_pattern
    {      ( _1 )}
| _1 = constr_longident _2 = pattern %prec prec_constr_appl
    {let _1 =
  let _1 =
    let _1 =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
            ( Ppat_construct(_1, (Some ([], _2))) )
  in
  let _endpos__1_ = _endpos__2_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = constr_longident _2 = LPAREN _3 = TYPE xs = nonempty_list_mkrhs_LIDENT__ _5 = RPAREN pat = simple_pattern
    {let _1 =
  let _1 =
    let newtypes =     ( xs ) in
    let constr =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
            ( Ppat_construct(constr, (Some (newtypes, pat))) )
  in
  let _endpos__1_ = _endpos_pat_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = name_tag _2 = pattern %prec prec_constr_appl
    {let _1 =
  let _1 =         ( Ppat_variant(_1, (Some _2)) ) in
  let _endpos__1_ = _endpos__2_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LAZY _1_inlined1 = ext _1_inlined2 = list_attribute_ _3 = simple_pattern
    {let _2 =
  let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
  let _2 =
    let _1 = _1_inlined1 in
        ( _1 )
  in
                      ( _1, _2 )
in
let _endpos = _endpos__3_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat_attrs ~loc:_sloc (Ppat_lazy _3) _2)}

simple_pattern:
  _1 = val_ident %prec below_EQUAL
    {let _1 =
  let _1 =
    let _1 =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
          ( Ppat_var (_1) )
  in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = simple_pattern_not_ident
    {                             ( _1 )}

simple_pattern_not_ident:
  _1 = LPAREN _2 = pattern _3 = RPAREN
    {let _endpos = _endpos__3_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( reloc_pat ~loc:_sloc _2 )}
| _1 = simple_delimited_pattern
    {      ( _1 )}
| _1 = LPAREN _2 = MODULE _1_inlined1 = ext _1_inlined2 = list_attribute_ _1_inlined3 = module_name _5 = RPAREN
    {let _4 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _3 =
  let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
  let _2 =
    let _1 = _1_inlined1 in
        ( _1 )
  in
                      ( _1, _2 )
in
let _endpos = _endpos__5_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat_attrs ~loc:_sloc (Ppat_unpack _4) _3 )}
| _1 = LPAREN _2 = MODULE _1_inlined1 = ext _1_inlined2 = list_attribute_ _1_inlined3 = module_name _5 = COLON _1_inlined4 = module_type _7 = RPAREN
    {let _6 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined4_, _startpos__1_inlined4_, _1_inlined4) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
        ( let (lid, cstrs, attrs) = package_type_of_module_type _1 in
        let descr = Ptyp_package (lid, cstrs) in
        mktyp ~loc:_sloc ~attrs descr )
in
let _4 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let (_endpos__4_, _startpos__4_) = (_endpos__1_inlined3_, _startpos__1_inlined3_) in
let _3 =
  let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
  let _2 =
    let _1 = _1_inlined1 in
        ( _1 )
  in
                      ( _1, _2 )
in
let _endpos = _endpos__7_ in
let _symbolstartpos = _startpos__1_ in
let _loc__4_ = (_startpos__4_, _endpos__4_) in
let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat_attrs ~loc:_sloc
          (Ppat_constraint(mkpat ~loc:_loc__4_ (Ppat_unpack _4), _6))
          _3 )}
| _1 = UNDERSCORE
    {let _1 =
  let _1 =       ( Ppat_any ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = signed_constant
    {let _1 =
  let _1 =       ( Ppat_constant _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = signed_constant _2 = DOTDOT _3 = signed_constant
    {let _1 =
  let _1 =       ( Ppat_interval (_1, _3) ) in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = constr_longident
    {let _1 =
  let _1 =
    let _1 =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
          ( Ppat_construct(_1, None) )
  in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = name_tag
    {let _1 =
  let _1 =       ( Ppat_variant(_1, None) ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = HASH _1_inlined1 = type_longident
    {let _1 =
  let _1 =
    let _2 =
      let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
          ( Ppat_type (_2) )
  in
  let _endpos__1_ = _endpos__1_inlined1_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _3 = simple_delimited_pattern
    {let _1 =
  let _1 =
    let _1 =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
          ( Ppat_open(_1, _3) )
  in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _1_inlined1 = LBRACKET _2_inlined1 = RBRACKET
    {let _1 =
  let _1 =
    let _3 =
      let (_endpos__2_, _startpos__1_) = (_endpos__2_inlined1_, _startpos__1_inlined1_) in
      let _1 =                                                      ((Lident ("[]"))) in
      let _endpos__1_ = _endpos__2_ in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
    let _endpos__3_ = _endpos__2_inlined1_ in
    let _1 =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
    let _endpos = _endpos__3_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( Ppat_open(_1, mkpat ~loc:_sloc (Ppat_construct(_3, None))) )
  in
  let _endpos__1_ = _endpos__2_inlined1_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _1_inlined1 = LPAREN _2_inlined1 = RPAREN
    {let _1 =
  let _1 =
    let _3 =
      let (_endpos__2_, _startpos__1_) = (_endpos__2_inlined1_, _startpos__1_inlined1_) in
      let _1 =                                                  ((Lident ("()"))) in
      let _endpos__1_ = _endpos__2_ in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
    let _endpos__3_ = _endpos__2_inlined1_ in
    let _1 =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
    let _endpos = _endpos__3_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( Ppat_open(_1, mkpat ~loc:_sloc (Ppat_construct(_3, None))) )
  in
  let _endpos__1_ = _endpos__2_inlined1_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _3 = LPAREN _4 = pattern _5 = RPAREN
    {let _1 =
  let _1 =
    let _1 =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
          ( Ppat_open (_1, _4) )
  in
  let _endpos__1_ = _endpos__5_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _3 = LPAREN _4 = pattern _5 = error
    {let _1 =
  let _1 =
    let _loc__5_ = (_startpos__5_, _endpos__5_) in
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
          ( unclosed "(" _loc__3_ ")" _loc__5_  )
  in
  let _endpos__1_ = _endpos__5_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = mod_longident _2 = DOT _3 = LPAREN _4 = error
    {let _1 =
  let _1 =
    let _loc__4_ = (_startpos__4_, _endpos__4_) in
          ( expecting _loc__4_ "pattern" )
  in
  let _endpos__1_ = _endpos__4_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LPAREN _2 = pattern _3 = error
    {let _1 =
  let _1 =
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
    let _loc__1_ = (_startpos__1_, _endpos__1_) in
          ( unclosed "(" _loc__1_ ")" _loc__3_ )
  in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LPAREN _2 = pattern _3 = COLON _4 = core_type _5 = RPAREN
    {let _1 =
  let _1 =       ( Ppat_constraint(_2, _4) ) in
  let _endpos__1_ = _endpos__5_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LPAREN _2 = pattern _3 = COLON _4 = core_type _5 = error
    {let _1 =
  let _1 =
    let _loc__5_ = (_startpos__5_, _endpos__5_) in
    let _loc__1_ = (_startpos__1_, _endpos__1_) in
          ( unclosed "(" _loc__1_ ")" _loc__5_ )
  in
  let _endpos__1_ = _endpos__5_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LPAREN _2 = pattern _3 = COLON _4 = error
    {let _1 =
  let _1 =
    let _loc__4_ = (_startpos__4_, _endpos__4_) in
          ( expecting _loc__4_ "type" )
  in
  let _endpos__1_ = _endpos__4_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = LPAREN _2 = MODULE _1_inlined1 = ext _1_inlined2 = list_attribute_ _4 = module_name _5 = COLON _1_inlined3 = module_type _7 = error
    {let _1 =
  let _1 =
    let _6 =
      let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
            ( let (lid, cstrs, attrs) = package_type_of_module_type _1 in
        let descr = Ptyp_package (lid, cstrs) in
        mktyp ~loc:_sloc ~attrs descr )
    in
    let _3 =
      let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
      let _2 =
        let _1 = _1_inlined1 in
            ( _1 )
      in
                          ( _1, _2 )
    in
    let _loc__7_ = (_startpos__7_, _endpos__7_) in
    let _loc__1_ = (_startpos__1_, _endpos__1_) in
          ( unclosed "(" _loc__1_ ")" _loc__7_ )
  in
  let _endpos__1_ = _endpos__7_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}
| _1 = extension
    {let _1 =
  let _1 =       ( Ppat_extension _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
      ( _1 )}

simple_delimited_pattern:
  _1 = LBRACE _1_inlined1 = listx_SEMI_record_pat_field_UNDERSCORE_ _3 = RBRACE
    {let _1 =
  let _1 =
    let _2 =
      let _1 = _1_inlined1 in
          ( let fields, closed = _1 in
      let closed = match closed with Some () -> Open | None -> Closed in
      fields, closed )
    in
          ( let (fields, closed) = _2 in
        Ppat_record(fields, closed) )
  in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = LBRACE _1_inlined1 = listx_SEMI_record_pat_field_UNDERSCORE_ _3 = error
    {let _1 =
  let _1 =
    let _2 =
      let _1 = _1_inlined1 in
          ( let fields, closed = _1 in
      let closed = match closed with Some () -> Open | None -> Closed in
      fields, closed )
    in
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
    let _loc__1_ = (_startpos__1_, _endpos__1_) in
          ( unclosed "{" _loc__1_ "}" _loc__3_ )
  in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = LBRACKET ps = separated_or_terminated_nonempty_list_SEMI_pattern_ _3 = RBRACKET
    {let _1 =
  let _1 =
    let _2 =     ( ps ) in
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
          ( fst (mktailpat _loc__3_ _2) )
  in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = LBRACKET ps = separated_or_terminated_nonempty_list_SEMI_pattern_ _3 = error
    {let _1 =
  let _1 =
    let _2 =     ( ps ) in
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
    let _loc__1_ = (_startpos__1_, _endpos__1_) in
          ( unclosed "[" _loc__1_ "]" _loc__3_ )
  in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = LBRACKETBAR ps = separated_or_terminated_nonempty_list_SEMI_pattern_ _3 = BARRBRACKET
    {let _1 =
  let _1 =
    let _2 =     ( ps ) in
          ( Ppat_array _2 )
  in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = LBRACKETBAR _2 = BARRBRACKET
    {let _1 =
  let _1 =       ( Ppat_array ([]) ) in
  let _endpos__1_ = _endpos__2_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = LBRACKETBAR ps = separated_or_terminated_nonempty_list_SEMI_pattern_ _3 = error
    {let _1 =
  let _1 =
    let _2 =     ( ps ) in
    let _loc__3_ = (_startpos__3_, _endpos__3_) in
    let _loc__1_ = (_startpos__1_, _endpos__1_) in
          ( unclosed "[|" _loc__1_ "|]" _loc__3_ )
  in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkpat ~loc:_sloc _1 )
in
    ( _1 )}

pattern_comma_list_pattern_:
  _1 = pattern_comma_list_pattern_ _2 = COMMA _3 = pattern
    {                                                ( _3 :: _1 )}
| _1 = pattern _2 = COMMA _3 = pattern
    {                                                ( [_3; _1] )}
| _1 = pattern _2 = COMMA _3 = error
    {let _loc__3_ = (_startpos__3_, _endpos__3_) in
                                                ( expecting _loc__3_ "pattern" )}

pattern_comma_list_pattern_no_exn_:
  _1 = pattern_comma_list_pattern_no_exn_ _2 = COMMA _3 = pattern
    {                                                ( _3 :: _1 )}
| _1 = pattern_no_exn _2 = COMMA _3 = pattern
    {                                                ( [_3; _1] )}
| _1 = pattern_no_exn _2 = COMMA _3 = error
    {let _loc__3_ = (_startpos__3_, _endpos__3_) in
                                                ( expecting _loc__3_ "pattern" )}

value_description:
  _1 = VAL ext = ext _1_inlined1 = list_attribute_ _1_inlined2 = val_ident _5 = COLON ty = possibly_poly_core_type_ _1_inlined3 = list_post_item_attribute_
    {let attrs2 =
  let _1 = _1_inlined3 in
      ( _1 )
in
let _endpos_attrs2_ = _endpos__1_inlined3_ in
let id =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let attrs1 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos_attrs2_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( let attrs = attrs1 @ attrs2 in
      let loc = make_loc _sloc in
      let docs = symbol_docs _sloc in
      Val.mk id ty ~attrs ~loc ~docs,
      ext )}

primitive_declaration:
  _1 = EXTERNAL ext = ext _1_inlined1 = list_attribute_ _1_inlined2 = val_ident _5 = COLON ty = possibly_poly_core_type_ _7 = EQUAL prim = nonempty_list_raw_string_ _1_inlined3 = list_post_item_attribute_
    {let attrs2 =
  let _1 = _1_inlined3 in
      ( _1 )
in
let _endpos_attrs2_ = _endpos__1_inlined3_ in
let id =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let attrs1 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos_attrs2_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( let attrs = attrs1 @ attrs2 in
      let loc = make_loc _sloc in
      let docs = symbol_docs _sloc in
      Val.mk id ty ~prim ~attrs ~loc ~docs,
      ext )}

nonempty_type_kind:
  ty = core_type
    {let priv =                                                 ( Public ) in
      ( (Ptype_abstract, priv, (Some ty)) )}
| _1 = PRIVATE ty = core_type
    {let priv =                                                 ( Private ) in
      ( (Ptype_abstract, priv, (Some ty)) )}
| cs = constructor_declarations
    {let priv =                                                 ( Public ) in
let oty =
  let _1 =     ( None ) in
      ( _1 )
in
      ( (Ptype_variant cs, priv, oty) )}
| _1 = PRIVATE cs = constructor_declarations
    {let priv =                                                 ( Private ) in
let oty =
  let _1 =     ( None ) in
      ( _1 )
in
      ( (Ptype_variant cs, priv, oty) )}
| x = core_type _2 = EQUAL cs = constructor_declarations
    {let priv =                                                 ( Public ) in
let oty =
  let _1 =
    let x =     ( x ) in
        ( Some x )
  in
      ( _1 )
in
      ( (Ptype_variant cs, priv, oty) )}
| x = core_type _2 = EQUAL _1 = PRIVATE cs = constructor_declarations
    {let priv =                                                 ( Private ) in
let oty =
  let _1 =
    let x =     ( x ) in
        ( Some x )
  in
      ( _1 )
in
      ( (Ptype_variant cs, priv, oty) )}
| _3 = DOTDOT
    {let priv =                                                 ( Public ) in
let oty =
  let _1 =     ( None ) in
      ( _1 )
in
      ( (Ptype_open, priv, oty) )}
| _1 = PRIVATE _3 = DOTDOT
    {let priv =                                                 ( Private ) in
let oty =
  let _1 =     ( None ) in
      ( _1 )
in
      ( (Ptype_open, priv, oty) )}
| x = core_type _2 = EQUAL _3 = DOTDOT
    {let priv =                                                 ( Public ) in
let oty =
  let _1 =
    let x =     ( x ) in
        ( Some x )
  in
      ( _1 )
in
      ( (Ptype_open, priv, oty) )}
| x = core_type _2 = EQUAL _1 = PRIVATE _3 = DOTDOT
    {let priv =                                                 ( Private ) in
let oty =
  let _1 =
    let x =     ( x ) in
        ( Some x )
  in
      ( _1 )
in
      ( (Ptype_open, priv, oty) )}
| _3 = LBRACE ls = label_declarations _5 = RBRACE
    {let priv =                                                 ( Public ) in
let oty =
  let _1 =     ( None ) in
      ( _1 )
in
      ( (Ptype_record ls, priv, oty) )}
| _1 = PRIVATE _3 = LBRACE ls = label_declarations _5 = RBRACE
    {let priv =                                                 ( Private ) in
let oty =
  let _1 =     ( None ) in
      ( _1 )
in
      ( (Ptype_record ls, priv, oty) )}
| x = core_type _2 = EQUAL _3 = LBRACE ls = label_declarations _5 = RBRACE
    {let priv =                                                 ( Public ) in
let oty =
  let _1 =
    let x =     ( x ) in
        ( Some x )
  in
      ( _1 )
in
      ( (Ptype_record ls, priv, oty) )}
| x = core_type _2 = EQUAL _1 = PRIVATE _3 = LBRACE ls = label_declarations _5 = RBRACE
    {let priv =                                                 ( Private ) in
let oty =
  let _1 =
    let x =     ( x ) in
        ( Some x )
  in
      ( _1 )
in
      ( (Ptype_record ls, priv, oty) )}

type_kind:
  
    {      ( (Ptype_abstract, Public, None) )}
| _1 = EQUAL _2 = nonempty_type_kind
    {      ( _2 )}

type_parameters:
  
    {      ( [] )}
| p = type_parameter
    {      ( [p] )}
| _1 = LPAREN xs = reversed_separated_nonempty_llist_COMMA_type_parameter_ _3 = RPAREN
    {let ps =
  let xs =     ( List.rev xs ) in
      ( xs )
in
      ( ps )}

type_parameter:
  _1 = type_variance _2 = type_variable
    {                                       ( _2, _1 )}

type_variable:
  _1 = QUOTE tyvar = ident
    {let _1 =
  let _1 =       ( Ptyp_var tyvar ) in
  let _endpos__1_ = _endpos_tyvar_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = UNDERSCORE
    {let _1 =
  let _1 =       ( Ptyp_any ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
    ( _1 )}

type_variance:
  
    {                                            ( NoVariance, NoInjectivity )}
| _1 = PLUS
    {                                            ( Covariant, NoInjectivity )}
| _1 = MINUS
    {                                            ( Contravariant, NoInjectivity )}
| _1 = BANG
    {                                            ( NoVariance, Injective )}
| _1 = PLUS _2 = BANG
    {                                            ( Covariant, Injective )}
| _1 = BANG _2 = PLUS
    {                                            ( Covariant, Injective )}
| _1 = MINUS _2 = BANG
    {                                            ( Contravariant, Injective )}
| _1 = BANG _2 = MINUS
    {                                            ( Contravariant, Injective )}
| _1 = INFIXOP2
    {let _loc__1_ = (_startpos__1_, _endpos__1_) in
      ( if _1 = "+!" then Covariant, Injective else
        if _1 = "-!" then Contravariant, Injective else
        expecting _loc__1_ "type_variance" )}
| _1 = PREFIXOP
    {let _loc__1_ = (_startpos__1_, _endpos__1_) in
      ( if _1 = "!+" then Covariant, Injective else
        if _1 = "!-" then Contravariant, Injective else
        expecting _loc__1_ "type_variance" )}

constructor_declarations:
  _1 = BAR
    {      ( [] )}
| xs = reversed_bar_llist_constructor_declaration_
    {let cs =     ( List.rev xs ) in
      ( cs )}

generic_constructor_declaration_BAR_:
  _1 = BAR _1_inlined1 = constr_ident vars_args_res = generalized_constructor_arguments _1_inlined2 = list_attribute_
    {let attrs =
  let _1 = _1_inlined2 in
      ( _1 )
in
let _endpos_attrs_ = _endpos__1_inlined2_ in
let cid =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos = _endpos_attrs_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    (
      let vars, args, res = vars_args_res in
      let info = symbol_info _endpos in
      let loc = make_loc _sloc in
      cid, vars, args, res, attrs, loc, info
    )}

generic_constructor_declaration_epsilon_:
  _1 = constr_ident vars_args_res = generalized_constructor_arguments _1_inlined1 = list_attribute_
    {let attrs =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos_attrs_ = _endpos__1_inlined1_ in
let cid =
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _startpos_cid_ = _startpos__1_ in
let _1 =     ( () ) in
let _endpos = _endpos_attrs_ in
let _symbolstartpos = _startpos_cid_ in
let _sloc = (_symbolstartpos, _endpos) in
    (
      let vars, args, res = vars_args_res in
      let info = symbol_info _endpos in
      let loc = make_loc _sloc in
      cid, vars, args, res, attrs, loc, info
    )}

str_exception_declaration:
  _1 = sig_exception_declaration
    {    ( _1 )}
| _1 = EXCEPTION ext = ext _1_inlined1 = list_attribute_ _1_inlined2 = constr_ident _5 = EQUAL _1_inlined3 = constr_longident _1_inlined4 = list_attribute_ _1_inlined5 = list_post_item_attribute_
    {let attrs =
  let _1 = _1_inlined5 in
      ( _1 )
in
let _endpos_attrs_ = _endpos__1_inlined5_ in
let attrs2 =
  let _1 = _1_inlined4 in
      ( _1 )
in
let lid =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let id =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let attrs1 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos_attrs_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
  ( let loc = make_loc _sloc in
    let docs = symbol_docs _sloc in
    Te.mk_exception ~attrs
      (Te.rebind id lid ~attrs:(attrs1 @ attrs2) ~loc ~docs)
    , ext )}

sig_exception_declaration:
  _1 = EXCEPTION ext = ext _1_inlined1 = list_attribute_ _1_inlined2 = constr_ident vars_args_res = generalized_constructor_arguments _1_inlined3 = list_attribute_ _1_inlined4 = list_post_item_attribute_
    {let attrs =
  let _1 = _1_inlined4 in
      ( _1 )
in
let _endpos_attrs_ = _endpos__1_inlined4_ in
let attrs2 =
  let _1 = _1_inlined3 in
      ( _1 )
in
let _endpos_attrs2_ = _endpos__1_inlined3_ in
let id =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let attrs1 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos = _endpos_attrs_ in
let _startpos = _startpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( let vars, args, res = vars_args_res in
      let loc = make_loc (_startpos, _endpos_attrs2_) in
      let docs = symbol_docs _sloc in
      Te.mk_exception ~attrs
        (Te.decl id ~vars ~args ~res ~attrs:(attrs1 @ attrs2) ~loc ~docs)
      , ext )}

generalized_constructor_arguments:
  
    {                                  ( ([],Pcstr_tuple ([]),None) )}
| _1 = OF _2 = constructor_arguments
    {                                  ( ([],_2,None) )}
| _1 = COLON _2 = constructor_arguments _3 = MINUSGREATER _4 = atomic_type %prec below_HASH
    {                                  ( ([],_2,(Some _4)) )}
| _1 = COLON xs = reversed_nonempty_llist_typevar_ _3 = DOT _4 = constructor_arguments _5 = MINUSGREATER _6 = atomic_type %prec below_HASH
    {let _2 =
  let _1 =
    let xs =     ( List.rev xs ) in
        ( xs )
  in
      ( _1 )
in
                                  ( (_2,_4,(Some _6)) )}
| _1 = COLON _2 = atomic_type %prec below_HASH
    {                                  ( ([],Pcstr_tuple ([]),(Some _2)) )}
| _1 = COLON xs = reversed_nonempty_llist_typevar_ _3 = DOT _4 = atomic_type %prec below_HASH
    {let _2 =
  let _1 =
    let xs =     ( List.rev xs ) in
        ( xs )
  in
      ( _1 )
in
                                  ( (_2,Pcstr_tuple ([]),(Some _4)) )}

constructor_arguments:
  x = atomic_type %prec below_HASH
    {let tys =
  let xs =
    let xs =     ( [ x ] ) in
        ( List.rev xs )
  in
      ( xs )
in
      ( Pcstr_tuple tys )}
| xs = reversed_separated_nonempty_llist_STAR_atomic_type_ _2 = STAR x = atomic_type %prec below_HASH
    {let tys =
  let xs =
    let xs =     ( x :: xs ) in
        ( List.rev xs )
  in
      ( xs )
in
      ( Pcstr_tuple tys )}
| _1 = LBRACE _2 = label_declarations _3 = RBRACE
    {      ( Pcstr_record _2 )}

label_declarations:
  _1 = label_declaration
    {                                                ( [_1] )}
| _1 = label_declaration_semi
    {                                                ( [_1] )}
| _1 = label_declaration_semi _2 = label_declarations
    {                                                ( _1 :: _2 )}

label_declaration:
  _1 = mutable_flag _1_inlined1 = LIDENT _3 = COLON _1_inlined2 = possibly_poly_core_type_no_attr_ _1_inlined3 = list_attribute_
    {let _5 =
  let _1 = _1_inlined3 in
      ( _1 )
in
let _endpos__5_ = _endpos__1_inlined3_ in
let _4 =
  let _1 = _1_inlined2 in
      ( _1 )
in
let _2 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _1 =                                                 ( _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _startpos__2_ = _startpos__1_inlined1_ in
let _endpos = _endpos__5_ in
let _symbolstartpos = if _startpos__1_ != _endpos__1_ then
  _startpos__1_
else
  _startpos__2_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let info = symbol_info _endpos in
        Type.field _2 _4 ~mut:_1 ~attrs:_5 ~loc:(make_loc _sloc) ~info )}

label_declaration_semi:
  _1 = mutable_flag _1_inlined1 = LIDENT _3 = COLON _1_inlined2 = possibly_poly_core_type_no_attr_ _1_inlined3 = list_attribute_ _6 = SEMI _1_inlined4 = list_attribute_
    {let _7 =
  let _1 = _1_inlined4 in
      ( _1 )
in
let _endpos__7_ = _endpos__1_inlined4_ in
let _5 =
  let _1 = _1_inlined3 in
      ( _1 )
in
let _endpos__5_ = _endpos__1_inlined3_ in
let _4 =
  let _1 = _1_inlined2 in
      ( _1 )
in
let _2 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _1 =                                                 ( _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _startpos__2_ = _startpos__1_inlined1_ in
let _endpos = _endpos__7_ in
let _symbolstartpos = if _startpos__1_ != _endpos__1_ then
  _startpos__1_
else
  _startpos__2_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let info =
          match rhs_info _endpos__5_ with
          | Some _ as info_before_semi -> info_before_semi
          | None -> symbol_info _endpos
       in
       Type.field _2 _4 ~mut:_1 ~attrs:(_5 @ _7) ~loc:(make_loc _sloc) ~info )}

extension_constructor_rebind_BAR_:
  _1 = BAR _1_inlined1 = constr_ident _3 = EQUAL _1_inlined2 = constr_longident _1_inlined3 = list_attribute_
    {let attrs =
  let _1 = _1_inlined3 in
      ( _1 )
in
let _endpos_attrs_ = _endpos__1_inlined3_ in
let lid =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let cid =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos = _endpos_attrs_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let info = symbol_info _endpos in
        Te.rebind cid lid ~attrs ~loc:(make_loc _sloc) ~info )}

extension_constructor_rebind_epsilon_:
  _1 = constr_ident _3 = EQUAL _1_inlined1 = constr_longident _1_inlined2 = list_attribute_
    {let attrs =
  let _1 = _1_inlined2 in
      ( _1 )
in
let _endpos_attrs_ = _endpos__1_inlined2_ in
let lid =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let cid =
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _startpos_cid_ = _startpos__1_ in
let _1 =     ( () ) in
let _endpos = _endpos_attrs_ in
let _symbolstartpos = _startpos_cid_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let info = symbol_info _endpos in
        Te.rebind cid lid ~attrs ~loc:(make_loc _sloc) ~info )}

with_constraint:
  _1 = TYPE _2 = type_parameters _1_inlined1 = label_longident _4 = with_type_binder _1_inlined2 = alias_type xs = reversed_llist_preceded_CONSTRAINT_constrain__
    {let _6 =
  let _1 =
    let xs =     ( List.rev xs ) in
        ( xs )
  in
      ( _1 )
in
let _endpos__6_ = _endpos_xs_ in
let _5 =
  let _1 = _1_inlined2 in
      ( _1 )
in
let _3 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos = _endpos__6_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let lident = loc_last _3 in
        Pwith_type
          (_3,
           (Type.mk lident
              ~params:_2
              ~cstrs:_6
              ~manifest:((Some (_5)))
              ~priv:_4
              ~loc:(make_loc _sloc))) )}
| _1 = TYPE _2 = type_parameters _1_inlined1 = label_longident _4 = COLONEQUAL _1_inlined2 = alias_type
    {let _5 =
  let _1 = _1_inlined2 in
      ( _1 )
in
let _endpos__5_ = _endpos__1_inlined2_ in
let _3 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos = _endpos__5_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let lident = loc_last _3 in
        Pwith_typesubst
         (_3,
           (Type.mk (lident)
              ~params:_2
              ~manifest:((Some (_5)))
              ~loc:(make_loc _sloc))) )}
| _1 = MODULE _1_inlined1 = mod_longident _3 = EQUAL _1_inlined2 = mod_ext_longident
    {let _4 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _2 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
      ( Pwith_module (_2, _4) )}
| _1 = MODULE _1_inlined1 = mod_longident _3 = COLONEQUAL _1_inlined2 = mod_ext_longident
    {let _4 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _2 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
      ( Pwith_modsubst (_2, _4) )}
| _1 = MODULE _2 = TYPE _1_inlined1 = mty_longident _4 = EQUAL rhs = module_type
    {let l =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
      ( Pwith_modtype (l, rhs) )}
| _1 = MODULE _2 = TYPE _1_inlined1 = mty_longident _4 = COLONEQUAL rhs = module_type
    {let l =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
      ( Pwith_modtypesubst (l, rhs) )}

with_type_binder:
  _1 = EQUAL
    {                   ( Public )}
| _1 = EQUAL _2 = PRIVATE
    {                   ( Private )}

possibly_poly_core_type_:
  _1 = core_type
    {    ( _1 )}
| xs = reversed_nonempty_llist_typevar_ _2 = DOT _3 = core_type
    {let _1 =
  let _1 =
    let _1 =
      let _1 =
        let xs =     ( List.rev xs ) in
            ( xs )
      in
          ( _1 )
    in
        ( Ptyp_poly(_1, _3) )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos__3_, _startpos_xs_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
    ( _1 )}

possibly_poly_core_type_no_attr_:
  _1 = alias_type
    {let _1 =     ( _1 ) in
    ( _1 )}
| xs = reversed_nonempty_llist_typevar_ _2 = DOT _1 = alias_type
    {let _1 =
  let _1 =
    let _3 =     ( _1 ) in
    let _1 =
      let _1 =
        let xs =     ( List.rev xs ) in
            ( xs )
      in
          ( _1 )
    in
        ( Ptyp_poly(_1, _3) )
  in
  let _startpos__1_ = _startpos_xs_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
    ( _1 )}

core_type:
  _1 = alias_type
    {let _1 =     ( _1 ) in
      ( _1 )}
| _1 = core_type _2 = attribute
    {      ( Typ.attr _1 _2 )}

alias_type:
  _1 = function_type
    {      ( _1 )}
| ty = alias_type _2 = AS _3 = QUOTE tyvar = ident
    {let _1 =
  let _1 =         ( Ptyp_alias(ty, tyvar) ) in
  let (_endpos__1_, _startpos__1_) = (_endpos_tyvar_, _startpos_ty_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
    ( _1 )}

function_type:
  ty = tuple_type %prec MINUSGREATER
    {      ( ty )}
| label = optlabel _1 = tuple_type _3 = MINUSGREATER codomain = function_type
    {let _1 =
  let _1 =
    let domain =                               ( extra_rhs_core_type _1 ~pos:_endpos__1_ ) in
    let label =       ( Optional label ) in
            ( Ptyp_arrow(label, domain, codomain) )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_codomain_, _startpos_label_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
    ( _1 )}
| label = LIDENT _2 = COLON _1 = tuple_type _3 = MINUSGREATER codomain = function_type
    {let _1 =
  let _1 =
    let domain =                               ( extra_rhs_core_type _1 ~pos:_endpos__1_ ) in
    let label =       ( Labelled label ) in
            ( Ptyp_arrow(label, domain, codomain) )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_codomain_, _startpos_label_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
    ( _1 )}
| _1 = tuple_type _3 = MINUSGREATER codomain = function_type
    {let _1 =
  let _1 =
    let domain =                               ( extra_rhs_core_type _1 ~pos:_endpos__1_ ) in
    let label =       ( Nolabel ) in
            ( Ptyp_arrow(label, domain, codomain) )
  in
  let _endpos__1_ = _endpos_codomain_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
    ( _1 )}

tuple_type:
  ty = atomic_type %prec below_HASH
    {      ( ty )}
| xs = reversed_separated_nontrivial_llist_STAR_atomic_type_
    {let _1 =
  let _1 =
    let tys =
      let xs =     ( List.rev xs ) in
          ( xs )
    in
            ( Ptyp_tuple (tys) )
  in
  let (_endpos__1_, _startpos__1_) = (_endpos_xs_, _startpos_xs_) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
    ( _1 )}

atomic_type:
  _1 = LPAREN _2 = core_type _3 = RPAREN
    {      ( _2 )}
| _1 = LPAREN _2 = MODULE _1_inlined1 = ext _1_inlined2 = list_attribute_ _1_inlined3 = module_type _5 = RPAREN
    {let _4 =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined3_, _startpos__1_inlined3_, _1_inlined3) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
        ( let (lid, cstrs, attrs) = package_type_of_module_type _1 in
        let descr = Ptyp_package (lid, cstrs) in
        mktyp ~loc:_sloc ~attrs descr )
in
let _3 =
  let (_1_inlined1, _1) = (_1_inlined2, _1_inlined1) in
  let _2 =
    let _1 = _1_inlined1 in
        ( _1 )
  in
                      ( _1, _2 )
in
let _endpos = _endpos__5_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( wrap_typ_attrs ~loc:_sloc (reloc_typ ~loc:_sloc _4) _3 )}
| _1 = QUOTE _2 = ident
    {let _1 =
  let _1 =         ( Ptyp_var _2 ) in
  let _endpos__1_ = _endpos__2_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
  ( _1 )}
| _1 = UNDERSCORE
    {let _1 =
  let _1 =         ( Ptyp_any ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
  ( _1 )}
| _1 = type_longident
    {let _1 =
  let _1 =
    let tid =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
    let tys =       ( [] ) in
            ( Ptyp_constr(tid, tys) )
  in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
  ( _1 )}
| ty = atomic_type _1 = type_longident
    {let _1 =
  let _1 =
    let tid =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
    let tys =       ( [ty] ) in
            ( Ptyp_constr(tid, tys) )
  in
  let _startpos__1_ = _startpos_ty_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
  ( _1 )}
| _1 = LPAREN xs = reversed_separated_nontrivial_llist_COMMA_core_type_ _3 = RPAREN _1_inlined1 = type_longident
    {let _1 =
  let _1 =
    let tid =
      let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
    let tys =
      let tys =
        let xs =     ( List.rev xs ) in
            ( xs )
      in
            ( tys )
    in
            ( Ptyp_constr(tid, tys) )
  in
  let _endpos__1_ = _endpos__1_inlined1_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
  ( _1 )}
| _1 = LESS _2 = meth_list _3 = GREATER
    {let _1 =
  let _1 =         ( let (f, c) = _2 in Ptyp_object (f, c) ) in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
  ( _1 )}
| _1 = LESS _2 = GREATER
    {let _1 =
  let _1 =         ( Ptyp_object ([], Closed) ) in
  let _endpos__1_ = _endpos__2_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
  ( _1 )}
| _2 = HASH _1 = clty_longident
    {let _1 =
  let _1 =
    let cid =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
    let tys =       ( [] ) in
            ( Ptyp_class(cid, tys) )
  in
  let _startpos__1_ = _startpos__2_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
  ( _1 )}
| ty = atomic_type _2 = HASH _1 = clty_longident
    {let _1 =
  let _1 =
    let cid =
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
    let tys =       ( [ty] ) in
            ( Ptyp_class(cid, tys) )
  in
  let _startpos__1_ = _startpos_ty_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
  ( _1 )}
| _1 = LPAREN xs = reversed_separated_nontrivial_llist_COMMA_core_type_ _3 = RPAREN _2 = HASH _1_inlined1 = clty_longident
    {let _1 =
  let _1 =
    let cid =
      let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
      let _endpos = _endpos__1_ in
      let _symbolstartpos = _startpos__1_ in
      let _sloc = (_symbolstartpos, _endpos) in
          ( mkrhs _1 _sloc )
    in
    let tys =
      let tys =
        let xs =     ( List.rev xs ) in
            ( xs )
      in
            ( tys )
    in
            ( Ptyp_class(cid, tys) )
  in
  let _endpos__1_ = _endpos__1_inlined1_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
  ( _1 )}
| _1 = LBRACKET _2 = tag_field _3 = RBRACKET
    {let _1 =
  let _1 =         ( Ptyp_variant([_2], Closed, None) ) in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
  ( _1 )}
| _1 = LBRACKET _2 = BAR xs = reversed_separated_nonempty_llist_BAR_row_field_ _4 = RBRACKET
    {let _1 =
  let _1 =
    let _3 =
      let _1 =
        let xs =     ( List.rev xs ) in
            ( xs )
      in
          ( _1 )
    in
            ( Ptyp_variant(_3, Closed, None) )
  in
  let _endpos__1_ = _endpos__4_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
  ( _1 )}
| _1 = LBRACKET _2 = row_field _3 = BAR xs = reversed_separated_nonempty_llist_BAR_row_field_ _5 = RBRACKET
    {let _1 =
  let _1 =
    let _4 =
      let _1 =
        let xs =     ( List.rev xs ) in
            ( xs )
      in
          ( _1 )
    in
            ( Ptyp_variant((_2 :: _4), Closed, None) )
  in
  let _endpos__1_ = _endpos__5_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
  ( _1 )}
| _1 = LBRACKETGREATER _2 = option_BAR_ xs = reversed_separated_nonempty_llist_BAR_row_field_ _4 = RBRACKET
    {let _1 =
  let _1 =
    let _3 =
      let _1 =
        let xs =     ( List.rev xs ) in
            ( xs )
      in
          ( _1 )
    in
            ( Ptyp_variant(_3, Open, None) )
  in
  let _endpos__1_ = _endpos__4_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
  ( _1 )}
| _1 = LBRACKETGREATER _2 = RBRACKET
    {let _1 =
  let _1 =         ( Ptyp_variant([], Open, None) ) in
  let _endpos__1_ = _endpos__2_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
  ( _1 )}
| _1 = LBRACKETLESS _2 = option_BAR_ xs = reversed_separated_nonempty_llist_BAR_row_field_ _4 = RBRACKET
    {let _1 =
  let _1 =
    let _3 =
      let _1 =
        let xs =     ( List.rev xs ) in
            ( xs )
      in
          ( _1 )
    in
            ( Ptyp_variant(_3, Closed, (Some ([]))) )
  in
  let _endpos__1_ = _endpos__4_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
  ( _1 )}
| _1 = LBRACKETLESS _2 = option_BAR_ xs = reversed_separated_nonempty_llist_BAR_row_field_ _4 = GREATER xs_inlined1 = reversed_nonempty_llist_name_tag_ _6 = RBRACKET
    {let _1 =
  let _1 =
    let _5 =
      let xs = xs_inlined1 in
      let _1 =
        let xs =     ( List.rev xs ) in
            ( xs )
      in
          ( _1 )
    in
    let _3 =
      let _1 =
        let xs =     ( List.rev xs ) in
            ( xs )
      in
          ( _1 )
    in
            ( Ptyp_variant(_3, Closed, (Some _5)) )
  in
  let _endpos__1_ = _endpos__6_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
  ( _1 )}
| _1 = extension
    {let _1 =
  let _1 =         ( Ptyp_extension _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mktyp ~loc:_sloc _1 )
in
  ( _1 )}

row_field:
  _1 = tag_field
    {      ( _1 )}
| _1 = core_type
    {let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( Rf.inherit_ ~loc:(make_loc _sloc) _1 )}

tag_field:
  _1 = name_tag _2 = OF _3 = opt_ampersand xs = reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ _1_inlined1 = list_attribute_
    {let _5 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos__5_ = _endpos__1_inlined1_ in
let _4 =
  let _1 =
    let xs =     ( List.rev xs ) in
        ( xs )
  in
      ( _1 )
in
let _1 =
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos = _endpos__5_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let info = symbol_info _endpos in
        let attrs = add_info_attrs info _5 in
        Rf.tag ~loc:(make_loc _sloc) ~attrs _1 _3 _4 )}
| _1 = name_tag _1_inlined1 = list_attribute_
    {let _2 =
  let _1 = _1_inlined1 in
      ( _1 )
in
let _endpos__2_ = _endpos__1_inlined1_ in
let _1 =
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos = _endpos__2_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( let info = symbol_info _endpos in
        let attrs = add_info_attrs info _2 in
        Rf.tag ~loc:(make_loc _sloc) ~attrs _1 (true) ([]) )}

opt_ampersand:
  _1 = AMPERSAND
    {                                                ( true )}
| 
    {                                                ( false )}

meth_list:
  _1 = LIDENT _2 = COLON _1_inlined1 = possibly_poly_core_type_no_attr_ _1_inlined2 = list_attribute_ _5 = SEMI _1_inlined3 = list_attribute_ tail = meth_list
    {let head =
  let _6 =
    let _1 = _1_inlined3 in
        ( _1 )
  in
  let _endpos__6_ = _endpos__1_inlined3_ in
  let _4 =
    let _1 = _1_inlined2 in
        ( _1 )
  in
  let _endpos__4_ = _endpos__1_inlined2_ in
  let _3 =
    let _1 = _1_inlined1 in
        ( _1 )
  in
  let _1 =
    let _1 =                                                 ( _1 ) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let _endpos = _endpos__6_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( let info =
        match rhs_info _endpos__4_ with
        | Some _ as info_before_semi -> info_before_semi
        | None -> symbol_info _endpos
      in
      let attrs = add_info_attrs info (_4 @ _6) in
      Of.tag ~loc:(make_loc _sloc) ~attrs _1 _3 )
in
      ( let (f, c) = tail in (head :: f, c) )}
| ty = atomic_type _2 = SEMI tail = meth_list
    {let head =
  let _endpos = _endpos_ty_ in
  let _symbolstartpos = _startpos_ty_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( Of.inherit_ ~loc:(make_loc _sloc) ty )
in
      ( let (f, c) = tail in (head :: f, c) )}
| _1 = LIDENT _2 = COLON _1_inlined1 = possibly_poly_core_type_no_attr_ _1_inlined2 = list_attribute_ _5 = SEMI _1_inlined3 = list_attribute_
    {let head =
  let _6 =
    let _1 = _1_inlined3 in
        ( _1 )
  in
  let _endpos__6_ = _endpos__1_inlined3_ in
  let _4 =
    let _1 = _1_inlined2 in
        ( _1 )
  in
  let _endpos__4_ = _endpos__1_inlined2_ in
  let _3 =
    let _1 = _1_inlined1 in
        ( _1 )
  in
  let _1 =
    let _1 =                                                 ( _1 ) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let _endpos = _endpos__6_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( let info =
        match rhs_info _endpos__4_ with
        | Some _ as info_before_semi -> info_before_semi
        | None -> symbol_info _endpos
      in
      let attrs = add_info_attrs info (_4 @ _6) in
      Of.tag ~loc:(make_loc _sloc) ~attrs _1 _3 )
in
      ( [head], Closed )}
| ty = atomic_type _2 = SEMI
    {let head =
  let _endpos = _endpos_ty_ in
  let _symbolstartpos = _startpos_ty_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( Of.inherit_ ~loc:(make_loc _sloc) ty )
in
      ( [head], Closed )}
| _1 = LIDENT _2 = COLON _1_inlined1 = possibly_poly_core_type_no_attr_ _1_inlined2 = list_attribute_
    {let head =
  let _4 =
    let _1 = _1_inlined2 in
        ( _1 )
  in
  let _endpos__4_ = _endpos__1_inlined2_ in
  let _3 =
    let _1 = _1_inlined1 in
        ( _1 )
  in
  let _1 =
    let _1 =                                                 ( _1 ) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mkrhs _1 _sloc )
  in
  let _endpos = _endpos__4_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( let info = symbol_info _endpos in
      let attrs = add_info_attrs info _4 in
      Of.tag ~loc:(make_loc _sloc) ~attrs _1 _3 )
in
      ( [head], Closed )}
| ty = atomic_type
    {let head =
  let _endpos = _endpos_ty_ in
  let _symbolstartpos = _startpos_ty_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( Of.inherit_ ~loc:(make_loc _sloc) ty )
in
      ( [head], Closed )}
| _1 = DOTDOT
    {      ( [], Open )}

constant:
  _1 = INT
    {                 ( let (n, m) = _1 in Pconst_integer (n, m) )}
| _1 = CHAR
    {                 ( Pconst_char (_1) )}
| _1 = STRING
    {                 ( let (s, strloc, d) = _1 in Pconst_string (s, strloc, d) )}
| _1 = FLOAT
    {                 ( let (f, m) = _1 in Pconst_float (f, m) )}

signed_constant:
  _1 = constant
    {                 ( _1 )}
| _1 = MINUS _2 = INT
    {                 ( let (n, m) = _2 in Pconst_integer(("-" ^ n), m) )}
| _1 = MINUS _2 = FLOAT
    {                 ( let (f, m) = _2 in Pconst_float(("-" ^ f), m) )}
| _1 = PLUS _2 = INT
    {                 ( let (n, m) = _2 in Pconst_integer (n, m) )}
| _1 = PLUS _2 = FLOAT
    {                 ( let (f, m) = _2 in Pconst_float(f, m) )}

ident:
  _1 = UIDENT
    {                              ( _1 )}
| _1 = LIDENT
    {                              ( _1 )}

val_extra_ident:
  _1 = LPAREN _2 = operator _3 = RPAREN
    {                              ( _2 )}
| _1 = LPAREN _2 = operator _3 = error
    {let _loc__3_ = (_startpos__3_, _endpos__3_) in
let _loc__1_ = (_startpos__1_, _endpos__1_) in
                              ( unclosed "(" _loc__1_ ")" _loc__3_ )}
| _1 = LPAREN _2 = error
    {let _loc__2_ = (_startpos__2_, _endpos__2_) in
                              ( expecting _loc__2_ "operator" )}
| _1 = LPAREN _2 = MODULE _3 = error
    {let _loc__3_ = (_startpos__3_, _endpos__3_) in
                              ( expecting _loc__3_ "module-expr" )}

val_ident:
  _1 = LIDENT
    {                              ( _1 )}
| _1 = val_extra_ident
    {                              ( _1 )}

operator:
  _1 = PREFIXOP
    {                                                ( _1 )}
| _1 = LETOP
    {                                                ( _1 )}
| _1 = ANDOP
    {                                                ( _1 )}
| _1 = DOTOP _2 = LPAREN _3 = index_mod _4 = RPAREN
    {                                                ( "."^ _1 ^"(" ^ _3 ^ ")" )}
| _1 = DOTOP _2 = LPAREN _3 = index_mod _4 = RPAREN _5 = LESSMINUS
    {                                                ( "."^ _1 ^ "(" ^ _3 ^ ")<-" )}
| _1 = DOTOP _2 = LBRACKET _3 = index_mod _4 = RBRACKET
    {                                                ( "."^ _1 ^"[" ^ _3 ^ "]" )}
| _1 = DOTOP _2 = LBRACKET _3 = index_mod _4 = RBRACKET _5 = LESSMINUS
    {                                                ( "."^ _1 ^ "[" ^ _3 ^ "]<-" )}
| _1 = DOTOP _2 = LBRACE _3 = index_mod _4 = RBRACE
    {                                                ( "."^ _1 ^"{" ^ _3 ^ "}" )}
| _1 = DOTOP _2 = LBRACE _3 = index_mod _4 = RBRACE _5 = LESSMINUS
    {                                                ( "."^ _1 ^ "{" ^ _3 ^ "}<-" )}
| _1 = HASHOP
    {                                                ( _1 )}
| _1 = BANG
    {                                                ( "!" )}
| op = INFIXOP0
    {let _1 =                   ( op ) in
                                                ( _1 )}
| op = INFIXOP1
    {let _1 =                   ( op ) in
                                                ( _1 )}
| op = INFIXOP2
    {let _1 =                   ( op ) in
                                                ( _1 )}
| op = INFIXOP3
    {let _1 =                   ( op ) in
                                                ( _1 )}
| op = INFIXOP4
    {let _1 =                   ( op ) in
                                                ( _1 )}
| _1 = PLUS
    {let _1 =                    ("+") in
                                                ( _1 )}
| _1 = PLUSDOT
    {let _1 =                   ("+.") in
                                                ( _1 )}
| _1 = PLUSEQ
    {let _1 =                   ("+=") in
                                                ( _1 )}
| _1 = MINUS
    {let _1 =                    ("-") in
                                                ( _1 )}
| _1 = MINUSDOT
    {let _1 =                   ("-.") in
                                                ( _1 )}
| _1 = STAR
    {let _1 =                    ("*") in
                                                ( _1 )}
| _1 = PERCENT
    {let _1 =                    ("%") in
                                                ( _1 )}
| _1 = EQUAL
    {let _1 =                    ("=") in
                                                ( _1 )}
| _1 = LESS
    {let _1 =                    ("<") in
                                                ( _1 )}
| _1 = GREATER
    {let _1 =                    (">") in
                                                ( _1 )}
| _1 = OR
    {let _1 =                   ("or") in
                                                ( _1 )}
| _1 = BARBAR
    {let _1 =                   ("||") in
                                                ( _1 )}
| _1 = AMPERSAND
    {let _1 =                    ("&") in
                                                ( _1 )}
| _1 = AMPERAMPER
    {let _1 =                   ("&&") in
                                                ( _1 )}
| _1 = COLONEQUAL
    {let _1 =                   (":=") in
                                                ( _1 )}

index_mod:
  
    {  ( "" )}
| _1 = SEMI _2 = DOTDOT
    {              ( ";.." )}

constr_extra_nonprefix_ident:
  _1 = LBRACKET _2 = RBRACKET
    {                                                ( "[]" )}
| _1 = LPAREN _2 = RPAREN
    {                                                ( "()" )}
| _1 = FALSE
    {                                                ( "false" )}
| _1 = TRUE
    {                                                ( "true" )}

constr_ident:
  _1 = UIDENT
    {                                                ( _1 )}
| _1 = LPAREN _2 = COLONCOLON _3 = RPAREN
    {let _1 =                                                 ( "::" ) in
                                                ( _1 )}
| _1 = constr_extra_nonprefix_ident
    {                                                ( _1 )}

constr_longident:
  _1 = mod_longident %prec below_DOT
    {                                         ( _1 )}
| _1 = mod_longident _2 = DOT _1_inlined1 = LPAREN _2_inlined1 = COLONCOLON _3 = RPAREN
    {let _3 =                                                 ( "::" ) in
                                         ( Ldot(_1,_3) )}
| _1 = LPAREN _2 = COLONCOLON _3 = RPAREN
    {let _1 =                                                 ( "::" ) in
                                         ( Lident (_1) )}
| _1 = constr_extra_nonprefix_ident
    {                                         ( Lident (_1) )}

mk_longident_mod_ext_longident_LIDENT_:
  _1 = LIDENT
    {                      ( Lident (_1) )}
| _1 = mod_ext_longident _2 = DOT _3 = LIDENT
    {                      ( Ldot(_1,_3) )}

mk_longident_mod_ext_longident_UIDENT_:
  _1 = UIDENT
    {                      ( Lident (_1) )}
| _1 = mod_ext_longident _2 = DOT _3 = UIDENT
    {                      ( Ldot(_1,_3) )}

mk_longident_mod_ext_longident___anonymous_41_:
  _1 = ident
    {let _1 =                                                   ( _1 ) in
                      ( Lident (_1) )}
| _1 = LPAREN _2 = COLONCOLON _3 = RPAREN
    {let _1 =
  let _1 =                                                 ( "::" ) in
                                                    ( _1 )
in
                      ( Lident (_1) )}
| _1 = val_extra_ident
    {let _1 =                                                   ( _1 ) in
                      ( Lident (_1) )}
| _1 = mod_ext_longident _2 = DOT _1_inlined1 = ident
    {let _3 =
  let _1 = _1_inlined1 in
                                                    ( _1 )
in
                      ( Ldot(_1,_3) )}
| _1 = mod_ext_longident _2 = DOT _1_inlined1 = LPAREN _2_inlined1 = COLONCOLON _3 = RPAREN
    {let _3 =
  let _1 =                                                 ( "::" ) in
                                                    ( _1 )
in
                      ( Ldot(_1,_3) )}
| _1 = mod_ext_longident _2 = DOT _1_inlined1 = val_extra_ident
    {let _3 =
  let _1 = _1_inlined1 in
                                                    ( _1 )
in
                      ( Ldot(_1,_3) )}

mk_longident_mod_ext_longident_ident_:
  _1 = ident
    {                      ( Lident (_1) )}
| _1 = mod_ext_longident _2 = DOT _3 = ident
    {                      ( Ldot(_1,_3) )}

mk_longident_mod_longident_LIDENT_:
  _1 = LIDENT
    {                      ( Lident (_1) )}
| _1 = mod_longident _2 = DOT _3 = LIDENT
    {                      ( Ldot(_1,_3) )}

mk_longident_mod_longident_UIDENT_:
  _1 = UIDENT
    {                      ( Lident (_1) )}
| _1 = mod_longident _2 = DOT _3 = UIDENT
    {                      ( Ldot(_1,_3) )}

mk_longident_mod_longident_val_ident_:
  _1 = val_ident
    {                      ( Lident (_1) )}
| _1 = mod_longident _2 = DOT _3 = val_ident
    {                      ( Ldot(_1,_3) )}

val_longident:
  _1 = mk_longident_mod_longident_val_ident_
    {                                           ( _1 )}

label_longident:
  _1 = mk_longident_mod_longident_LIDENT_
    {                                        ( _1 )}

type_longident:
  _1 = mk_longident_mod_ext_longident_LIDENT_
    {                                             ( _1 )}

mod_longident:
  _1 = mk_longident_mod_longident_UIDENT_
    {                                         ( _1 )}

mod_ext_longident:
  _1 = mk_longident_mod_ext_longident_UIDENT_
    {                                            ( _1 )}
| _1 = mod_ext_longident _2 = LPAREN _3 = mod_ext_longident _4 = RPAREN
    {let _endpos = _endpos__4_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
      ( lapply ~loc:_sloc _1 _3 )}
| _1 = mod_ext_longident _2 = LPAREN _3 = error
    {let _loc__3_ = (_startpos__3_, _endpos__3_) in
      ( expecting _loc__3_ "module path" )}

mty_longident:
  _1 = mk_longident_mod_ext_longident_ident_
    {                                          ( _1 )}

clty_longident:
  _1 = mk_longident_mod_ext_longident_LIDENT_
    {                                           ( _1 )}

class_longident:
  _1 = mk_longident_mod_longident_LIDENT_
    {                                      ( _1 )}

any_longident:
  _1 = mk_longident_mod_ext_longident___anonymous_41_
    {      ( _1 )}
| _1 = constr_extra_nonprefix_ident
    {                                 ( Lident (_1) )}

toplevel_directive:
  _1 = HASH _1_inlined1 = ident
    {let arg =     ( None ) in
let _endpos_arg_ = _endpos__1_inlined1_ in
let dir =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos = _endpos_arg_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mk_directive ~loc:_sloc dir arg )}
| _1 = HASH _1_inlined1 = ident _1_inlined2 = STRING
    {let arg =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let x =
    let _1 =                   ( let (s, _, _) = _1 in Pdir_string s ) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mk_directive_arg ~loc:_sloc _1 )
  in
      ( Some x )
in
let _endpos_arg_ = _endpos__1_inlined2_ in
let dir =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos = _endpos_arg_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mk_directive ~loc:_sloc dir arg )}
| _1 = HASH _1_inlined1 = ident _1_inlined2 = INT
    {let arg =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let x =
    let _1 =                   ( let (n, m) = _1 in Pdir_int (n ,m) ) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mk_directive_arg ~loc:_sloc _1 )
  in
      ( Some x )
in
let _endpos_arg_ = _endpos__1_inlined2_ in
let dir =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos = _endpos_arg_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mk_directive ~loc:_sloc dir arg )}
| _1 = HASH _1_inlined1 = ident _1_inlined2 = val_longident
    {let arg =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let x =
    let _1 =                   ( Pdir_ident _1 ) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mk_directive_arg ~loc:_sloc _1 )
  in
      ( Some x )
in
let _endpos_arg_ = _endpos__1_inlined2_ in
let dir =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos = _endpos_arg_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mk_directive ~loc:_sloc dir arg )}
| _1 = HASH _1_inlined1 = ident _1_inlined2 = mod_longident
    {let arg =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined2_, _startpos__1_inlined2_, _1_inlined2) in
  let x =
    let _1 =                   ( Pdir_ident _1 ) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mk_directive_arg ~loc:_sloc _1 )
  in
      ( Some x )
in
let _endpos_arg_ = _endpos__1_inlined2_ in
let dir =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos = _endpos_arg_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mk_directive ~loc:_sloc dir arg )}
| _1 = HASH _1_inlined1 = ident _1_inlined2 = FALSE
    {let arg =
  let (_endpos__1_, _startpos__1_) = (_endpos__1_inlined2_, _startpos__1_inlined2_) in
  let x =
    let _1 =                   ( Pdir_bool false ) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mk_directive_arg ~loc:_sloc _1 )
  in
      ( Some x )
in
let _endpos_arg_ = _endpos__1_inlined2_ in
let dir =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos = _endpos_arg_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mk_directive ~loc:_sloc dir arg )}
| _1 = HASH _1_inlined1 = ident _1_inlined2 = TRUE
    {let arg =
  let (_endpos__1_, _startpos__1_) = (_endpos__1_inlined2_, _startpos__1_inlined2_) in
  let x =
    let _1 =                   ( Pdir_bool true ) in
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
        ( mk_directive_arg ~loc:_sloc _1 )
  in
      ( Some x )
in
let _endpos_arg_ = _endpos__1_inlined2_ in
let dir =
  let (_endpos__1_, _startpos__1_, _1) = (_endpos__1_inlined1_, _startpos__1_inlined1_, _1_inlined1) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkrhs _1 _sloc )
in
let _endpos = _endpos_arg_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mk_directive ~loc:_sloc dir arg )}

name_tag:
  _1 = BACKQUOTE _2 = ident
    {                                                ( _2 )}

rec_flag:
  
    {                                                ( Nonrecursive )}
| _1 = REC
    {                                                ( Recursive )}

direction_flag:
  _1 = TO
    {                                                ( Upto )}
| _1 = DOWNTO
    {                                                ( Downto )}

private_flag:
  
    {let _1 =                                                 ( Public ) in
    ( _1 )}
| _1 = PRIVATE
    {let _1 =                                                 ( Private ) in
    ( _1 )}

mutable_flag:
  
    {                                                ( Immutable )}
| _1 = MUTABLE
    {                                                ( Mutable )}

virtual_flag:
  
    {                                                ( Concrete )}
| _1 = VIRTUAL
    {                                                ( Virtual )}

mutable_virtual_flags:
  
    {      ( Immutable, Concrete )}
| _1 = MUTABLE
    {      ( Mutable, Concrete )}
| _1 = VIRTUAL
    {      ( Immutable, Virtual )}
| _1 = MUTABLE _2 = VIRTUAL
    {      ( Mutable, Virtual )}
| _1 = VIRTUAL _2 = MUTABLE
    {      ( Mutable, Virtual )}

private_virtual_flags:
  
    {                 ( Public, Concrete )}
| _1 = PRIVATE
    {            ( Private, Concrete )}
| _1 = VIRTUAL
    {            ( Public, Virtual )}
| _1 = PRIVATE _2 = VIRTUAL
    {                    ( Private, Virtual )}
| _1 = VIRTUAL _2 = PRIVATE
    {                    ( Private, Virtual )}

virtual_with_mutable_flag:
  _1 = VIRTUAL
    {            ( Immutable )}
| _1 = MUTABLE _2 = VIRTUAL
    {                    ( Mutable )}
| _1 = VIRTUAL _2 = MUTABLE
    {                    ( Mutable )}

virtual_with_private_flag:
  _1 = VIRTUAL
    {            ( Public )}
| _1 = PRIVATE _2 = VIRTUAL
    {                    ( Private )}
| _1 = VIRTUAL _2 = PRIVATE
    {                    ( Private )}

subtractive:
  _1 = MINUS
    {                                                ( "-" )}
| _1 = MINUSDOT
    {                                                ( "-." )}

additive:
  _1 = PLUS
    {                                                ( "+" )}
| _1 = PLUSDOT
    {                                                ( "+." )}

optlabel:
  _1 = OPTLABEL
    {                                                ( (_1) )}
| _1 = QUESTION _2 = LIDENT _3 = COLON
    {                                                ( _2 )}

single_attr_id:
  _1 = LIDENT
    {           ( _1 )}
| _1 = UIDENT
    {           ( _1 )}
| _1 = AND
    {        ( "and" )}
| _1 = AS
    {       ( "as" )}
| _1 = ASSERT
    {           ( "assert" )}
| _1 = BEGIN
    {          ( "begin" )}
| _1 = CLASS
    {          ( "class" )}
| _1 = CONSTRAINT
    {               ( "constraint" )}
| _1 = DO
    {       ( "do" )}
| _1 = DONE
    {         ( "done" )}
| _1 = DOWNTO
    {           ( "downto" )}
| _1 = ELSE
    {         ( "else" )}
| _1 = END
    {        ( "end" )}
| _1 = EXCEPTION
    {              ( "exception" )}
| _1 = EXTERNAL
    {             ( "external" )}
| _1 = FALSE
    {          ( "false" )}
| _1 = FOR
    {        ( "for" )}
| _1 = FUN
    {        ( "fun" )}
| _1 = FUNCTION
    {             ( "function" )}
| _1 = FUNCTOR
    {            ( "functor" )}
| _1 = IF
    {       ( "if" )}
| _1 = IN
    {       ( "in" )}
| _1 = INCLUDE
    {            ( "include" )}
| _1 = INHERIT
    {            ( "inherit" )}
| _1 = INITIALIZER
    {                ( "initializer" )}
| _1 = LAZY
    {         ( "lazy" )}
| _1 = LET
    {        ( "let" )}
| _1 = MATCH
    {          ( "match" )}
| _1 = METHOD
    {           ( "method" )}
| _1 = MODULE
    {           ( "module" )}
| _1 = MUTABLE
    {            ( "mutable" )}
| _1 = NEW
    {        ( "new" )}
| _1 = NONREC
    {           ( "nonrec" )}
| _1 = OBJECT
    {           ( "object" )}
| _1 = OF
    {       ( "of" )}
| _1 = OPEN
    {         ( "open" )}
| _1 = OR
    {       ( "or" )}
| _1 = PRIVATE
    {            ( "private" )}
| _1 = REC
    {        ( "rec" )}
| _1 = SIG
    {        ( "sig" )}
| _1 = STRUCT
    {           ( "struct" )}
| _1 = THEN
    {         ( "then" )}
| _1 = TO
    {       ( "to" )}
| _1 = TRUE
    {         ( "true" )}
| _1 = TRY
    {        ( "try" )}
| _1 = TYPE
    {         ( "type" )}
| _1 = VAL
    {        ( "val" )}
| _1 = VIRTUAL
    {            ( "virtual" )}
| _1 = WHEN
    {         ( "when" )}
| _1 = WHILE
    {          ( "while" )}
| _1 = WITH
    {         ( "with" )}

attr_id:
  _1 = single_attr_id
    {let _1 =
  let _1 =                      ( _1 ) in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkloc _1 (make_loc _sloc) )
in
    ( _1 )}
| _1 = single_attr_id _2 = DOT _3 = attr_id
    {let _1 =
  let _1 =                                  ( (_1 ^ "." ^ (_3.txt)) ) in
  let _endpos__1_ = _endpos__3_ in
  let _endpos = _endpos__1_ in
  let _symbolstartpos = _startpos__1_ in
  let _sloc = (_symbolstartpos, _endpos) in
      ( mkloc _1 (make_loc _sloc) )
in
    ( _1 )}

attribute:
  _1 = LBRACKETAT _2 = attr_id _3 = payload _4 = RBRACKET
    {let _endpos = _endpos__4_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( Attr.mk ~loc:(make_loc _sloc) _2 _3 )}

post_item_attribute:
  _1 = LBRACKETATAT _2 = attr_id _3 = payload _4 = RBRACKET
    {let _endpos = _endpos__4_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( Attr.mk ~loc:(make_loc _sloc) _2 _3 )}

floating_attribute:
  _1 = LBRACKETATATAT _2 = attr_id _3 = payload _4 = RBRACKET
    {let _endpos = _endpos__4_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mark_symbol_docs _sloc;
      Attr.mk ~loc:(make_loc _sloc) _2 _3 )}

ext:
  
    {                    ( None )}
| _1 = PERCENT _2 = attr_id
    {                    ( Some _2 )}

extension:
  _1 = LBRACKETPERCENT _2 = attr_id _3 = payload _4 = RBRACKET
    {                                             ( (_2, _3) )}
| _1 = QUOTED_STRING_EXPR
    {let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mk_quotedext ~loc:_sloc _1 )}

item_extension:
  _1 = LBRACKETPERCENTPERCENT _2 = attr_id _3 = payload _4 = RBRACKET
    {                                                    ( (_2, _3) )}
| _1 = QUOTED_STRING_ITEM
    {let _endpos = _endpos__1_ in
let _symbolstartpos = _startpos__1_ in
let _sloc = (_symbolstartpos, _endpos) in
    ( mk_quotedext ~loc:_sloc _1 )}

payload:
  _1 = structure
    {              ( PStr _1 )}
| _1 = COLON _2 = signature
    {                    ( PSig _2 )}
| _1 = COLON _2 = core_type
    {                    ( PTyp _2 )}
| _1 = QUESTION _2 = pattern
    {                     ( PPat (_2, None) )}
| _1 = QUESTION _2 = pattern _3 = WHEN _4 = seq_expr
    {                                   ( PPat (_2, (Some _4)) )}

%%


