/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* The parser definition */

/* The commands [make list-parse-errors] and [make generate-parse-errors]
   run Menhir on a modified copy of the parser where every block of
   text comprised between the markers [BEGIN AVOID] and -----------
   [END AVOID] has been removed. This file should be formatted in
   such a way that this results in a clean removal of certain
   symbols, productions, or declarations. */

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
           ppat_loc_stack = push_loc x.ppat_loc x.ppat_loc_stack };;
let reloc_exp ~loc x =
  { x with pexp_loc = make_loc loc;
           pexp_loc_stack = push_loc x.pexp_loc x.pexp_loc_stack };;
let reloc_typ ~loc x =
  { x with ptyp_loc = make_loc loc;
           ptyp_loc_stack = push_loc x.ptyp_loc x.ptyp_loc_stack };;

let mkexpvar ~loc (name : string Ploc.vala) =
  mkexp ~loc (Pexp_ident(mkrhs (vaval  (Lident name)) loc))

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
  Pexp_apply(op, (vaval [Nolabel, arg1; Nolabel, arg2]))

let neg_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus ~oploc name arg =
  match name, arg.pexp_desc with
  | "-", Pexp_constant(Ploc.VaVal (Pconst_integer (Ploc.VaVal n,m))) ->
      Pexp_constant(vaval (Pconst_integer(vaval (neg_string n),m)))
  | ("-" | "-."), Pexp_constant(Ploc.VaVal (Pconst_float (Ploc.VaVal f, m))) ->
      Pexp_constant(vaval (Pconst_float(vaval (neg_string f), m)))
  | _ ->
      Pexp_apply(mkoperator ~loc:oploc (vaval ("~" ^ name)), vaval [Nolabel, arg])

let mkuplus ~oploc name arg =
  let desc = arg.pexp_desc in
  match name, desc with
  | "+", Pexp_constant(Ploc.VaVal (Pconst_integer _))
  | ("+" | "+."), Pexp_constant(Ploc.VaVal (Pconst_float _)) -> desc
  | _ ->
      Pexp_apply(mkoperator ~loc:oploc (vaval ("~" ^ name)), vaval [Nolabel, arg])

(* TODO define an abstraction boundary between locations-as-pairs
   and locations-as-Location.t; it should be clear when we move from
   one world to the other *)

let mkexp_cons_desc consloc args =
  Pexp_construct(mkrhs (vaval (Lident (vaval "::"))) consloc, vaval (Some args))
let mkexp_cons ~loc consloc args =
  mkexp ~loc (mkexp_cons_desc consloc args)

let mkpat_cons_desc consloc args =
  Ppat_construct(mkrhs (vaval (Lident (vaval "::"))) consloc, vaval (Some (vaval [], args)))
let mkpat_cons ~loc consloc args =
  mkpat ~loc (mkpat_cons_desc consloc args)

let ghexp_cons_desc consloc args =
  Pexp_construct(ghrhs (vaval (Lident (vaval "::"))) consloc, vaval (Some args))
let ghpat_cons_desc consloc args =
  Ppat_construct(ghrhs (vaval (Lident (vaval "::"))) consloc, vaval (Some (vaval [], args)))

let rec mktailexp nilloc = let open Location in function
    [] ->
      let nil = ghloc ~loc:nilloc (vaval (Lident (vaval "[]"))) in
      Pexp_construct (nil, vaval None), nilloc
  | e1 :: el ->
      let exp_el, el_loc = mktailexp nilloc el in
      let loc = (e1.pexp_loc.loc_start, snd el_loc) in
      let arg = ghexp ~loc (Pexp_tuple (vaval [e1; ghexp ~loc:el_loc exp_el])) in
      ghexp_cons_desc loc arg, loc

let rec mktailpat nilloc = let open Location in function
    [] ->
      let nil = ghloc ~loc:nilloc (vaval (Lident (vaval "[]"))) in
      Ppat_construct (nil, vaval None), nilloc
  | p1 :: pl ->
      let pat_pl, el_loc = mktailpat nilloc pl in
      let loc = (p1.ppat_loc.loc_start, snd el_loc) in
      let arg = ghpat ~loc (Ppat_tuple (vaval [p1; ghpat ~loc:el_loc pat_pl])) in
      ghpat_cons_desc loc arg, loc

let mkstrexp ~loc e attrs =
  { pstr_desc = Pstr_eval (e, attrs); pstr_loc = make_loc loc }

let mkexp_constraint ~loc e (t1, t2) =
  match t1, t2 with
  | Some t, None -> ghexp ~loc (Pexp_constraint(e, t))
  | _, Some t -> ghexp ~loc (Pexp_coerce(e, vaval t1, t))
  | None, None -> assert false

let mkexp_opt_constraint ~loc e = function
  | None -> e
  | Some constraint_ -> mkexp_constraint ~loc e constraint_

let mkpat_opt_constraint ~loc p = function
  | None -> p
  | Some typ -> ghpat ~loc (Ppat_constraint(p, typ))

let syntax_error () =
  raise Syntaxerr.Escape_error

let unclosed opening_name opening_loc closing_name closing_loc =
  raise(Syntaxerr.Error(Syntaxerr.Unclosed(make_loc opening_loc, opening_name,
                                           make_loc closing_loc, closing_name)))

let expecting loc nonterm =
    raise Syntaxerr.(Error(Expecting(make_loc loc, nonterm)))

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
    { pexp_desc = Pexp_tuple (Ploc.VaVal explist); pexp_loc = _ } -> explist
  | exp -> [exp]

let builtin_arraylike_name loc _ ~assign paren_kind n =
  let opname = if assign then "set" else "get" in
  let opname = if !Clflags.unsafe then "unsafe_" ^ opname else opname in
  let prefix = match paren_kind with
    | Paren -> Lident (vaval "Array")
    | Bracket -> Lident (vaval "String")
    | Brace ->
       let submodule_name = match n with
         | One -> "Array1"
         | Two -> "Array2"
         | Three -> "Array3"
         | Many -> "Genarray" in
       Ldot(vaval (Lident (vaval "Bigarray")), vaval submodule_name) in
   ghloc ~loc (Ldot(vaval prefix,vaval opname))

let builtin_arraylike_index loc paren_kind index = match paren_kind with
    | Paren | Bracket -> One, [Nolabel, index]
    | Brace ->
       (* Multi-indices for bigarray are comma-separated ([a.{1,2,3,4}]) *)
       match bigarray_untuplify index with
     | [x] -> One, [Nolabel, x]
     | [x; y] -> Two, [Nolabel, x; Nolabel, y]
     | [x; y; z] -> Three, [Nolabel, x; Nolabel, y; Nolabel, z]
     | coords -> Many, [Nolabel, ghexp ~loc (Pexp_array (vaval coords))]

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
  let lid = match Option.map unvala prefix with
    | None -> Lident (vaval name)
    | Some p -> Ldot(vaval p,vaval  name) in
  ghloc ~loc lid

let user_index loc _ index =
  (* Multi-indices for user-defined operators are semicolon-separated
     ([a.%[1;2;3;4]]) *)
  match index with
    | [a] -> One, [Nolabel, a]
    | l -> Many, [Nolabel, mkexp ~loc (Pexp_array (vaval l))]

let user_indexing_operators:
      (Longident.t Ploc.vala option * string, expression list) array_family
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
  mkexp ~loc (Pexp_apply(ghexp ~loc (Pexp_ident (loc_map vaval fn)), vaval args))

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

(*-*)let loc_last_vala (id : Longident.t Location.loc) : string Ploc.vala Location.loc =
(*-*)  loc_map Longident.last_vala id
(*-*)
(*-*)let loc_vala_last (id : Longident.t Ploc.vala Location.loc) : string Ploc.vala Location.loc =
(*-*)  loc_map (Pcaml.vala_map Longident.last) id
(*-*)
let loc_lident (id : string Location.loc) : Longident.t Location.loc =
  loc_map (fun x -> Lident (vaval x)) id

let exp_of_longident ~loc lid =
  let lid = make_ghost (loc_map (fun id -> vaval (Lident (vaval (Longident.last id)))) lid) in
  ghexp ~loc (Pexp_ident lid)

let exp_of_label ~loc lbl =
  mkexp ~loc (Pexp_ident (loc_map vaval (loc_lident lbl)))

let pat_of_label lbl =
  Pat.mk ~loc:lbl.loc  (Ppat_var (loc_last_vala lbl))

let mk_newtypes ~loc newtypes exp =
  let mkexp = mkexp ~loc in
  List.fold_right (fun newtype exp -> mkexp (Pexp_newtype (newtype, exp)))
    newtypes exp

let wrap_type_annotation ~loc newtypes core_type body =
  let mkexp, ghtyp = mkexp ~loc, ghtyp ~loc in
  let mk_newtypes = mk_newtypes ~loc in
  let exp = mkexp(Pexp_constraint(body,core_type)) in
  let exp = mk_newtypes newtypes exp in
  (exp, ghtyp(Ptyp_poly(vaval newtypes, Typ.varify_constructors newtypes core_type)))

let wrap_exp_attrs ~loc body (ext, attrs) =
  let ghexp = ghexp ~loc in
  (* todo: keep exact location for the entire attribute *)
  let body = {body with pexp_attributes = append_list_vala attrs body.pexp_attributes} in
  match ext with
  | None -> body
  | Some id -> ghexp(Pexp_extension (id, PStr (vaval [mkstrexp ~loc (vaval body) (vaval [])])))

let mkexp_attrs ~loc d attrs =
  wrap_exp_attrs ~loc (mkexp ~loc d) attrs

let wrap_typ_attrs ~loc typ (ext, attrs) =
  (* todo: keep exact location for the entire attribute *)
  let typ = {typ with ptyp_attributes = append_list_vala attrs typ.ptyp_attributes} in
  match ext with
  | None -> typ
  | Some id -> ghtyp ~loc (Ptyp_extension (id, PTyp typ))

let wrap_pat_attrs ~loc pat (ext, attrs) =
  (* todo: keep exact location for the entire attribute *)
  let pat = {pat with ppat_attributes = append_list_vala attrs pat.ppat_attributes} in
  match ext with
  | None -> pat
  | Some id -> ghpat ~loc (Ppat_extension (id, PPat (pat, vaval None)))

let mkpat_attrs ~loc d attrs =
  wrap_pat_attrs ~loc (mkpat ~loc d) attrs

let wrap_class_attrs ~loc:_ body attrs =
  {body with pcl_attributes = append_list_vala attrs body.pcl_attributes}
let wrap_mod_attrs ~loc:_ attrs body =
  {body with pmod_attributes = append_list_vala attrs body.pmod_attributes}
let wrap_mty_attrs ~loc:_ attrs body =
  {body with pmty_attributes = append_list_vala attrs body.pmty_attributes}

let wrap_str_ext ~loc body ext =
  match ext with
  | None -> body
  | Some id -> ghstr ~loc (Pstr_extension ((id, PStr (vaval [body])), vaval []))

let wrap_mkstr_ext ~loc (item, ext) =
  wrap_str_ext ~loc (mkstr ~loc item) ext

let wrap_sig_ext ~loc body ext =
  match ext with
  | None -> body
  | Some id -> ghsig ~loc (Psig_extension ((id, PSig (vaval [body])), vaval []))

let wrap_mksig_ext ~loc (item, ext) =
  wrap_sig_ext ~loc (mksig ~loc item) ext

let mk_quotedext ~loc (id, idloc, str, strloc, delim) =
  let exp_id = mkloc (vaval id) idloc in
  let e = ghexp ~loc (Pexp_constant (vaval (Pconst_string (vaval str, strloc, Option.map vaval delim)))) in
  (exp_id, PStr (vaval [mkstrexp ~loc (vaval e) (vaval [])]))

let text_str pos = Str.text (rhs_text pos)
let text_sig pos = Sig.text (rhs_text pos)
let text_cstr pos = Cf.text (rhs_text pos)
let text_csig pos = Ctf.text (rhs_text pos)
let text_def pos =
  List.map (fun def -> Ptop_def (vaval [def])) (Str.text (rhs_text pos))

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
    (fun txt -> List.map (fun def -> Ptop_def (vaval [def])) (Str.text txt))
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
  { lbs_bindings: let_binding list Ploc.vala;
    lbs_rec: rec_flag Ploc.vala;
    lbs_extension: string Ploc.vala Asttypes.loc option }

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
  { lbs with lbs_bindings = append_list_vala (vaval [lb]) lbs.lbs_bindings }

let mklbs ext rf lb =
  let lbs = {
    lbs_bindings = vaval [];
    lbs_rec = rf;
    lbs_extension = ext;
  } in
  addlb lbs lb

let vb_of_lb lb =
  Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
    lb.lb_pattern lb.lb_expression

let val_of_let_bindings ~loc lbs =
  let bindings =
    Pcaml.vala_map (List.map
                      (fun lb ->
                        Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
                          ~docs:(Lazy.force lb.lb_docs)
                          ~text:(Lazy.force lb.lb_text)
                          lb.lb_pattern lb.lb_expression))
      lbs.lbs_bindings
  in
  let str = mkstr ~loc (Pstr_value(lbs.lbs_rec, Pcaml.vala_map List.rev bindings)) in
  match lbs.lbs_extension with
  | None -> str
  | Some id -> ghstr ~loc (Pstr_extension((id, PStr (vaval [str])), vaval []))

let expr_of_let_bindings ~loc lbs body =
  let bindings =
    Pcaml.vala_map (List.map vb_of_lb)
      lbs.lbs_bindings
  in
    mkexp_attrs ~loc (Pexp_let(lbs.lbs_rec, Pcaml.vala_map List.rev bindings, body))
      (lbs.lbs_extension, vaval [])

let class_of_let_bindings ~loc lbs body =
  let bindings =
    Pcaml.vala_map (List.map vb_of_lb)
      lbs.lbs_bindings
  in
    (* Our use of let_bindings(no_ext) guarantees the following: *)
    assert (lbs.lbs_extension = None);
    mkclass ~loc (Pcl_let (lbs.lbs_rec, Pcaml.vala_map List.rev bindings, body))

(* Alternatively, we could keep the generic module type in the Parsetree
   and extract the package type during type-checking. In that case,
   the assertions below should be turned into explicit checks. *)
let package_type_of_module_type pmty =
  let err loc s =
    raise (Syntaxerr.Error (Syntaxerr.Invalid_package_type (loc, s)))
  in
  let map_cstr = function
    | Pwith_type (lid, Ploc.VaVal ptyp) ->
        let loc = ptyp.ptype_loc in
        if ptyp.ptype_params <> vaval [] then
          err loc "parametrized types are not supported";
        if ptyp.ptype_cstrs <> vaval [] then
          err loc "constrained types are not supported";
        if unvala ptyp.ptype_private <> Public then
          err loc "private types are not supported";

        (* restrictions below are checked by the 'with_constraint' rule *)
        assert (ptyp.ptype_kind = Ptype_abstract);
        assert (ptyp.ptype_attributes = vaval []);
        let ty =
          match unvala ptyp.ptype_manifest with
          | Some (Ploc.VaVal ty) -> ty
          | None -> assert false
        in
        (lid, ty)
    | _ ->
        err pmty.pmty_loc "only 'with type t =' constraints are supported"
  in
  match pmty with
  | {pmty_desc = Pmty_ident lid} -> ((loc_map vaval lid, vaval []), pmty.pmty_attributes)
  | {pmty_desc = Pmty_with({pmty_desc = Pmty_ident lid}, cstrs)} ->
      ((loc_map vaval lid, Pcaml.vala_map (List.map map_cstr) cstrs), pmty.pmty_attributes)
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

/* Tokens */

/* The alias that follows each token is used by Menhir when it needs to
   produce a sentence (that is, a sequence of tokens) in concrete syntax. */

/* Some tokens represent multiple concrete strings. In most cases, an
   arbitrary concrete string can be chosen. In a few cases, one must
   be careful: e.g., in PREFIXOP and INFIXOP2, one must choose a concrete
   string that will not trigger a syntax error; see how [not_expecting]
   is used in the definition of [type_variance]. */

%token AMPERAMPER             "&&"
%token AMPERSAND              "&"
%token AND                    "and"
%token AS                     "as"
%token ASSERT                 "assert"
%token BACKQUOTE              "`"
%token BANG                   "!"
%token BAR                    "|"
%token BARBAR                 "||"
%token BARRBRACKET            "|]"
%token BEGIN                  "begin"
%token <char> CHAR            "'a'" (* just an example *)
%token CLASS                  "class"
%token COLON                  ":"
%token COLONCOLON             "::"
%token COLONEQUAL             ":="
%token COLONGREATER           ":>"
%token COMMA                  ","
%token CONSTRAINT             "constraint"
%token DO                     "do"
%token DONE                   "done"
%token DOT                    "."
%token DOTDOT                 ".."
%token DOWNTO                 "downto"
%token ELSE                   "else"
%token END                    "end"
%token EOF                    ""
%token EQUAL                  "="
%token EXCEPTION              "exception"
%token EXTERNAL               "external"
%token FALSE                  "false"
%token <string * char option> FLOAT "42.0" (* just an example *)
%token FOR                    "for"
%token FUN                    "fun"
%token FUNCTION               "function"
%token FUNCTOR                "functor"
%token GREATER                ">"
%token GREATERRBRACE          ">}"
%token GREATERRBRACKET        ">]"
%token IF                     "if"
%token IN                     "in"
%token INCLUDE                "include"
%token <string> INFIXOP0      "!="   (* just an example *)
%token <string> INFIXOP1      "@"    (* just an example *)
%token <string> INFIXOP2      "+!"   (* chosen with care; see above *)
%token <string> INFIXOP3      "land" (* just an example *)
%token <string> INFIXOP4      "**"   (* just an example *)
%token <string> DOTOP         ".+"
%token <string> LETOP         "let*" (* just an example *)
%token <string> ANDOP         "and*" (* just an example *)
%token INHERIT                "inherit"
%token INITIALIZER            "initializer"
%token <string * char option> INT "42"  (* just an example *)
%token <string> LABEL         "~label:" (* just an example *)
%token LAZY                   "lazy"
%token LBRACE                 "{"
%token LBRACELESS             "{<"
%token LBRACKET               "["
%token LBRACKETBAR            "[|"
%token LBRACKETLESS           "[<"
%token LBRACKETGREATER        "[>"
%token LBRACKETPERCENT        "[%"
%token LBRACKETPERCENTPERCENT "[%%"
%token LESS                   "<"
%token LESSMINUS              "<-"
%token LET                    "let"
%token <string> LIDENT        "lident" (* just an example *)
%token LPAREN                 "("
%token LBRACKETAT             "[@"
%token LBRACKETATAT           "[@@"
%token LBRACKETATATAT         "[@@@"
%token MATCH                  "match"
%token METHOD                 "method"
%token MINUS                  "-"
%token MINUSDOT               "-."
%token MINUSGREATER           "->"
%token MODULE                 "module"
%token MUTABLE                "mutable"
%token NEW                    "new"
%token NONREC                 "nonrec"
%token OBJECT                 "object"
%token OF                     "of"
%token OPEN                   "open"
%token <string> OPTLABEL      "?label:" (* just an example *)
%token OR                     "or"
/* %token PARSER              "parser" */
%token PERCENT                "%"
%token PLUS                   "+"
%token PLUSDOT                "+."
%token PLUSEQ                 "+="
%token <string> PREFIXOP      "!+" (* chosen with care; see above *)
%token PRIVATE                "private"
%token QUESTION               "?"
%token QUOTE                  "'"
%token RBRACE                 "}"
%token RBRACKET               "]"
%token REC                    "rec"
%token RPAREN                 ")"
%token SEMI                   ";"
%token SEMISEMI               ";;"
%token HASH                   "#"
%token <string> HASHOP        "##" (* just an example *)
%token SIG                    "sig"
%token STAR                   "*"
%token <string * Location.t * string option>
       STRING                 "\"hello\"" (* just an example *)
%token <string * Location.t * string * Location.t * string option>
       QUOTED_STRING_EXPR     "{%hello|world|}"  (* just an example *)
%token <string * Location.t * string * Location.t * string option>
       QUOTED_STRING_ITEM     "{%%hello|world|}" (* just an example *)
%token STRUCT                 "struct"
%token THEN                   "then"
%token TILDE                  "~"
%token TO                     "to"
%token TRUE                   "true"
%token TRY                    "try"
%token TYPE                   "type"
/*-*/%token TYPESUBST                   "typesubst"
%token <string> UIDENT        "UIdent" (* just an example *)
%token UNDERSCORE             "_"
%token VAL                    "val"
%token VIRTUAL                "virtual"
%token WHEN                   "when"
%token WHILE                  "while"
%token WITH                   "with"
%token <string * Location.t> COMMENT    "(* comment *)"
%token <Docstrings.docstring> DOCSTRING "(** documentation *)"
/*-*/%token <string> ANTI
/*-*/%token <string> ANTI_NOATTRS
/*-*/%token <string> ANTI_OPT
/*-*/%token <string> ANTI_TUPLELIST
/*-*/%token <string> ANTI_LIST
/*-*/%token <string> ANTI_CLASSLIST
/*-*/%token <string> ANTI_CLASSDESCLIST
/*-*/%token <string> ANTI_CLASSTYPELIST
/*-*/%token <string> ANTI_CONSTRUCTORLIST
/*-*/%token <string> ANTI_ID
/*-*/%token <string> ANTI_LID
/*-*/%token <string> ANTI_UID
/*-*/%token <string> ANTI_LONGID
/*-*/%token <string> ANTI_LONGLID
/*-*/%token <string> ANTI_TYP
/*-*/%token <string> ANTI_PRIV
/*-*/%token <string> ANTI_ALGATTRS
/*-*/%token <string> ANTI_ITEMATTRS
/*-*/%token <string> ANTI_MUTABLE
/*-*/%token <string> ANTI_WHENO
/*-*/%token <string> ANTI_WITHE
/*-*/%token <string> ANTI_RECFLAG
/*-*/%token <string> ANTI_NONRECFLAG
/*-*/%token <string> ANTI_OVERRIDEFLAG
/*-*/%token <string> ANTI_CLOSEDFLAG
/*-*/%token <string> ANTI_EXPR
/*-*/%token <string> ANTI_EXPROPT
/*-*/%token <string> ANTI_PATTOPT
/*-*/%token <string> ANTI_CTYPOPT
/*-*/%token <string> ANTI_PATT
/*-*/%token <string> ANTI_INT
/*-*/%token <string> ANTI_INT32
/*-*/%token <string> ANTI_INT64
/*-*/%token <string> ANTI_NATIVEINT
/*-*/%token <string> ANTI_CHAR
/*-*/%token <string> ANTI_STRING
/*-*/%token <string> ANTI_DELIM
/*-*/%token <string> ANTI_FLOAT
/*-*/%token <string> ANTI_LABEL
/*-*/%token <string> ANTI_DIRFLAG
/*-*/%token <string> ANTI_EXCON
/*-*/%token <string> ANTI_LETOP
/*-*/%token <string> ANTI_ATTRID
/*-*/%token <string> ANTI_CONSTANT
/*-*/%token <string> ANTI_ISCONST
/*-*/%token <string> ANTI_VIRTUAL
/*-*/%token <string> ANTI_TYPEDECL
/*-*/%token <string> ANTI_CASES
%token EOL                    "\\n"      (* not great, but EOL is unused *)

/* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*/

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%right    MINUSGREATER                  /* function_type (t -> t -> t) */
%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%nonassoc below_LBRACKETAT
%nonassoc LBRACKETAT
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ /* expr (e OP e OP e) */
%left     PERCENT INFIXOP3 STAR                 /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */
%nonassoc prec_unary_minus prec_unary_plus /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_HASH
%nonassoc HASH                         /* simple_expr/toplevel_directive */
%left     HASHOP
%nonassoc below_DOT
%nonassoc DOT DOTOP
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW PREFIXOP STRING TRUE UIDENT
          LBRACKETPERCENT QUOTED_STRING_EXPR
/*-*/          ANTI ANTI_NOATTRS ANTI_UID ANTI_LID ANTI_LONGID ANTI_LONGLID
/*-*/          ANTI_INT ANTI_INT32 ANTI_INT64 ANTI_NATIVEINT ANTI_CHAR ANTI_STRING ANTI_DELIM ANTI_FLOAT
/*-*/          ANTI_EXPROPT ANTI_PATTOPT ANTI_CONSTANT ANTI_ALGATTRS ANTI_CASES

/* Entry points */

/* Several start symbols are marked with AVOID so that they are not used by
   [make generate-parse-errors]. The three start symbols that we keep are
   [implementation], [use_file], and [toplevel_phrase]. The latter two are
   of marginal importance; only [implementation] really matters, since most
   states in the automaton are reachable from it. */

%start implementation                   /* for implementation files */
%type <Parsetree.structure> implementation
/* BEGIN AVOID */
%start interface                        /* for interface files */
%type <Parsetree.signature> interface
/* END AVOID */
%start toplevel_phrase                  /* for interactive use */
%type <Parsetree.toplevel_phrase> toplevel_phrase
%start use_file                         /* for the #use directive */
%type <Parsetree.toplevel_phrase list> use_file
/* BEGIN AVOID */
%start parse_core_type
%type <Parsetree.core_type> parse_core_type
%start parse_expression
%type <Parsetree.expression> parse_expression
%start parse_pattern
%type <Parsetree.pattern> parse_pattern
%start parse_constr_longident
%type <Longident.t> parse_constr_longident
%start parse_val_longident
%type <Longident.t> parse_val_longident
%start parse_mty_longident
%type <Longident.t> parse_mty_longident
%start parse_mod_ext_longident
%type <Longident.t> parse_mod_ext_longident
%start parse_mod_longident
%type <Longident.t> parse_mod_longident
%start parse_any_longident
%type <Longident.t> parse_any_longident
/*-*/%start parse_module_type
/*-*/%type <Parsetree.module_type> parse_module_type
/*-*/%start parse_module_expr
/*-*/%type <Parsetree.module_expr> parse_module_expr
/*-*/%start parse_longlident
/*-*/%type <Longident.t> parse_longlident
/*-*/%start parse_value_binding
/*-*/%type <Parsetree.value_binding> parse_value_binding
/*-*/%start parse_arg_label
/*-*/%type <Asttypes.arg_label> parse_arg_label
/*-*/%start parse_extension_constructor
/*-*/%type <Parsetree.extension_constructor> parse_extension_constructor
/*-*/%start parse_binding_op
/*-*/%type <Parsetree.binding_op> parse_binding_op
/*-*/%start parse_type_declaration
/*-*/%type <Parsetree.type_declaration> parse_type_declaration
/*-*/%start parse_type_substitution
/*-*/%type <Parsetree.type_declaration> parse_type_substitution
/*-*/%start parse_constant
/*-*/%type <Parsetree.constant> parse_constant
/*-*/%start parse_structure_item
/*-*/%type <Parsetree.structure_item> parse_structure_item
/*-*/%start parse_structure
/*-*/%type <Parsetree.structure> parse_structure
/*-*/%start parse_signature_item
/*-*/%type <Parsetree.signature_item> parse_signature_item
/*-*/%start parse_signature
/*-*/%type <Parsetree.signature> parse_signature
/*-*/%start parse_row_field
/*-*/%type <Parsetree.row_field> parse_row_field
/*-*/%start parse_object_field
/*-*/%type <Parsetree.object_field> parse_object_field
/*-*/%start parse_class_description
/*-*/%type <Parsetree.class_description> parse_class_description
/*-*/%start parse_class_expr
/*-*/%type <Parsetree.class_expr> parse_class_expr
/*-*/%start parse_class_type
/*-*/%type <Parsetree.class_type> parse_class_type
/*-*/%start parse_class_field
/*-*/%type <Parsetree.class_field> parse_class_field
/*-*/%start parse_functor_parameter
/*-*/%type <Parsetree.functor_parameter Ploc.vala> parse_functor_parameter
/*-*/%start parse_module_declaration
/*-*/%type <Parsetree.module_declaration> parse_module_declaration
/*-*/%start parse_with_constraint
/*-*/%type <Parsetree.with_constraint> parse_with_constraint
/*-*/%start parse_class_type_field
/*-*/%type <Parsetree.class_type_field> parse_class_type_field
/*-*/%start parse_str_type_extension
/*-*/%type <Parsetree.type_extension> parse_str_type_extension
/*-*/%start parse_sig_type_extension
/*-*/%type <Parsetree.type_extension> parse_sig_type_extension
/* END AVOID */

/*-*/%type <Parsetree.expression list> expr_semi_list
/*-*/%type <Parsetree.expression> simple_expr
/*-*/%type <string> constr_extra_nonprefix_ident
/*-*/%type <string Ploc.vala> ident_vala name_tag_vala val_ident_vala
/*-*/%type <label> ident name_tag
/*-*/%type <Asttypes.private_flag> inline_private_flag private_flag
/*-*/%type <Parsetree.core_type> core_type core_type_no_attr
/*-*/%type <string> constr_ident
/*-*/%start parse_constructor_declaration
/*-*/%type <Parsetree.constructor_declaration> parse_constructor_declaration
/*-*/%start parse_attribute
/*-*/%type <Parsetree.attribute> parse_attribute
/*-*/%start parse_extension
/*-*/%type <Parsetree.extension> parse_extension
/*-*/%start parse_label_declaration
/*-*/%type <Parsetree.label_declaration> parse_label_declaration
/*-*/%start parse_match_case
/*-*/%type <Parsetree.case> parse_match_case
/*-*/%start parse_lident_vala_loc
/*-*/%type <string Ploc.vala Location.loc> parse_lident_vala_loc
/*-*/
/*-*/%type <Longident.t> mod_ext_longident
/*-*/%type <Longident.t> mod_longident
/*-*/%type <Longident.t Ploc.vala Asttypes.loc * Parsetree.expression> record_expr_field
/*-*/%type <Asttypes.arg_label Ploc.vala * Parsetree.expression option Ploc.vala * Parsetree.pattern> labeled_simple_pattern
/*-*/%type <string Ploc.vala option Ploc.vala> module_name
/*-*/%type <string Ploc.vala Location.loc> attr_id
/*-*/%type <(core_type * (variance * injectivity)) list> type_parameters
/*-*/%type <label_declaration list> label_declarations

%%

/* macros */
%inline extra_str(symb): symb { vaval (extra_str $startpos $endpos $1) };
%inline extra_sig(symb): symb { vaval (extra_sig $startpos $endpos $1) };
%inline extra_cstr(symb): symb { extra_cstr $startpos $endpos $1 };
%inline extra_csig(symb): symb { extra_csig $startpos $endpos $1 };
%inline extra_def(symb): symb { extra_def $startpos $endpos $1 };
%inline extra_text(symb): symb { extra_text $startpos $endpos $1 };
%inline extra_rhs(symb): symb { extra_rhs_core_type $1 ~pos:$endpos($1) };
%inline mkrhs(symb): symb
    { mkrhs $1 $sloc }
;

%inline text_str(symb): symb
  { text_str $startpos @ [$1] }
%inline text_str_SEMISEMI: SEMISEMI
  { text_str $startpos }
%inline text_sig(symb): symb
  { text_sig $startpos @ [$1] }
%inline text_sig_SEMISEMI: SEMISEMI
  { text_sig $startpos }
%inline text_def(symb): symb
  { text_def $startpos @ [$1] }
%inline top_def(symb): symb
  { Ptop_def (vaval [$1]) }
%inline text_cstr(symb): symb
  { text_cstr $startpos @ [$1] }
%inline text_csig(symb): symb
  { text_csig $startpos @ [$1] }

(* Using this %inline definition means that we do not control precisely
   when [mark_rhs_docs] is called, but I don't think this matters. *)
%inline mark_rhs_docs(symb): symb
  { mark_rhs_docs $startpos $endpos;
    $1 }

%inline op(symb): symb
   { mkoperator ~loc:$sloc (vaval $1) }

%inline mkloc(symb): symb
    { mkloc $1 (make_loc $sloc) }

%inline mkexp(symb): symb
    { mkexp ~loc:$sloc $1 }
%inline mkpat(symb): symb
    { mkpat ~loc:$sloc $1 }
%inline mktyp(symb): symb
    { mktyp ~loc:$sloc $1 }
%inline mkstr(symb): symb
    { mkstr ~loc:$sloc $1 }
%inline mksig(symb): symb
    { mksig ~loc:$sloc $1 }
%inline mkmod(symb): symb
    { mkmod ~loc:$sloc $1 }
%inline mkmty(symb): symb
    { mkmty ~loc:$sloc $1 }
%inline mkcty(symb): symb
    { mkcty ~loc:$sloc $1 }
%inline mkctf(symb): symb
    { mkctf ~loc:$sloc $1 }
%inline mkcf(symb): symb
    { mkcf ~loc:$sloc $1 }
%inline mkclass(symb): symb
    { mkclass ~loc:$sloc $1 }

%inline wrap_mkstr_ext(symb): symb
    { wrap_mkstr_ext ~loc:$sloc $1 }
%inline wrap_mksig_ext(symb): symb
    { wrap_mksig_ext ~loc:$sloc $1 }

%inline mk_directive_arg(symb): symb
    { mk_directive_arg ~loc:$sloc $1 }

/* Generic definitions */

(* [iloption(X)] recognizes either nothing or [X]. Assuming [X] produces
   an OCaml list, it produces an OCaml list, too. *)

%inline iloption(X):
  /* nothing */
    { [] }
| x = X
    { x }

(* [llist(X)] recognizes a possibly empty list of [X]s. It is left-recursive. *)

reversed_llist(X):
  /* empty */
    { [] }
| xs = reversed_llist(X) x = X
    { x :: xs }

%inline llist(X):
  xs = rev(reversed_llist(X))
    { xs }

(* [reversed_nonempty_llist(X)] recognizes a nonempty list of [X]s, and produces
   an OCaml list in reverse order -- that is, the last element in the input text
   appears first in this list. Its definition is left-recursive. *)

reversed_nonempty_llist(X):
  x = X
    { [ x ] }
| xs = reversed_nonempty_llist(X) x = X
    { x :: xs }

(* [nonempty_llist(X)] recognizes a nonempty list of [X]s, and produces an OCaml
   list in direct order -- that is, the first element in the input text appears
   first in this list. *)

%inline nonempty_llist(X):
  xs = rev(reversed_nonempty_llist(X))
    { xs }

(* [reversed_separated_nonempty_llist(separator, X)] recognizes a nonempty list
   of [X]s, separated with [separator]s, and produces an OCaml list in reverse
   order -- that is, the last element in the input text appears first in this
   list. Its definition is left-recursive. *)

(* [inline_reversed_separated_nonempty_llist(separator, X)] is semantically
   equivalent to [reversed_separated_nonempty_llist(separator, X)], but is
   marked %inline, which means that the case of a list of length one and
   the case of a list of length more than one will be distinguished at the
   use site, and will give rise there to two productions. This can be used
   to avoid certain conflicts. *)

%inline inline_reversed_separated_nonempty_llist(separator, X):
  x = X
    { [ x ] }
| xs = reversed_separated_nonempty_llist(separator, X)
  separator
  x = X
    { x :: xs }

reversed_separated_nonempty_llist(separator, X):
  xs = inline_reversed_separated_nonempty_llist(separator, X)
    { xs }

(* [separated_nonempty_llist(separator, X)] recognizes a nonempty list of [X]s,
   separated with [separator]s, and produces an OCaml list in direct order --
   that is, the first element in the input text appears first in this list. *)

%inline separated_nonempty_llist(separator, X):
  xs = rev(reversed_separated_nonempty_llist(separator, X))
    { xs }

%inline inline_separated_nonempty_llist(separator, X):
  xs = rev(inline_reversed_separated_nonempty_llist(separator, X))
    { xs }

(* [reversed_separated_nontrivial_llist(separator, X)] recognizes a list of at
   least two [X]s, separated with [separator]s, and produces an OCaml list in
   reverse order -- that is, the last element in the input text appears first
   in this list. Its definition is left-recursive. *)

reversed_separated_nontrivial_llist(separator, X):
  xs = reversed_separated_nontrivial_llist(separator, X)
  separator
  x = X
    { x :: xs }
| x1 = X
  separator
  x2 = X
    { [ x2; x1 ] }

(* [separated_nontrivial_llist(separator, X)] recognizes a list of at least
   two [X]s, separated with [separator]s, and produces an OCaml list in direct
   order -- that is, the first element in the input text appears first in this
   list. *)

%inline separated_nontrivial_llist(separator, X):
  xs = rev(reversed_separated_nontrivial_llist(separator, X))
    { xs }

(* [separated_or_terminated_nonempty_list(delimiter, X)] recognizes a nonempty
   list of [X]s, separated with [delimiter]s, and optionally terminated with a
   final [delimiter]. Its definition is right-recursive. *)

separated_or_terminated_nonempty_list(delimiter, X):
  x = X ioption(delimiter)
    { [x] }
| x = X
  delimiter
  xs = separated_or_terminated_nonempty_list(delimiter, X)
    { x :: xs }

(* [reversed_preceded_or_separated_nonempty_llist(delimiter, X)] recognizes a
   nonempty list of [X]s, separated with [delimiter]s, and optionally preceded
   with a leading [delimiter]. It produces an OCaml list in reverse order. Its
   definition is left-recursive. *)

reversed_preceded_or_separated_nonempty_llist(delimiter, X):
  ioption(delimiter) x = X
    { [x] }
| xs = reversed_preceded_or_separated_nonempty_llist(delimiter, X)
  delimiter
  x = X
    { x :: xs }

(* [preceded_or_separated_nonempty_llist(delimiter, X)] recognizes a nonempty
   list of [X]s, separated with [delimiter]s, and optionally preceded with a
   leading [delimiter]. It produces an OCaml list in direct order. *)

%inline preceded_or_separated_nonempty_llist(delimiter, X):
  xs = rev(reversed_preceded_or_separated_nonempty_llist(delimiter, X))
    { xs }

(* [bar_llist(X)] recognizes a nonempty list of [X]'s, separated with BARs,
   with an optional leading BAR. We assume that [X] is itself parameterized
   with an opening symbol, which can be [epsilon] or [BAR]. *)

(* This construction may seem needlessly complicated: one might think that
   using [preceded_or_separated_nonempty_llist(BAR, X)], where [X] is *not*
   itself parameterized, would be sufficient. Indeed, this simpler approach
   would recognize the same language. However, the two approaches differ in
   the footprint of [X]. We want the start location of [X] to include [BAR]
   when present. In the future, we might consider switching to the simpler
   definition, at the cost of producing slightly different locations. TODO *)

reversed_bar_llist(X):
    (* An [X] without a leading BAR. *)
    x = X(epsilon)
      { [x] }
  | (* An [X] with a leading BAR. *)
    x = X(BAR)
      { [x] }
  | (* An initial list, followed with a BAR and an [X]. *)
    xs = reversed_bar_llist(X)
    x = X(BAR)
      { x :: xs }

%inline bar_llist(X):
  xs = reversed_bar_llist(X)
    { List.rev xs }

(* [xlist(A, B)] recognizes [AB*]. We assume that the semantic value for [A]
   is a pair [x, b], while the semantic value for [B*] is a list [bs].
   We return the pair [x, b :: bs]. *)

%inline xlist(A, B):
  a = A bs = B*
    { let (x, b) = a in x, b :: bs }

/*-*/%inline xlist_vala(A, B):
/*-*/  a = A bs = B*
/*-*/    { let (x, b) = a in x, vaval(b :: bs) }

(* [listx(delimiter, X, Y)] recognizes a nonempty list of [X]s, optionally
   followed with a [Y], separated-or-terminated with [delimiter]s. The
   semantic value is a pair of a list of [X]s and an optional [Y]. *)

listx(delimiter, X, Y):
| x = X ioption(delimiter)
    { [x], None }
| x = X delimiter y = Y delimiter?
    { [x], Some y }
| x = X
  delimiter
  tail = listx(delimiter, X, Y)
    { let xs, y = tail in
      x :: xs, y }

(* -------------------------------------------------------------------------- *)

(* Entry points. *)

(* An .ml file. *)
implementation:
  structure EOF
    { $1 }
;

/* BEGIN AVOID */
(* An .mli file. *)
interface:
  signature EOF
    { $1 }
;
/* END AVOID */

(* A toplevel phrase. *)
toplevel_phrase:
  (* An expression with attributes, ended by a double semicolon. *)
  extra_str(text_str(str_exp))
  SEMISEMI
    { Ptop_def $1 }
| (* A list of structure items, ended by a double semicolon. *)
  extra_str(flatten(text_str(structure_item)*))
  SEMISEMI
    { Ptop_def $1 }
| (* A directive, ended by a double semicolon. *)
  toplevel_directive
  SEMISEMI
    { $1 }
| (* End of input. *)
  EOF
    { raise End_of_file }
;

(* An .ml file that is read by #use. *)
use_file:
  (* An optional standalone expression,
     followed with a series of elements,
     followed with EOF. *)
  extra_def(append(
    optional_use_file_standalone_expression,
    flatten(use_file_element*)
  ))
  EOF
    { $1 }
;

(* An optional standalone expression is just an expression with attributes
   (str_exp), with extra wrapping. *)
%inline optional_use_file_standalone_expression:
  iloption(text_def(top_def(str_exp)))
    { $1 }
;

(* An element in a #used file is one of the following:
   - a double semicolon followed with an optional standalone expression;
   - a structure item;
   - a toplevel directive.
 *)
%inline use_file_element:
  preceded(SEMISEMI, optional_use_file_standalone_expression)
| text_def(top_def(structure_item))
| text_def(mark_rhs_docs(toplevel_directive))
      { $1 }
;

/* BEGIN AVOID */
parse_core_type:
  core_type EOF
    { $1 }
;

parse_expression:
  seq_expr EOF
    { $1 }
;

parse_pattern:
  pattern EOF
    { $1 }
;

parse_mty_longident:
  mty_longident EOF
    { $1 }
;

parse_val_longident:
  val_longident EOF
    { $1 }
;

parse_constr_longident:
  constr_longident EOF
    { $1 }
;

parse_mod_ext_longident:
  mod_ext_longident EOF
    { $1 }
;

parse_mod_longident:
  mod_longident EOF
    { $1 }
;

parse_any_longident:
  any_longident EOF
    { $1 }
;
/*-*/parse_longlident:
/*-*/  type_longident EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_structure_item:
/*-*/  structure_item EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_structure:
/*-*/  structure EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_signature_item:
/*-*/  signature_item EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_signature:
/*-*/  signature EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_constructor_declaration:
/*-*/  constructor_declaration(epsilon) EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_attribute:
/*-*/  attribute EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_extension:
/*-*/  extension EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_label_declaration:
/*-*/  label_declaration EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_match_case:
/*-*/  match_case EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_module_type:
/*-*/  module_type EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_module_expr:
/*-*/  module_expr EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_value_binding:
/*-*/  value_binding EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_type_declaration:
/*-*/  core_type_declaration(type_kind) EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_type_substitution:
/*-*/  core_type_declaration(type_subst_kind) EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_arg_label:
/*-*/  arg_label EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_extension_constructor:
/*-*/  extension_constructor(epsilon) EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_binding_op:
/*-*/  pbop_op = mkrhs(vaval(ANDOP)) body = letop_binding_body EOF
/*-*/    { let pbop_pat, pbop_exp = body in
(*-*)      let pbop_loc = make_loc $sloc in
(*-*)      {pbop_op; pbop_pat; pbop_exp; pbop_loc} }
/*-*/| pbop_op = mkrhs(vaval(LETOP)) body = letop_binding_body EOF
/*-*/    { let pbop_pat, pbop_exp = body in
(*-*)      let pbop_loc = make_loc $sloc in
(*-*)      {pbop_op; pbop_pat; pbop_exp; pbop_loc} }
/*-*/| pbop_op = ANTI_LID body = letop_binding_body EOF
/*-*/    { let pbop_pat, pbop_exp = body in
(*-*)      let pbop_loc = make_loc $sloc in
(*-*)      let pbop_op = mkrhs (vaant pbop_op) $sloc in
(*-*)      {pbop_op; pbop_pat; pbop_exp; pbop_loc} }
/*-*/;
/*-*/
/*-*/parse_lident_vala_loc:
/*-*/  mkloc(lident_vala) EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_constant:
/*-*/  constant EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_row_field:
/*-*/  row_field EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_object_field:
/*-*/  field EOF
/*-*/    { $1 }
/*-*/| inherit_field EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_class_description:
/*-*/  class_description EOF
/*-*/    { snd $1 }
/*-*/;
/*-*/
/*-*/parse_class_expr:
/*-*/  class_expr EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_class_type:
/*-*/  class_type EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_class_field:
/*-*/  class_field EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_functor_parameter:
/*-*/  functor_arg EOF
/*-*/    { snd $1 }
/*-*/;
/*-*/
/*-*/parse_module_declaration:
/*-*/  module_declaration EOF
/*-*/    { fst $1 }
/*-*/;
/*-*/
/*-*/parse_with_constraint:
/*-*/  with_constraint EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_class_type_field:
/*-*/  class_sig_field EOF
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/parse_str_type_extension:
/*-*/  str_type_extension EOF
/*-*/    { fst $1 }
/*-*/;
/*-*/
/*-*/parse_sig_type_extension:
/*-*/  sig_type_extension EOF
/*-*/    { fst $1 }
/*-*/;
/*-*/
/* END AVOID */

(* -------------------------------------------------------------------------- *)

(* Functor arguments appear in module expressions and module types. *)

%inline functor_args:
  reversed_nonempty_llist(functor_arg)
    { $1 }
    (* Produce a reversed list on purpose;
       later processed using [fold_left]. *)
;

functor_arg:
    (* An anonymous and untyped argument. *)
    LPAREN RPAREN
      { $startpos, vaval Unit }
  | (* An argument accompanied with an explicit type. *)
    LPAREN x = mkrhs(module_name) COLON mty = module_type RPAREN
      { $startpos, vaval (Named (x, mty)) }
/*-*/  | ANTI_OPT
/*-*/      { $startpos, vaant $1 }
;

module_name:
    vala(module_name_, ANTI_OPT) { $1 }
;
%inline module_name_:
    (* A named argument. *)
    x = uident_vala
      { Some x }
  | (* An anonymous argument. *)
    UNDERSCORE
      { None }
;

(* the antiquotations that can be used in a position that produces a "xtr" *)
/*-*/  %inline xtr_antis: ANTI { $1 } | ANTI_NOATTRS { $1 } ;

(* -------------------------------------------------------------------------- *)

(* Module expressions. *)

(* The syntax of module expressions is not properly stratified. The cases of
   functors, functor applications, and attributes interact and cause conflicts,
   which are resolved by precedence declarations. This is concise but fragile.
   Perhaps in the future an explicit stratification could be used. *)

module_expr:
  | STRUCT attrs = vaval(attributes) s = structure END
      { mkmod ~loc:$sloc ~attrs (Pmod_structure s) }
  | STRUCT vaval(attributes) structure error
      { unclosed "struct" $loc($1) "end" $loc($4) }
  | FUNCTOR attrs = vaval(attributes) args = functor_args MINUSGREATER me = module_expr
      { wrap_mod_attrs ~loc:$sloc attrs (
          List.fold_left (fun acc (startpos, arg) ->
            mkmod ~loc:(startpos, $endpos) (Pmod_functor (arg, acc))
          ) me args
        ) }
  | me = paren_module_expr
      { me }
  | me = module_expr attr = attribute
      { Mod.attr me attr }
/*-*/  | me = module_expr ANTI_ALGATTRS
/*-*/      { Mod.attrs me (vaant $2) }
  | mkmod(
      (* A module identifier. *)
      x = mkrhs(mod_longident_vala)
        { Pmod_ident x }
/*-*/    | xtr_antis { Pmod_xtr (Location.mkloc $1 (make_loc $sloc)) }
    | (* In a functor application, the actual argument must be parenthesized. *)
      me1 = module_expr me2 = paren_module_expr
        { Pmod_apply(me1, me2) }
    | (* Application to unit is sugar for application to an empty structure. *)
      me1 = module_expr LPAREN RPAREN
        { (* TODO review mkmod location *)
          Pmod_apply(me1, mkmod ~loc:$sloc (Pmod_structure (vaval []))) }
    | (* An extension. *)
      ex = extension
        { Pmod_extension ex }
    )
    { $1 }
;

(* A parenthesized module expression is a module expression that begins
   and ends with parentheses. *)

paren_module_expr:
    (* A module expression annotated with a module type. *)
    LPAREN me = module_expr COLON mty = module_type RPAREN
      { mkmod ~loc:$sloc (Pmod_constraint(me, mty)) }
  | LPAREN module_expr COLON module_type error
      { unclosed "(" $loc($1) ")" $loc($5) }
  | (* A module expression within parentheses. *)
    LPAREN me = module_expr RPAREN
      { me (* TODO consider reloc *) }
  | LPAREN module_expr error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | (* A core language expression that produces a first-class module.
       This expression can be annotated in various ways. *)
    LPAREN VAL attrs = vaval(attributes) e = expr_colon_package_type RPAREN
      { mkmod ~loc:$sloc ~attrs (Pmod_unpack e) }
  | LPAREN VAL vaval(attributes) expr COLON error
      { unclosed "(" $loc($1) ")" $loc($6) }
  | LPAREN VAL vaval(attributes) expr COLONGREATER error
      { unclosed "(" $loc($1) ")" $loc($6) }
  | LPAREN VAL vaval(attributes) expr error
      { unclosed "(" $loc($1) ")" $loc($5) }
;

(* The various ways of annotating a core language expression that
   produces a first-class module that we wish to unpack. *)
%inline expr_colon_package_type:
    e = expr
      { e }
  | e = expr COLON ty = package_type
      { ghexp ~loc:$loc (Pexp_constraint (e, ty)) }
  | e = expr COLON ty1 = package_type COLONGREATER ty2 = package_type
      { ghexp ~loc:$loc (Pexp_coerce (e, vaval (Some ty1), ty2)) }
  | e = expr COLONGREATER ty2 = package_type
      { ghexp ~loc:$loc (Pexp_coerce (e, vaval None, ty2)) }
;

(* A structure, which appears between STRUCT and END (among other places),
   begins with an optional standalone expression, and continues with a list
   of structure elements. *)
structure:
  extra_str(append(
    optional_structure_standalone_expression,
    flatten(structure_element*)
  ))
  { $1 }
/*-*/| ANTI_LIST { vaant $1 }
;

(* An optional standalone expression is just an expression with attributes
   (str_exp), with extra wrapping. *)
%inline optional_structure_standalone_expression:
  items = iloption(mark_rhs_docs(text_str(str_exp)))
    { items }
;

(* An expression with attributes, wrapped as a structure item. *)
%inline str_exp:
  e = vaval(seq_expr)
  attrs = post_item_attributes_vala
    { mkstrexp ~loc:$sloc e attrs }
;

(* A structure element is one of the following:
   - a double semicolon followed with an optional standalone expression;
   - a structure item. *)
%inline structure_element:
    append(text_str_SEMISEMI, optional_structure_standalone_expression)
  | text_str(structure_item)
      { $1 }
;

(* A structure item. *)
structure_item:
    let_bindings(ext)
      { val_of_let_bindings ~loc:$sloc $1 }
  | mkstr(
      item_extension post_item_attributes_vala
        { let docs = symbol_docs $sloc in
          Pstr_extension ($1, add_docs_attrs docs $2) }
    | floating_attribute
        { Pstr_attribute $1 }
    )
  | wrap_mkstr_ext(
      primitive_declaration
        { pstr_primitive $1 }
    | value_description
        { pstr_primitive $1 }
    | type_declarations
        { pstr_type $1 }
/*-*/    | TYPE
/*-*/      ext = ext
/*-*/      attrs1 = vaval(attributes)
/*-*/      nr = nonrec_flag_vala
/*-*/      l = ANTI_LIST
/*-*/      { assert (ext = None) ;
(*-*)        assert (unvala attrs1 = []) ;
(*-*)        pstr_type((nr, None), vaant l)
(*-*)      }
    | str_type_extension
        { pstr_typext $1 }
    | str_exception_declaration
        { pstr_exception $1 }
    | module_binding
        { $1 }
    | rec_module_bindings
        { pstr_recmodule $1 }
    | module_type_declaration
        { let (body, ext) = $1 in (Pstr_modtype body, ext) }
    | open_declaration
        { let (body, ext) = $1 in (Pstr_open body, ext) }
    | class_declarations
        { let (ext, l) = $1 in (Pstr_class l, ext) }
    | class_type_declarations
        { let (ext, l) = $1 in (Pstr_class_type l, ext) }
    | include_statement(module_expr)
        { pstr_include $1 }
/*-*/    | ANTI_EXPR { Pstr_eval (vaant $1, vaval []), None }
    )
    { $1 }
;

(* A single module binding. *)
%inline module_binding:
  MODULE
  ext = ext attrs1 = vaval(attributes)
  name = mkrhs(module_name)
  body = module_binding_body
  attrs2 = post_item_attributes_vala
    { let docs = symbol_docs $sloc in
      let loc = make_loc $sloc in
      let attrs = append_list_vala attrs1 attrs2 in
      let body = Mb.mk name body ~attrs ~loc ~docs in
      Pstr_module body, ext }
;

(* The body (right-hand side) of a module binding. *)
module_binding_body:
    EQUAL me = module_expr
      { me }
  | mkmod(
      COLON mty = module_type EQUAL me = module_expr
        { Pmod_constraint(me, mty) }
    | arg_and_pos = functor_arg body = module_binding_body
        { let (_, arg) = arg_and_pos in
          Pmod_functor(arg, body) }
  ) { $1 }
;

(* A group of recursive module bindings. *)
%inline rec_module_bindings:
  xlist(rec_module_binding, and_module_binding)
    { let (a,b) = $1 in a, vaval b }
/*-*/| MODULE
/*-*/  ext = ext
/*-*/  attrs1 = vaval(attributes)
/*-*/  REC
/*-*/  l = ANTI_LIST
/*-*/  {
(*-*)    assert (ext = None) ;
(*-*)    assert (unvala attrs1 = []) ;
(*-*)    None, vaant l
(*-*)  }
;

(* The first binding in a group of recursive module bindings. *)
%inline rec_module_binding:
  MODULE
  ext = ext
  attrs1 = vaval(attributes)
  REC
  name = mkrhs(module_name)
  body = module_binding_body
  attrs2 = post_item_attributes_vala
  {
    let loc = make_loc $sloc in
    let attrs = append_list_vala attrs1 attrs2 in
    let docs = symbol_docs $sloc in
    ext,
    Mb.mk name body ~attrs ~loc ~docs
  }
;

(* The following bindings in a group of recursive module bindings. *)
%inline and_module_binding:
  AND
  attrs1 = vaval(attributes)
  name = mkrhs(module_name)
  body = module_binding_body
  attrs2 = post_item_attributes_vala
  {
    let loc = make_loc $sloc in
    let attrs = append_list_vala attrs1 attrs2 in
    let docs = symbol_docs $sloc in
    let text = symbol_text $symbolstartpos in
    Mb.mk name body ~attrs ~loc ~text ~docs
  }
;

(* -------------------------------------------------------------------------- *)

(* Shared material between structures and signatures. *)

(* An [include] statement can appear in a structure or in a signature,
   which is why this definition is parameterized. *)
%inline include_statement(thing):
  INCLUDE
  ext = ext
  attrs1 = vaval(attributes)
  thing = thing
  attrs2 = post_item_attributes_vala
  {
    let attrs = append_list_vala attrs1 attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Incl.mk thing ~attrs ~loc ~docs, ext
  }
;

(* A module type declaration. *)
module_type_declaration:
  MODULE TYPE
  ext = ext
  attrs1 = vaval(attributes)
  id = mkrhs(ident_vala)
  typ = preceded(EQUAL, module_type)?
  attrs2 = post_item_attributes_vala
  {
    let attrs = append_list_vala attrs1 attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Mtd.mk id ~typ:(vaval typ) ~attrs ~loc ~docs, ext
  }
/*-*/| MODULE TYPE
/*-*/  ext = ext
/*-*/  attrs1 = vaval(attributes)
/*-*/  id = mkrhs(ident_vala)
/*-*/  typ = ANTI_OPT
/*-*/  attrs2 = post_item_attributes_vala
/*-*/  {
(*-*)    let attrs = append_list_vala attrs1 attrs2 in
(*-*)    let loc = make_loc $sloc in
(*-*)    let docs = symbol_docs $sloc in
(*-*)    Mtd.mk id ~typ:(vaant typ) ~attrs ~loc ~docs, ext
(*-*)  }
;

(* -------------------------------------------------------------------------- *)

(* Opens. *)

open_declaration:
  OPEN
  override = override_flag_vala
  ext = ext
  attrs1 = vaval(attributes)
  me = module_expr
  attrs2 = post_item_attributes_vala
  {
    let attrs = append_list_vala attrs1 attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Opn.mk me ~override ~attrs ~loc ~docs, ext
  }
;

open_description:
  OPEN
  override = override_flag_vala
  ext = ext
  attrs1 = vaval(attributes)
  id = mkrhs(mod_ext_longident_vala)
  attrs2 = post_item_attributes_vala
  {
    let attrs = append_list_vala attrs1 attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Opn.mk id ~override ~attrs ~loc ~docs, ext
  }
;

%inline open_dot_declaration: mkrhs(mod_longident_vala)
  { let loc = make_loc $loc($1) in
    let me = Mod.ident ~loc $1 in
    Opn.mk ~loc me }
;

(* -------------------------------------------------------------------------- *)

/* Module types */

module_type:
  | SIG attrs = vaval(attributes) s = signature END
      { mkmty ~loc:$sloc ~attrs (Pmty_signature s) }
  | SIG vaval(attributes) signature error
      { unclosed "sig" $loc($1) "end" $loc($4) }
  | FUNCTOR attrs = vaval(attributes) args = functor_args
    MINUSGREATER mty = module_type
      %prec below_WITH
      { wrap_mty_attrs ~loc:$sloc attrs (
          List.fold_left (fun acc (startpos, arg) ->
            mkmty ~loc:(startpos, $endpos) (Pmty_functor (arg, acc))
          ) mty args
        ) }
  | MODULE TYPE OF vaval(attributes) module_expr %prec below_LBRACKETAT
      { mkmty ~loc:$sloc ~attrs:$4 (Pmty_typeof $5) }
  | LPAREN module_type RPAREN
      { $2 }
  | LPAREN module_type error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | module_type attribute
      { Mty.attr $1 $2 }
/*-*/  | module_type ANTI_ALGATTRS
/*-*/      { Mty.attrs $1 (vaant $2) }
  | mkmty(
      mkrhs(mty_longident)
        { Pmty_ident $1 }
/*-*/    | xtr_antis { Pmty_xtr (Location.mkloc $1 (make_loc $sloc)) }
    | module_type MINUSGREATER module_type
        %prec below_WITH
        { Pmty_functor(vaval (Named (mknoloc (vaval None), $1)), $3) }
    | module_type WITH vala(separated_nonempty_llist(AND, with_constraint), ANTI_LIST)
        { Pmty_with($1, $3) }
    | LPAREN MODULE mkrhs(mod_longident_vala) RPAREN
        { Pmty_alias $3 }
    | extension
        { Pmty_extension $1 }
    )
    { $1 }
;
(* A signature, which appears between SIG and END (among other places),
   is a list of signature elements. *)
signature:
  extra_sig(flatten(signature_element*))
    { $1 }
/*-*/| ANTI_LIST { vaant $1 }
;

(* A signature element is one of the following:
   - a double semicolon;
   - a signature item. *)
%inline signature_element:
    text_sig_SEMISEMI
  | text_sig(signature_item)
      { $1 }
;

(* A signature item. *)
signature_item:
  | item_extension post_item_attributes_vala
      { let docs = symbol_docs $sloc in
        mksig ~loc:$sloc (Psig_extension ($1, (add_docs_attrs docs $2))) }
  | mksig(
      floating_attribute
        { Psig_attribute $1 }
    )
    { $1 }
  | wrap_mksig_ext(
      value_description
        { psig_value $1 }
    | primitive_declaration
        { psig_value $1 }
    | type_declarations
        { psig_type $1 }
/*-*/    | TYPE
/*-*/      ext = ext
/*-*/      attrs1 = vaval(attributes)
/*-*/      nr = nonrec_flag_vala
/*-*/      l = ANTI_LIST
/*-*/      { assert (ext = None) ;
(*-*)        assert (unvala attrs1 = []) ;
(*-*)        psig_type((nr, None), vaant l)
(*-*)      }
    | type_subst_declarations
        { psig_typesubst $1 }
/*-*/    | TYPESUBST
/*-*/      l = ANTI_LIST
/*-*/      { psig_typesubst((Recursive, None), vaant l) }
    | sig_type_extension
        { psig_typext $1 }
    | sig_exception_declaration
        { psig_exception $1 }
    | module_declaration
        { let (body, ext) = $1 in (Psig_module body, ext) }
    | module_alias
        { let (body, ext) = $1 in (Psig_module body, ext) }
    | module_subst
        { let (body, ext) = $1 in (Psig_modsubst body, ext) }
    | rec_module_declarations
        { let (ext, l) = $1 in (Psig_recmodule l, ext) }
    | module_type_declaration
        { let (body, ext) = $1 in (Psig_modtype body, ext) }
    | module_type_subst
        { let (body, ext) = $1 in (Psig_modtypesubst body, ext) }
    | open_description
        { let (body, ext) = $1 in (Psig_open body, ext) }
    | include_statement(module_type)
        { psig_include $1 }
    | class_descriptions
        { let (ext, l) = $1 in (Psig_class l, ext) }
    | class_type_declarations
        { let (ext, l) = $1 in (Psig_class_type l, ext) }
    )
    { $1 }

(* A module declaration. *)
%inline module_declaration:
  MODULE
  ext = ext attrs1 = vaval(attributes)
  name = mkrhs(module_name)
  body = module_declaration_body
  attrs2 = post_item_attributes_vala
  {
    let attrs = append_list_vala attrs1 attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Md.mk name body ~attrs ~loc ~docs, ext
  }
;

(* The body (right-hand side) of a module declaration. *)
module_declaration_body:
    COLON mty = module_type
      { mty }
  | mkmty(
      arg_and_pos = functor_arg body = module_declaration_body
        { let (_, arg) = arg_and_pos in
          Pmty_functor(arg, body) }
    )
    { $1 }
;

(* A module alias declaration (in a signature). *)
%inline module_alias:
  MODULE
  ext = ext attrs1 = vaval(attributes)
  name = mkrhs(module_name)
  EQUAL
  body = module_expr_alias
  attrs2 = post_item_attributes_vala
  {
    let attrs = append_list_vala attrs1 attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Md.mk name body ~attrs ~loc ~docs, ext
  }
;
%inline module_expr_alias:
  id = mkrhs(mod_longident_vala)
    { Mty.alias ~loc:(make_loc $sloc) id }
;
(* A module substitution (in a signature). *)
module_subst:
  MODULE
  ext = ext attrs1 = vaval(attributes)
  uid = mkrhs(uident_vala)
  COLONEQUAL
  body = mkrhs(mod_ext_longident_vala)
  attrs2 = post_item_attributes_vala
  {
    let attrs = append_list_vala attrs1 attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Ms.mk uid body ~attrs ~loc ~docs, ext
  }
| MODULE ext vaval(attributes) mkrhs(UIDENT) COLONEQUAL error
    { expecting $loc($6) "module path" }
;

(* A group of recursive module declarations. *)
%inline rec_module_declarations:
  xlist(rec_module_declaration, and_module_declaration)
    { let a,b = $1 in a, vaval b }
/*-*/| MODULE
/*-*/  ext = ext
/*-*/  attrs1 = vaval(attributes)
/*-*/  REC
/*-*/  l = ANTI_LIST
/*-*/  { assert (ext = None) ;
(*-*)    assert (unvala attrs1 = []) ;
(*-*)    None, vaant l
(*-*)  }
;
%inline rec_module_declaration:
  MODULE
  ext = ext
  attrs1 = vaval(attributes)
  REC
  name = mkrhs(module_name)
  COLON
  mty = module_type
  attrs2 = post_item_attributes_vala
  {
    let attrs = append_list_vala attrs1 attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    ext, Md.mk name mty ~attrs ~loc ~docs
  }
;
%inline and_module_declaration:
  AND
  attrs1 = vaval(attributes)
  name = mkrhs(module_name)
  COLON
  mty = module_type
  attrs2 = post_item_attributes_vala
  {
    let attrs = append_list_vala attrs1 attrs2 in
    let docs = symbol_docs $sloc in
    let loc = make_loc $sloc in
    let text = symbol_text $symbolstartpos in
    Md.mk name mty ~attrs ~loc ~text ~docs
  }
;

(* A module type substitution *)
module_type_subst:
  MODULE TYPE
  ext = ext
  attrs1 = vaval(attributes)
  id = mkrhs(ident_vala)
  COLONEQUAL
  typ=module_type
  attrs2 = post_item_attributes_vala
  {
    let attrs = append_list_vala attrs1 attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Mtd.mk id ~typ:(vaval (Some typ)) ~attrs ~loc ~docs, ext
  }


(* -------------------------------------------------------------------------- *)

(* Class declarations. *)

%inline class_declarations:
  xlist(class_declaration, and_class_declaration)
    { let (a,b) = $1 in a, vaval b }
/*-*/| CLASS ANTI_CLASSLIST
/*-*/   { None, vaant $2 }
;
%inline class_declaration:
  CLASS
  ext = ext
  attrs1 = vaval(attributes)
  virt = virtual_flag_vala
  params = vala(formal_class_parameters, ANTI_LIST)
  id = mkrhs(lident_vala)
  body = class_fun_binding
  attrs2 = post_item_attributes_vala
  {
    let attrs = append_list_vala attrs1 attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    ext,
    Ci.mk id body ~virt ~params ~attrs ~loc ~docs
  }
;
%inline and_class_declaration:
  AND
  attrs1 = vaval(attributes)
  virt = virtual_flag_vala
  params = vala(formal_class_parameters, ANTI_LIST)
  id = mkrhs(lident_vala)
  body = class_fun_binding
  attrs2 = post_item_attributes_vala
  {
    let attrs = append_list_vala attrs1 attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    let text = symbol_text $symbolstartpos in
    Ci.mk id body ~virt ~params ~attrs ~loc ~text ~docs
  }
;

class_fun_binding:
    EQUAL class_expr
      { $2 }
  | mkclass(
      COLON class_type EQUAL class_expr
        { Pcl_constraint($4, $2) }
    | labeled_simple_pattern class_fun_binding
      { let (l,o,p) = $1 in Pcl_fun(l, o, p, $2) }
    ) { $1 }
;

formal_class_parameters:
  params = class_parameters(type_parameter)
    { params }
;

(* -------------------------------------------------------------------------- *)

(* Class expressions. *)

class_expr:
    class_simple_expr
      { $1 }
  | FUN vaval(attributes) class_fun_def
      { wrap_class_attrs ~loc:$sloc $3 $2 }
  | let_bindings(no_ext) IN class_expr
      { class_of_let_bindings ~loc:$sloc $1 $3 }
  | LET OPEN override_flag_vala vaval(attributes) mkrhs(mod_longident_vala) IN class_expr
      { let loc = ($startpos($2), $endpos($5)) in
        let od = Opn.mk ~override:$3 ~loc:(make_loc loc) $5 in
        mkclass ~loc:$sloc ~attrs:$4 (Pcl_open(od, $7)) }
  | class_expr attribute
      { Cl.attr $1 $2 }
/*-*/  | class_expr ANTI_ALGATTRS
/*-*/      { Cl.attrs $1 (vaant $2) }
  | mkclass(
      class_simple_expr vala(nonempty_llist(labeled_simple_expr), ANTI_LIST)
        { Pcl_apply($1, Pcaml.vala_map (List.map (fun (a,b) -> (a, b))) $2) }
    | extension
        { Pcl_extension $1 }
    ) { $1 }
;
class_simple_expr:
  | LPAREN class_expr RPAREN
      { $2 }
  | LPAREN class_expr error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | mkclass(
      tys = vala(actual_class_parameters, ANTI_LIST) cid = mkrhs(class_longident)
        { Pcl_constr(cid, tys) }
    | OBJECT vaval(attributes) class_structure error
        { unclosed "object" $loc($1) "end" $loc($4) }
    | LPAREN class_expr COLON class_type RPAREN
        { Pcl_constraint($2, $4) }
    | LPAREN class_expr COLON class_type error
        { unclosed "(" $loc($1) ")" $loc($5) }
    ) { $1 }
  | OBJECT vaval(attributes) class_structure END
    { mkclass ~loc:$sloc ~attrs:$2 (Pcl_structure $3) }
/*-*/  | xtr_antis
/*-*/      { mkclass ~loc:$sloc (Pcl_xtr (Location.mkloc $1 (make_loc $sloc))) }
;

class_fun_def:
  mkclass(
    labeled_simple_pattern MINUSGREATER e = class_expr
  | labeled_simple_pattern e = class_fun_def
      { let (l,o,p) = $1 in Pcl_fun(l, o, p, e) }
  ) { $1 }
;
%inline class_structure:
  |  vaval(class_self_pattern) vala(extra_cstr(class_fields), ANTI_LIST)
       { Cstr.mk $1 $2 }
/*-*/  |  ANTI_PATT vala(extra_cstr(class_fields), ANTI_LIST)
/*-*/       { Cstr.mk (vaant $1) $2 }
;
class_self_pattern:
    LPAREN pattern RPAREN
      { reloc_pat ~loc:$sloc $2 }
  | mkpat(LPAREN pattern COLON core_type RPAREN
      { Ppat_constraint($2, $4) })
      { $1 }
  | /* empty */
      { ghpat ~loc:$sloc Ppat_any }
;
%inline class_fields:
  flatten(text_cstr(class_field)*)
    { $1 }
;
class_field:
  | INHERIT override_flag_vala vaval(attributes) class_expr
    self = vala(preceded(AS, mkrhs(lident_vala))?, ANTI_OPT)
    post_item_attributes_vala
      { let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_inherit ($2, $4, self)) ~attrs:(append_list_vala $3 $6) ~docs }
  | VAL value post_item_attributes_vala
      { let v, attrs = $2 in
        let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_val v) ~attrs:(append_list_vala attrs $3) ~docs }
  | METHOD method_ post_item_attributes_vala
      { let meth, attrs = $2 in
        let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_method meth) ~attrs:(append_list_vala attrs $3) ~docs }
  | CONSTRAINT vaval(attributes) constrain_field post_item_attributes_vala
      { let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_constraint $3) ~attrs:(append_list_vala $2 $4) ~docs }
  | INITIALIZER vaval(attributes) seq_expr post_item_attributes_vala
      { let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_initializer $3) ~attrs:(append_list_vala $2 $4) ~docs }
  | item_extension post_item_attributes_vala
      { let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_extension $1) ~attrs:$2 ~docs }
  | mkcf(floating_attribute
      { Pcf_attribute $1 })
      { $1 }
;
value:
    no_override_flag
    attrs = vaval(attributes)
    mutable_ = vala(virtual_with_mutable_flag, ANTI_MUTABLE)
    label = mkrhs(label_vala) COLON ty = core_type
      { (label, mutable_, Cfk_virtual ty), attrs }
  | override_flag_vala vaval(attributes) mutable_flag_vala mkrhs(label_vala) EQUAL seq_expr
      { ($4, $3, Cfk_concrete ($1, $6)), $2 }
  | override_flag_vala vaval(attributes) mutable_flag_vala mkrhs(label_vala) type_constraint
    EQUAL seq_expr
      { let e = mkexp_constraint ~loc:$sloc $7 $5 in
        ($4, $3, Cfk_concrete ($1, e)), $2
      }
;
method_:
    no_override_flag
    attrs = vaval(attributes)
    private_ = vala(virtual_with_private_flag, ANTI_PRIV)
    label = mkrhs(label_vala) COLON ty = poly_type
      { (label, private_, Cfk_virtual ty), attrs }
  | vaval(override_flag) vaval(attributes) private_flag_vala mkrhs(label_vala) strict_binding
      { let e = $5 in
        let loc = Location.(e.pexp_loc.loc_start, e.pexp_loc.loc_end) in
        ($4, $3,
        Cfk_concrete ($1, ghexp ~loc (Pexp_poly (e, None)))), $2 }
  | vaval(override_flag) vaval(attributes) private_flag_vala mkrhs(label_vala)
    COLON poly_type EQUAL seq_expr
      { let poly_exp =
          let loc = ($startpos($6), $endpos($8)) in
          ghexp ~loc (Pexp_poly($8, Some $6)) in
        ($4, $3, Cfk_concrete ($1, poly_exp)), $2 }
  | vaval(override_flag) vaval(attributes) private_flag_vala mkrhs(label_vala) COLON TYPE lident_list
    DOT core_type EQUAL seq_expr
      { let poly_exp_loc = ($startpos($7), $endpos($11)) in
        let poly_exp =
          let exp, poly =
            (* it seems odd to use the global ~loc here while poly_exp_loc
               is tighter, but this is what ocamlyacc does;
               TODO improve parser.mly *)
            wrap_type_annotation ~loc:$sloc $7 $9 $11 in
          ghexp ~loc:poly_exp_loc (Pexp_poly(exp, Some poly)) in
        ($4, $3,
        Cfk_concrete ($1, poly_exp)), $2 }
/*-*/  | ANTI_OVERRIDEFLAG private_flag_vala mkrhs(label_vala) EQUAL ANTI
/*-*/     {
(*-*)       let e = mkexp ~loc:$sloc (Pexp_xtr (Location.mkloc $5 (make_loc $sloc))) in
(*-*)       (($3, $2, Cfk_concrete(vaant $1, e)), vaval [])
(*-*)     }
;

/* Class types */

class_type:
    class_signature
      { $1 }
  | mkcty(
      label = arg_label_vala
      domain = tuple_type
      MINUSGREATER
      codomain = class_type
        { Pcty_arrow(label, domain, codomain) }
    ) { $1 }
 ;
class_signature:
    mkcty(
      tys = vala(actual_class_parameters, ANTI_LIST) cid = mkrhs(clty_longident)
        { Pcty_constr (cid, tys) }
    | extension
        { Pcty_extension $1 }
    ) { $1 }
  | OBJECT vaval(attributes) class_sig_body END
      { mkcty ~loc:$sloc ~attrs:$2 (Pcty_signature $3) }
  | OBJECT vaval(attributes) class_sig_body error
      { unclosed "object" $loc($1) "end" $loc($4) }
  | class_signature attribute
      { Cty.attr $1 $2 }
/*-*/  | class_signature ANTI_ALGATTRS
/*-*/      { Cty.attrs $1 (vaant $2) }
  | LET OPEN override_flag_vala attributes_vala mkrhs(mod_longident_vala) IN class_signature
      { let loc = ($startpos($2), $endpos($5)) in
        let od = Opn.mk ~override:$3 ~loc:(make_loc loc) $5 in
        mkcty ~loc:$sloc ~attrs:$4 (Pcty_open(od, $7)) }
/*-*/  | xtr_antis
/*-*/      { mkcty ~loc:$sloc (Pcty_xtr (Location.mkloc $1 (make_loc $sloc))) }
;
%inline class_parameters(parameter):
  | /* empty */
      { [] }
  | LBRACKET params = separated_nonempty_llist(COMMA, parameter) RBRACKET
      { params }
;
%inline actual_class_parameters:
  tys = class_parameters(core_type)
    { tys }
;
%inline class_sig_body:
    class_self_type vala(extra_csig(class_sig_fields), ANTI_LIST)
      { Csig.mk $1 $2 }
;
class_self_type:
    LPAREN core_type RPAREN
      { $2 }
  | mktyp((* empty *) { Ptyp_any })
      { $1 }
;
%inline class_sig_fields:
  flatten(text_csig(class_sig_field)*)
    { $1 }
;
class_sig_field:
    INHERIT vaval(attributes) class_signature post_item_attributes_vala
      { let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_inherit $3) ~attrs:(append_list_vala $2 $4) ~docs }
  | VAL vaval(attributes) value_type post_item_attributes_vala
      { let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_val $3) ~attrs:(append_list_vala $2 $4) ~docs }
  | METHOD vaval(attributes) private_virtual_flags mkrhs(label_vala) COLON poly_type
    post_item_attributes_vala
      { let (p, v) = $3 in
        let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_method ($4, vaval p, vaval v, $6)) ~attrs:(append_list_vala $2 $7) ~docs }
/*-*/  | METHOD vaval(attributes) p = ANTI_PRIV v = ANTI_VIRTUAL mkrhs(label_vala) COLON poly_type
/*-*/    post_item_attributes_vala
/*-*/      { let docs = symbol_docs $sloc in
(*-*)        mkctf ~loc:$sloc (Pctf_method ($5, vaant p, vaant v, $7)) ~attrs:(append_list_vala $2 $8) ~docs }
  | CONSTRAINT vaval(attributes) constrain_field post_item_attributes_vala
      { let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_constraint $3) ~attrs:(append_list_vala $2 $4) ~docs }
  | item_extension post_item_attributes_vala
      { let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_extension $1) ~attrs:$2 ~docs }
  | mkctf(floating_attribute
      { Pctf_attribute $1 })
      { $1 }
;
%inline value_type:
  flags = mutable_virtual_flags
  label = mkrhs(label_vala)
  COLON
  ty = core_type
  {
    let mut, virt = flags in
    label, vaval mut, vaval virt, ty
  }
/*-*/| mut = ANTI_MUTABLE
/*-*/  virt = ANTI_VIRTUAL
/*-*/  label = mkrhs(label_vala)
/*-*/  COLON
/*-*/  ty = core_type
/*-*/  {
(*-*)    label, vaant mut, vaant virt, ty
(*-*)  }
;
%inline constrain:
    core_type_no_attr EQUAL core_type_no_attr
    { $1, $3, make_loc $sloc }
;
constrain_field:
  core_type EQUAL core_type
    { $1, $3 }
;
(* A group of class descriptions. *)
%inline class_descriptions:
  xlist(class_description, and_class_description)
    { let (a,b) = $1 in a, vaval b }
/*-*/| CLASS ANTI_CLASSDESCLIST
/*-*/    { None, vaant $2 }
;
%inline class_description:
  CLASS
  ext = ext
  attrs1 = vaval(attributes)
  virt = virtual_flag_vala
  params = vala(formal_class_parameters, ANTI_LIST)
  id = mkrhs(lident_vala)
  COLON
  cty = class_type
  attrs2 = post_item_attributes_vala
    {
      let attrs = append_list_vala attrs1 attrs2 in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      ext,
      Ci.mk id cty ~virt ~params ~attrs ~loc ~docs
    }
;
%inline and_class_description:
  AND
  attrs1 = vaval(attributes)
  virt = virtual_flag_vala
  params = vala(formal_class_parameters, ANTI_LIST)
  id = mkrhs(lident_vala)
  COLON
  cty = class_type
  attrs2 = post_item_attributes_vala
    {
      let attrs = append_list_vala attrs1 attrs2 in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      let text = symbol_text $symbolstartpos in
      Ci.mk id cty ~virt ~params ~attrs ~loc ~text ~docs
    }
;
class_type_declarations:
  xlist(class_type_declaration, and_class_type_declaration)
    { let (a,b) =  $1 in a,vaval b }
/*-*/| CLASS TYPE ANTI_CLASSTYPELIST
/*-*/    { None, vaant $3 }
;
%inline class_type_declaration:
  CLASS TYPE
  ext = ext
  attrs1 = vaval(attributes)
  virt = virtual_flag_vala
  params = vala(formal_class_parameters, ANTI_LIST)
  id = mkrhs(lident_vala)
  EQUAL
  csig = class_signature
  attrs2 = post_item_attributes_vala
    {
      let attrs = append_list_vala attrs1 attrs2 in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      ext,
      Ci.mk id csig ~virt ~params ~attrs ~loc ~docs
    }
;
%inline and_class_type_declaration:
  AND
  attrs1 = vaval(attributes)
  virt = virtual_flag_vala
  params = vala(formal_class_parameters, ANTI_LIST)
  id = mkrhs(lident_vala)
  EQUAL
  csig = class_signature
  attrs2 = post_item_attributes_vala
    {
      let attrs = append_list_vala attrs1 attrs2 in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      let text = symbol_text $symbolstartpos in
      Ci.mk id csig ~virt ~params ~attrs ~loc ~text ~docs
    }
;

/* Core expressions */

seq_expr:
  | expr        %prec below_SEMI  { $1 }
  | expr SEMI                     { $1 }
  | mkexp(expr SEMI seq_expr
    { Pexp_sequence($1, $3) })
    { $1 }
  | expr SEMI PERCENT attr_id seq_expr
    { let seq = mkexp ~loc:$sloc (Pexp_sequence ($1, $5)) in
      let payload = PStr (vaval[mkstrexp ~loc:$sloc (vaval seq) (vaval [])]) in
      mkexp ~loc:$sloc (Pexp_extension ($4, payload)) }
;
labeled_simple_pattern:
    QUESTION LPAREN label_let_pattern vala(opt_default, ANTI_EXPROPT) RPAREN
      { (vaval (Optional (vaval (fst $3))), $4, snd $3) }
  | QUESTION label_var
      { (vaval (Optional (vaval (fst $2))), vaval None, snd $2) }
  | OPTLABEL LPAREN let_pattern vala(opt_default, ANTI_EXPROPT) RPAREN
      { (vaval (Optional (vaval $1)), $4, $3) }
  | OPTLABEL pattern_var
      { (vaval (Optional (vaval $1)), vaval None, $2) }
  | TILDE LPAREN label_let_pattern RPAREN
      { (vaval (Labelled (vaval (fst $3))), vaval None, snd $3) }
  | TILDE label_var
      { (vaval (Labelled (vaval (fst $2))), vaval None, snd $2) }
  | LABEL simple_pattern
      { (vaval (Labelled (vaval $1)), vaval None, $2) }
  | simple_pattern
      { (vaval Nolabel, vaval None, $1) }
/*-*/  | ANTI_LABEL LPAREN simple_pattern vala(opt_default, ANTI_EXPROPT) RPAREN
/*-*/      { (vaant $1, $4, $3) }
;

pattern_var:
  mkpat(
      mkrhs(lident_vala)     { Ppat_var $1 }
    | UNDERSCORE        { Ppat_any }
  ) { $1 }
;

%inline opt_default:
  preceded(EQUAL, seq_expr)?
    { $1 }
;
label_let_pattern:
    x = label_var
      { x }
  | x = label_var COLON cty = core_type
      { let lab, pat = x in
        lab,
        mkpat ~loc:$sloc (Ppat_constraint (pat, cty)) }
;
%inline label_var:
    mkrhs(LIDENT)
      { ($1.Location.txt, mkpat ~loc:$sloc (Ppat_var (loc_map vaval $1))) }
;
let_pattern:
    pattern
      { $1 }
  | mkpat(pattern COLON core_type
      { Ppat_constraint($1, $3) })
      { $1 }
;

%inline indexop_expr(dot, index, right):
  | array=simple_expr d=dot LPAREN i=index RPAREN r=right
    { array, d, Paren,   i, r }
  | array=simple_expr d=dot LBRACE i=index RBRACE r=right
    { array, d, Brace,   i, r }
  | array=simple_expr d=dot LBRACKET i=index RBRACKET r=right
    { array, d, Bracket, i, r }
;

%inline indexop_error(dot, index):
  | simple_expr dot _p=LPAREN index  _e=error
    { indexop_unclosed_error $loc(_p)  Paren $loc(_e) }
  | simple_expr dot _p=LBRACE index  _e=error
    { indexop_unclosed_error $loc(_p) Brace $loc(_e) }
  | simple_expr dot _p=LBRACKET index  _e=error
    { indexop_unclosed_error $loc(_p) Bracket $loc(_e) }
;

%inline qualified_dotop: ioption(DOT mod_longident {vaval $2}) DOTOP { $1, $2 };

/*-*/%inline vaval(X):
/*-*/   X
/*-*/   { vaval $1 }
/*-*/;
/*-*/
/*-*/%inline vaant(X):
/*-*/   X
/*-*/   { vaant $1 }
/*-*/;
/*-*/
/*-*/%inline vala(X,anti):
/*-*/   X
/*-*/     { vaval $1 }
/*-*/  | anti
/*-*/     { vaant $1 }
/*-*/;
/*-*/
expr:
    simple_expr %prec below_HASH
      { $1 }
  | expr_attrs
      { let desc, attrs = $1 in
        mkexp_attrs ~loc:$sloc desc attrs }
  | mkexp(expr_)
      { $1 }
  | let_bindings(ext) IN seq_expr
      { expr_of_let_bindings ~loc:$sloc $1 $3 }
  | pbop_op = mkrhs(vaval(LETOP)) bindings = letop_bindings IN body = seq_expr
      { let (pbop_pat, pbop_exp, rev_ands) = bindings in
        let ands = List.rev rev_ands in
        let pbop_loc = make_loc $sloc in
        let let_ = {pbop_op; pbop_pat; pbop_exp; pbop_loc} in
        mkexp ~loc:$sloc (Pexp_letop{ let_=vaval let_; ands = vaval ands; body}) }
/*-*/  | ANTI_LETOP ANTI_LIST IN body = seq_expr
/*-*/      { mkexp ~loc:$sloc (Pexp_letop{ let_=vaant $1; ands=vaant $2; body}) }
  | expr COLONCOLON expr
      { mkexp_cons ~loc:$sloc $loc($2) (ghexp ~loc:$sloc (Pexp_tuple(vaval[$1;$3]))) }
  | mkrhs(label_vala) LESSMINUS expr
      { mkexp ~loc:$sloc (Pexp_setinstvar($1, $3)) }
  | simple_expr DOT mkrhs(vala(label_longident, ANTI_LONGLID)) LESSMINUS expr
      { mkexp ~loc:$sloc (Pexp_setfield($1, $3, $5)) }
  | indexop_expr(DOT, seq_expr, LESSMINUS v=expr {Some v})
    { mk_indexop_expr builtin_indexing_operators ~loc:$sloc $1 }
  | indexop_expr(qualified_dotop, expr_semi_list, LESSMINUS v=expr {Some v})
    { mk_indexop_expr user_indexing_operators ~loc:$sloc $1 }
  | expr attribute
      { Exp.attr $1 $2 }
/*-*/  | expr ANTI_ALGATTRS
/*-*/      { Exp.attrs $1 (vaant $2) }
/* BEGIN AVOID */
  | UNDERSCORE
     { not_expecting $loc($1) "wildcard \"_\"" }
/* END AVOID */
;
%inline expr_attrs:
  | LET MODULE ext_attributes mkrhs(module_name) module_binding_body IN seq_expr
      { Pexp_letmodule($4, $5, $7), $3 }
  | LET EXCEPTION ext_attributes let_exception_declaration IN seq_expr
      { Pexp_letexception(vaval $4, $6), $3 }
/*-*/  | LET ANTI_EXCON IN seq_expr
/*-*/      { Pexp_letexception(vaant $2, $4), (None, vaval []) }
  | LET OPEN override_flag_vala ext_attributes module_expr IN seq_expr
      { let open_loc = make_loc ($startpos($2), $endpos($5)) in
        let od = Opn.mk $5 ~override:$3 ~loc:open_loc in
        Pexp_open(od, $7), $4 }
  | FUNCTION ext_attributes vala(match_cases, ANTI_CASES)
      { Pexp_function $3, $2 }
  | FUN ext_attributes labeled_simple_pattern fun_def
      { let (l,o,p) = $3 in
        Pexp_fun(l, o, p, $4), $2 }
  | FUN ext_attributes LPAREN TYPE lident_list RPAREN fun_def
      { (mk_newtypes ~loc:$sloc $5 $7).pexp_desc, $2 }
  | MATCH ext_attributes seq_expr WITH vala(match_cases, ANTI_CASES)
      { Pexp_match($3, $5), $2 }
  | TRY ext_attributes seq_expr WITH vala(match_cases, ANTI_CASES)
      { Pexp_try($3, $5), $2 }
  | TRY ext_attributes seq_expr WITH error
      { syntax_error() }
/*-*/  | IF ext_attributes seq_expr THEN expr ANTI_EXPROPT
/*-*/      { Pexp_ifthenelse($3, $5, vaant $6), $2 }
  | IF ext_attributes seq_expr THEN expr ELSE expr
      { Pexp_ifthenelse($3, $5, vaval(Some $7)), $2 }
  | IF ext_attributes seq_expr THEN expr
      { Pexp_ifthenelse($3, $5, vaval None), $2 }
  | WHILE ext_attributes seq_expr DO seq_expr DONE
      { Pexp_while($3, $5), $2 }
  | FOR ext_attributes pattern EQUAL seq_expr direction_flag_vala seq_expr DO
    seq_expr DONE
      { Pexp_for($3, $5, $7, $6, $9), $2 }
  | ASSERT ext_attributes simple_expr %prec below_HASH
      { Pexp_assert $3, $2 }
  | LAZY ext_attributes simple_expr %prec below_HASH
      { Pexp_lazy $3, $2 }
  | OBJECT ext_attributes class_structure END
      { Pexp_object $3, $2 }
  | OBJECT ext_attributes class_structure error
      { unclosed "object" $loc($1) "end" $loc($4) }
;
%inline expr_:
  | simple_expr vala(nonempty_llist(labeled_simple_expr), ANTI_LIST)
      { Pexp_apply($1, Pcaml.vala_map (List.map (fun (a,b) -> (a, b))) $2) }
/*-*/  | vaant(ANTI_TUPLELIST)
/*-*/      { Pexp_tuple $1 }
  | expr_comma_list %prec below_COMMA
      { Pexp_tuple(vaval($1)) }
  | mkrhs(constr_longident_vala) simple_expr %prec below_HASH
      { Pexp_construct($1, vaval(Some $2)) }
/*-*/  | mkrhs(constr_longident_vala) vaant(ANTI_EXPROPT) %prec below_HASH
/*-*/      { Pexp_construct($1, $2) }
  | name_tag_vala simple_expr %prec below_HASH
      { Pexp_variant($1, vaval(Some $2)) }
/*-*/  | name_tag_vala ANTI_EXPROPT %prec below_HASH
/*-*/      { Pexp_variant($1, vaant $2) }
  | e1 = expr op = op(infix_operator) e2 = expr
      { mkinfix e1 op e2 }
  | subtractive expr %prec prec_unary_minus
      { mkuminus ~oploc:$loc($1) $1 $2 }
  | additive expr %prec prec_unary_plus
      { mkuplus ~oploc:$loc($1) $1 $2 }
;

simple_expr:
  | LPAREN seq_expr RPAREN
      { reloc_exp ~loc:$sloc $2 }
  | LPAREN seq_expr error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN seq_expr type_constraint RPAREN
      { mkexp_constraint ~loc:$sloc $2 $3 }
/*-*/  | LPAREN e = seq_expr ty1opt = ANTI_CTYPOPT COLONGREATER ty2 = core_type RPAREN
/*-*/      { mkexp ~loc:$sloc (Pexp_coerce(e, vaant ty1opt, ty2)) }
  | indexop_expr(DOT, seq_expr, { None })
      { mk_indexop_expr builtin_indexing_operators ~loc:$sloc $1 }
  | indexop_expr(qualified_dotop, expr_semi_list, { None })
      { mk_indexop_expr user_indexing_operators ~loc:$sloc $1 }
  | indexop_error (DOT, seq_expr) { $1 }
  | indexop_error (qualified_dotop, expr_semi_list) { $1 }
  | simple_expr_attrs
    { let desc, attrs = $1 in
      mkexp_attrs ~loc:$sloc desc attrs }
  | mkexp(simple_expr_)
      { $1 }
;
%inline simple_expr_attrs:
  | BEGIN ext = ext attrs = vaval(attributes) e = seq_expr END
      { e.pexp_desc, (ext, append_list_vala attrs e.pexp_attributes) }
  | BEGIN ext_attributes END
      { Pexp_construct (mkloc (vaval (Lident (vaval"()"))) (make_loc $sloc), vaval None), $2 }
  | BEGIN ext_attributes seq_expr error
      { unclosed "begin" $loc($1) "end" $loc($4) }
  | NEW ext_attributes mkrhs(vaval(class_longident))
      { Pexp_new($3), $2 }
  | LPAREN MODULE ext_attributes module_expr RPAREN
      { Pexp_pack $4, $3 }
  | LPAREN MODULE ext_attributes module_expr COLON package_type RPAREN
      { Pexp_constraint (ghexp ~loc:$sloc (Pexp_pack $4), $6), $3 }
  | LPAREN MODULE ext_attributes module_expr COLON error
      { unclosed "(" $loc($1) ")" $loc($6) }
;
%inline simple_expr_:
  | mkrhs(vala(val_longident, ANTI_LONGLID))
      { Pexp_ident ($1) }
/*-*/  | xtr_antis { Pexp_xtr (Location.mkloc $1 (make_loc $sloc)) }
  | vala(constant, ANTI_CONSTANT)
      { Pexp_constant $1 }
  | mkrhs(constr_longident_vala) %prec prec_constant_constructor
      { Pexp_construct($1, vaval None) }
  | name_tag_vala %prec prec_constant_constructor
      { Pexp_variant($1, vaval None) }
  | op(PREFIXOP) simple_expr
      { Pexp_apply($1, vaval[Nolabel,$2]) }
  | op(BANG {"!"}) simple_expr
      { Pexp_apply($1, vaval[Nolabel,$2]) }
  | LBRACELESS object_expr_content GREATERRBRACE
      { Pexp_override $2 }
  | LBRACELESS object_expr_content error
      { unclosed "{<" $loc($1) ">}" $loc($3) }
  | LBRACELESS GREATERRBRACE
      { Pexp_override (vaval []) }
  | simple_expr DOT mkrhs(vala(label_longident, ANTI_LONGLID))
      { Pexp_field($1, $3) }
  | od=open_dot_declaration DOT LPAREN seq_expr RPAREN
      { Pexp_open(od, $4) }
  | od=open_dot_declaration DOT LBRACELESS object_expr_content GREATERRBRACE
      { (* TODO: review the location of Pexp_override *)
        Pexp_open(od, mkexp ~loc:$sloc (Pexp_override $4)) }
  | mod_longident DOT LBRACELESS object_expr_content error
      { unclosed "{<" $loc($3) ">}" $loc($5) }
  | simple_expr HASH mkrhs(label_vala)
      { Pexp_send($1, $3) }
  | simple_expr op(HASHOP) simple_expr
      { mkinfix $1 $2 $3 }
  | extension
      { Pexp_extension $1 }
  | od=open_dot_declaration DOT mkrhs(LPAREN RPAREN {vaval (Lident (vaval "()"))})
      { Pexp_open(od, mkexp ~loc:($loc($3)) (Pexp_construct($3, vaval None))) }
  | mod_longident DOT LPAREN seq_expr error
      { unclosed "(" $loc($3) ")" $loc($5) }
  | LBRACE record_expr_content RBRACE
      { let (exten, fields) = $2 in
        Pexp_record(fields, exten) }
  | LBRACE record_expr_content error
      { unclosed "{" $loc($1) "}" $loc($3) }
  | od=open_dot_declaration DOT LBRACE record_expr_content RBRACE
      { let (exten, fields) = $4 in
        Pexp_open(od, mkexp ~loc:($startpos($3), $endpos)
                        (Pexp_record(fields, exten))) }
  | mod_longident DOT LBRACE record_expr_content error
      { unclosed "{" $loc($3) "}" $loc($5) }
  | LBRACKETBAR vala(expr_semi_list, ANTI_LIST) BARRBRACKET
      { Pexp_array($2) }
  | LBRACKETBAR expr_semi_list error
      { unclosed "[|" $loc($1) "|]" $loc($3) }
  | LBRACKETBAR BARRBRACKET
      { Pexp_array (vaval []) }
  | od=open_dot_declaration DOT LBRACKETBAR expr_semi_list BARRBRACKET
      { Pexp_open(od, mkexp ~loc:($startpos($3), $endpos) (Pexp_array(vaval $4))) }
  | od=open_dot_declaration DOT LBRACKETBAR BARRBRACKET
      { (* TODO: review the location of Pexp_array *)
        Pexp_open(od, mkexp ~loc:($startpos($3), $endpos) (Pexp_array (vaval []))) }
  | mod_longident DOT
    LBRACKETBAR expr_semi_list error
      { unclosed "[|" $loc($3) "|]" $loc($5) }
  | LBRACKET expr_semi_list RBRACKET
      { fst (mktailexp $loc($3) $2) }
  | LBRACKET expr_semi_list error
      { unclosed "[" $loc($1) "]" $loc($3) }
  | od=open_dot_declaration DOT LBRACKET expr_semi_list RBRACKET
      { let list_exp =
          (* TODO: review the location of list_exp *)
          let tail_exp, _tail_loc = mktailexp $loc($5) $4 in
          mkexp ~loc:($startpos($3), $endpos) tail_exp in
        Pexp_open(od, list_exp) }
  | od=open_dot_declaration DOT mkrhs(LBRACKET RBRACKET {vaval (Lident (vaval "[]"))})
      { Pexp_open(od, mkexp ~loc:$loc($3) (Pexp_construct($3, vaval None))) }
  | mod_longident DOT
    LBRACKET expr_semi_list error
      { unclosed "[" $loc($3) "]" $loc($5) }
  | od=open_dot_declaration DOT LPAREN MODULE ext_attributes module_expr COLON
    package_type RPAREN
      { let modexp =
          mkexp_attrs ~loc:($startpos($3), $endpos)
            (Pexp_constraint (ghexp ~loc:$sloc (Pexp_pack $6), $8)) $5 in
        Pexp_open(od, modexp) }
  | mod_longident DOT
    LPAREN MODULE ext_attributes module_expr COLON error
      { unclosed "(" $loc($3) ")" $loc($8) }
;
labeled_simple_expr:
    simple_expr %prec below_HASH
      { (Nolabel, $1) }
  | LABEL simple_expr %prec below_HASH
      { (Labelled (vaval $1), $2) }
  | TILDE label = LIDENT
      { let loc = $loc(label) in
        (Labelled (vaval label), mkexpvar ~loc (vaval label)) }
  | QUESTION label = LIDENT
      { let loc = $loc(label) in
        (Optional (vaval label), mkexpvar ~loc (vaval label)) }
  | OPTLABEL simple_expr %prec below_HASH
      { (Optional (vaval $1), $2) }
;
%inline lident_list:
  xs = mkrhs(lident_vala)+
    { xs }
;
%inline let_ident:
    val_ident_vala { mkpatvar ~loc:$sloc $1 }
;
let_binding_body_no_punning:
    let_ident strict_binding
      { ($1, $2) }
  | let_ident type_constraint EQUAL seq_expr
      { let v = $1 in (* PR#7344 *)
        let t =
          match $2 with
            Some t, None -> t
          | _, Some t -> t
          | _ -> assert false
        in
        let loc = Location.(t.ptyp_loc.loc_start, t.ptyp_loc.loc_end) in
        let typ = ghtyp ~loc (Ptyp_poly(vaval [],t)) in
        let patloc = ($startpos($1), $endpos($2)) in
        (ghpat ~loc:patloc (Ppat_constraint(v, typ)),
         mkexp_constraint ~loc:$sloc $4 $2) }
  | let_ident COLON vala(typevar_list, ANTI_LIST) DOT core_type EQUAL seq_expr
      (* TODO: could replace [typevar_list DOT core_type]
               with [mktyp(poly(core_type))]
               and simplify the semantic action? *)
      { let typloc = ($startpos($3), $endpos($5)) in
        let patloc = ($startpos($1), $endpos($5)) in
        (ghpat ~loc:patloc
           (Ppat_constraint($1, ghtyp ~loc:typloc (Ptyp_poly($3,$5)))),
         $7) }
  | let_ident COLON TYPE lident_list DOT core_type EQUAL seq_expr
      { let exp, poly =
          wrap_type_annotation ~loc:$sloc $4 $6 $8 in
        let loc = ($startpos($1), $endpos($6)) in
        (ghpat ~loc (Ppat_constraint($1, poly)), exp) }
  | pattern_no_exn EQUAL seq_expr
      { ($1, $3) }
  | simple_pattern_not_ident COLON core_type EQUAL seq_expr
      { let loc = ($startpos($1), $endpos($3)) in
        (ghpat ~loc (Ppat_constraint($1, $3)), $5) }
;
let_binding_body:
  | let_binding_body_no_punning
      { let p,e = $1 in (p,e,false) }
/* BEGIN AVOID */
  | val_ident_vala %prec below_HASH
      { (mkpatvar ~loc:$loc $1, mkexpvar ~loc:$loc $1, true) }
  (* The production that allows puns is marked so that [make list-parse-errors]
     does not attempt to exploit it. That would be problematic because it
     would then generate bindings such as [let x], which are rejected by the
     auxiliary function [addlb] via a call to [syntax_error]. *)
/* END AVOID */
;
(* The formal parameter EXT can be instantiated with ext or no_ext
   so as to indicate whether an extension is allowed or disallowed. *)
/*-*/value_binding:
/*-*/  body = let_binding_body
/*-*/  attrs2 = post_item_attributes_vala
/*-*/ { vb_of_lb (mklb ~loc:$sloc true body attrs2) }
/*-*/;
let_bindings(EXT):
    let_binding(EXT)                            { $1 }
/*-*/  | LET
/*-*/    ext = EXT
/*-*/    attrs1 = vaval(attributes)
/*-*/    rec_flag = rec_flag_vala
/*-*/    list = vaant(ANTI_LIST)
/*-*/    {
(*-*)      match (ext, unvala attrs1) with
(*-*)        (None, []) ->
(*-*)        { lbs_bindings = list ; lbs_rec = rec_flag ; lbs_extension = None }
(*-*)      | _ -> syntax_error()
(*-*)    }
  | let_bindings(EXT) and_let_binding           { addlb $1 $2 }
;
%inline let_binding(EXT):
  LET
  ext = EXT
  attrs1 = vaval(attributes)
  rec_flag = rec_flag_vala
  body = let_binding_body
  attrs2 = post_item_attributes_vala
    {
      let attrs = append_list_vala attrs1 attrs2 in
      mklbs ext rec_flag (mklb ~loc:$sloc true body attrs)
    }
;
and_let_binding:
  AND
  attrs1 = vaval(attributes)
  body = let_binding_body
  attrs2 = post_item_attributes_vala
    {
      let attrs = append_list_vala attrs1 attrs2 in
      mklb ~loc:$sloc false body attrs
    }
;
letop_binding_body:
    pat = let_ident exp = strict_binding
      { (pat, exp) }
  | val_ident_vala
      (* Let-punning *)
      { (mkpatvar ~loc:$loc $1, mkexpvar ~loc:$loc $1) }
  | pat = simple_pattern COLON typ = core_type EQUAL exp = seq_expr
      { let loc = ($startpos(pat), $endpos(typ)) in
        (ghpat ~loc (Ppat_constraint(pat, typ)), exp) }
  | pat = pattern_no_exn EQUAL exp = seq_expr
      { (pat, exp) }
;
letop_bindings:
    body = letop_binding_body
      { let let_pat, let_exp = body in
        let_pat, let_exp, [] }
  | bindings = letop_bindings pbop_op = mkrhs(vaval(ANDOP)) body = letop_binding_body
      { let let_pat, let_exp, rev_ands = bindings in
        let pbop_pat, pbop_exp = body in
        let pbop_loc = make_loc $sloc in
        let and_ = {pbop_op; pbop_pat; pbop_exp; pbop_loc} in
        let_pat, let_exp, and_ :: rev_ands }
;
fun_binding:
    strict_binding
      { $1 }
  | type_constraint EQUAL seq_expr
      { mkexp_constraint ~loc:$sloc $3 $1 }
;
strict_binding:
    EQUAL seq_expr
      { $2 }
  | labeled_simple_pattern fun_binding
      { let (l, o, p) = $1 in ghexp ~loc:$sloc (Pexp_fun(l, o, p, $2)) }
  | LPAREN TYPE lident_list RPAREN fun_binding
      { mk_newtypes ~loc:$sloc $3 $5 }
;
%inline match_cases:
  xs = preceded_or_separated_nonempty_llist(BAR, match_case)
    { xs }
;
match_case:
/*-*/    vaval(pattern) vaant(ANTI_WHENO) MINUSGREATER vaval(seq_expr)
/*-*/      { Exp.case $1 ~guard:$2 $4 }
  | vaval(pattern) MINUSGREATER vaval(seq_expr)
      { Exp.case $1 ~guard:(vaval None) $3 }
  | vaval(pattern) WHEN seq_expr MINUSGREATER vaval(seq_expr)
      { Exp.case $1 ~guard:(vaval (Some (vaval $3))) $5 }
  | vaval(pattern) MINUSGREATER DOT
      { Exp.case $1 ~guard:(vaval None) (vaval (Exp.unreachable ~loc:(make_loc $loc($3)) ())) }
;
fun_def:
    MINUSGREATER seq_expr
      { $2 }
  | mkexp(COLON atomic_type MINUSGREATER seq_expr
      { Pexp_constraint ($4, $2) })
      { $1 }
/* Cf #5939: we used to accept (fun p when e0 -> e) */
  | labeled_simple_pattern fun_def
      {
       let (l,o,p) = $1 in
       ghexp ~loc:$sloc (Pexp_fun(l, o, p, $2))
      }
  | LPAREN TYPE lident_list RPAREN fun_def
      { mk_newtypes ~loc:$sloc $3 $5 }
;
%inline expr_comma_list:
  es = separated_nontrivial_llist(COMMA, expr)
    { es }
;
record_expr_content:
  eo = vala(ioption(terminated(simple_expr, WITH)), ANTI_WITHE)
  fields = vala(separated_or_terminated_nonempty_list(SEMI, record_expr_field), ANTI_LIST)
    { eo, fields }
;
%inline record_expr_field:
  | label = mkrhs(vala(label_longident, ANTI_LONGID))
    c = type_constraint?
    eo = preceded(EQUAL, expr)?
      { let e =
          match eo with
          | None ->
              (* No pattern; this is a pun. Desugar it. *)
              exp_of_longident ~loc:$sloc (loc_map unvala label)
          | Some e ->
              e
        in
        label, mkexp_opt_constraint ~loc:$sloc e c }
;
%inline object_expr_content:
  xs = vala(separated_or_terminated_nonempty_list(SEMI, object_expr_field), ANTI_LIST)
    { xs }
;
%inline object_expr_field:
    label = mkrhs(label_vala)
    oe = preceded(EQUAL, expr)?
      { let e =
          match oe with
          | None ->
              (* No expression; this is a pun. Desugar it. *)
              exp_of_label ~loc:$sloc (loc_map unvala label)
          | Some e ->
              e
        in
        label, e }
;
%inline expr_semi_list:
  es = separated_or_terminated_nonempty_list(SEMI, expr)
    { es }
;
type_constraint:
    COLON core_type                             { (Some $2, None) }
  | COLON core_type COLONGREATER core_type      { (Some $2, Some $4) }
  | COLONGREATER core_type                      { (None, Some $2) }
  | COLON error                                 { syntax_error() }
  | COLONGREATER error                          { syntax_error() }
;

/* Patterns */

(* Whereas [pattern] is an arbitrary pattern, [pattern_no_exn] is a pattern
   that does not begin with the [EXCEPTION] keyword. Thus, [pattern_no_exn]
   is the intersection of the context-free language [pattern] with the
   regular language [^EXCEPTION .*].

   Ideally, we would like to use [pattern] everywhere and check in a later
   phase that EXCEPTION patterns are used only where they are allowed (there
   is code in typing/typecore.ml to this end). Unfortunately, in the
   definition of [let_binding_body], we cannot allow [pattern]. That would
   create a shift/reduce conflict: upon seeing LET EXCEPTION ..., the parser
   wouldn't know whether this is the beginning of a LET EXCEPTION construct or
   the beginning of a LET construct whose pattern happens to begin with
   EXCEPTION. The conflict is avoided there by using [pattern_no_exn] in the
   definition of [let_binding_body].

   In order to avoid duplication between the definitions of [pattern] and
   [pattern_no_exn], we create a parameterized definition [pattern_(self)]
   and instantiate it twice. *)

pattern:
    pattern_(pattern)
      { $1 }
  | EXCEPTION ext_attributes pattern %prec prec_constr_appl
      { mkpat_attrs ~loc:$sloc (Ppat_exception $3) $2}
;

pattern_no_exn:
    pattern_(pattern_no_exn)
      { $1 }
;

%inline pattern_(self):
  | self COLONCOLON pattern
      { mkpat_cons ~loc:$sloc $loc($2) (ghpat ~loc:$sloc (Ppat_tuple (vaval [$1;$3]))) }
  | self attribute
      { Pat.attr $1 $2 }
/*-*/  | self ANTI_ALGATTRS
/*-*/      { Pat.attrs $1 (vaant $2) }
  | pattern_gen
      { $1 }
  | mkpat(
      self AS mkrhs(val_ident_vala)
        { Ppat_alias($1, $3) }
    | self AS error
        { expecting $loc($3) "identifier" }
    | pattern_comma_list(self) %prec below_COMMA
        { Ppat_tuple(vaval (List.rev $1)) }
/*-*/  | ANTI_TUPLELIST
/*-*/      { Ppat_tuple (vaant $1) }
    | self COLONCOLON error
        { expecting $loc($3) "pattern" }
    | self BAR pattern
        { Ppat_or($1, $3) }
    | self BAR error
        { expecting $loc($3) "pattern" }
  ) { $1 }
;

pattern_gen:
    simple_pattern
      { $1 }
  | mkpat(
      mkrhs(constr_longident_vala) pattern %prec prec_constr_appl
        { Ppat_construct($1, vaval(Some (vaval [], $2))) }
    | constr=mkrhs(constr_longident_vala) LPAREN TYPE newtypes=vala(lident_list, ANTI_LIST) RPAREN
        pat=simple_pattern
        { Ppat_construct(constr, vaval(Some (newtypes, pat))) }
/*-*/    | constr=mkrhs(constr_longident_vala) pattopt = ANTI_PATTOPT
/*-*/        { Ppat_construct(constr, vaant pattopt) }
    | name_tag_vala pattern %prec prec_constr_appl
        { Ppat_variant($1, vaval(Some $2)) }
/*-*/    | name_tag_vala pattopt = ANTI_PATTOPT
/*-*/        { Ppat_variant($1, vaant pattopt) }
    ) { $1 }
  | LAZY ext_attributes simple_pattern
      { mkpat_attrs ~loc:$sloc (Ppat_lazy $3) $2}
;
simple_pattern:
    mkpat(mkrhs(val_ident_vala) %prec below_EQUAL
      { Ppat_var ($1) })
      { $1 }
  | simple_pattern_not_ident { $1 }
;

simple_pattern_not_ident:
  | LPAREN pattern RPAREN
      { reloc_pat ~loc:$sloc $2 }
  | simple_delimited_pattern
      { $1 }
  | LPAREN MODULE ext_attributes mkrhs(module_name) RPAREN
      { mkpat_attrs ~loc:$sloc (Ppat_unpack $4) $3 }
  | LPAREN MODULE ext_attributes mkrhs(module_name) COLON package_type RPAREN
      { mkpat_attrs ~loc:$sloc
          (Ppat_constraint(mkpat ~loc:$loc($4) (Ppat_unpack $4), $6))
          $3 }
  | mkpat(simple_pattern_not_ident_)
      { $1 }
;
%inline simple_pattern_not_ident_:
/*-*/  | xtr_antis { Ppat_xtr (Location.mkloc $1 (make_loc $sloc)) }
  | UNDERSCORE
      { Ppat_any }
  | vala(signed_constant, ANTI_CONSTANT)
      { Ppat_constant $1 }
  | vala(signed_constant, ANTI_CONSTANT) DOTDOT
      vala(signed_constant, ANTI_CONSTANT)
      { Ppat_interval ($1, $3) }
  | mkrhs(constr_longident_vala)
      { Ppat_construct($1, vaval None) }
  | name_tag_vala
      { Ppat_variant($1, vaval None) }
  | HASH mkrhs(vala(type_longident, ANTI_LONGLID))
      { Ppat_type ($2) }
  | mkrhs(mod_longident_vala) DOT simple_delimited_pattern
      { Ppat_open($1, $3) }
/*-*/  | mkrhs(mod_longident_vala) DOT xtr_antis
/*-*/      { Ppat_open($1, mkpat ~loc:$sloc (Ppat_xtr (Location.mkloc $3 (make_loc $sloc)))) }
  | mkrhs(mod_longident_vala) DOT mkrhs(LBRACKET RBRACKET {vaval(Lident (vaval "[]"))})
    { Ppat_open($1, mkpat ~loc:$sloc (Ppat_construct($3, vaval None))) }
  | mkrhs(mod_longident_vala) DOT mkrhs(LPAREN RPAREN {vaval(Lident (vaval "()"))})
    { Ppat_open($1, mkpat ~loc:$sloc (Ppat_construct($3, vaval None))) }
  | mkrhs(mod_longident_vala) DOT LPAREN pattern RPAREN
      { Ppat_open ($1, $4) }
  | mod_longident DOT LPAREN pattern error
      { unclosed "(" $loc($3) ")" $loc($5)  }
  | mod_longident DOT LPAREN error
      { expecting $loc($4) "pattern" }
  | LPAREN pattern error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN pattern COLON core_type RPAREN
      { Ppat_constraint($2, $4) }
  | LPAREN pattern COLON core_type error
      { unclosed "(" $loc($1) ")" $loc($5) }
  | LPAREN pattern COLON error
      { expecting $loc($4) "type" }
  | LPAREN MODULE ext_attributes module_name COLON package_type
    error
      { unclosed "(" $loc($1) ")" $loc($7) }
  | extension
      { Ppat_extension $1 }
;

simple_delimited_pattern:
  mkpat(
      LBRACE record_pat_content RBRACE
      { let (fields, closed) = $2 in
        Ppat_record(fields, closed) }
    | LBRACE record_pat_content error
      { unclosed "{" $loc($1) "}" $loc($3) }
    | LBRACKET pattern_semi_list RBRACKET
      { fst (mktailpat $loc($3) $2) }
    | LBRACKET pattern_semi_list error
      { unclosed "[" $loc($1) "]" $loc($3) }
    | LBRACKETBAR vala(pattern_semi_list, ANTI_LIST) BARRBRACKET
      { Ppat_array $2 }
    | LBRACKETBAR BARRBRACKET
      { Ppat_array (vaval []) }
    | LBRACKETBAR pattern_semi_list error
      { unclosed "[|" $loc($1) "|]" $loc($3) }
  ) { $1 }

pattern_comma_list(self):
    pattern_comma_list(self) COMMA pattern      { $3 :: $1 }
  | self COMMA pattern                          { [$3; $1] }
  | self COMMA error                            { expecting $loc($3) "pattern" }
;
%inline pattern_semi_list:
  ps = separated_or_terminated_nonempty_list(SEMI, pattern)
    { ps }
;
(* A label-pattern list is a nonempty list of label-pattern pairs, optionally
   followed with an UNDERSCORE, separated-or-terminated with semicolons. *)
%inline record_pat_content:
  listx(SEMI, record_pat_field, UNDERSCORE)
    { let fields, closed = $1 in
      let closed = match closed with Some () -> Open | None -> Closed in
      vaval(fields), vaval(closed) }
/*-*/| ANTI_LIST vala(UNDERSCORE?, ANTI_CLOSEDFLAG)
/*-*/    {
(*-*)      let closed = Pcaml.vala_map (function Some () -> Open | None -> Closed) $2 in
(*-*)      vaant $1, closed
(*-*)    }
;
%inline record_pat_field:
  label = mkrhs(vala(label_longident, ANTI_LONGLID))
  octy = preceded(COLON, core_type)?
  opat = preceded(EQUAL, pattern)?
    { let label, pat =
        match opat with
        | None ->
            (* No pattern; this is a pun. Desugar it.
               But that the pattern was there and the label reconstructed (which
               piece of AST is marked as ghost is important for warning
               emission). *)
            make_ghost label, pat_of_label (loc_map unvala label)
        | Some pat ->
            label, pat
      in
      label, mkpat_opt_constraint ~loc:$sloc pat octy
    }
;

/* Value descriptions */

value_description:
  VAL
  ext = ext
  attrs1 = vaval(attributes)
  id = mkrhs(val_ident_vala)
  COLON
  ty = core_type
  attrs2 = post_item_attributes_vala
    { let attrs = append_list_vala attrs1 attrs2 in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      Val.mk id ty ~attrs ~loc ~docs,
      ext }
;

/* Primitive declarations */

primitive_declaration:
  EXTERNAL
  ext = ext
  attrs1 = vaval(attributes)
  id = mkrhs(val_ident_vala)
  COLON
  ty = core_type
  EQUAL
  prim = vala(raw_string+, ANTI_LIST)
  attrs2 = post_item_attributes_vala
    { let attrs = append_list_vala attrs1 attrs2 in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      Val.mk id ty ~prim ~attrs ~loc ~docs,
      ext }
;

(* Type declarations and type substitutions. *)

(* Type declarations [type t = u] and type substitutions [type t := u] are very
   similar, so we view them as instances of [generic_type_declarations]. In the
   case of a type declaration, the use of [nonrec_flag] means that [NONREC] may
   be absent or present, whereas in the case of a type substitution, the use of
   [no_nonrec_flag] means that [NONREC] must be absent. The use of [type_kind]
   versus [type_subst_kind] means that in the first case, we expect an [EQUAL]
   sign, whereas in the second case, we expect [COLONEQUAL]. *)

%inline type_declarations:
  generic_type_declarations(nonrec_flag_vala, type_kind)
    { $1 }
;

%inline type_subst_declarations:
  generic_type_declarations(no_nonrec_flag, type_subst_kind)
    { $1 }
;

(* A set of type declarations or substitutions begins with a
   [generic_type_declaration] and continues with a possibly empty list of
   [generic_and_type_declaration]s. *)

%inline generic_type_declarations(flag, kind):
  xlist_vala(
    generic_type_declaration(flag, kind),
    generic_and_type_declaration(kind)
  )
  { $1 }
;

/*-*/%inline core_type_declaration(kind):
/*-*/  params = vala(type_parameters, ANTI_LIST)
/*-*/  id = mkrhs(lident_vala)
/*-*/  kind_priv_manifest = kind
/*-*/  cstrs = vala(constraints, ANTI_LIST)
/*-*/  attrs2 = post_item_attributes_vala
/*-*/    {
(*-*)      let (kind, priv, manifest) = kind_priv_manifest in
(*-*)      let docs = symbol_docs $sloc in
(*-*)      let attrs = attrs2 in
(*-*)      let loc = make_loc $sloc in
(*-*)      Type.mk id ~params ~cstrs ~kind ~priv ~manifest ~attrs ~loc ~docs
(*-*)    }
/*-*/;
/*-*/
/*-*/
(* [generic_type_declaration] and [generic_and_type_declaration] look similar,
   but are in reality different enough that it is difficult to share anything
   between them. *)

%inline generic_type_declaration(flag, kind):
  TYPE
  ext = ext
  attrs1 = vaval(attributes)
  flag = flag
  params = vala(type_parameters, ANTI_LIST)
  id = mkrhs(lident_vala)
  kind_priv_manifest = kind
  cstrs = vaval(constraints)
  attrs2 = post_item_attributes_vala
    {
      let (kind, priv, manifest) = kind_priv_manifest in
      let docs = symbol_docs $sloc in
      let attrs = append_list_vala attrs1 attrs2 in
      let loc = make_loc $sloc in
      (flag, ext),
      Type.mk id ~params ~cstrs ~kind ~priv ~manifest ~attrs ~loc ~docs
    }
;
%inline generic_and_type_declaration(kind):
  AND
  attrs1 = vaval(attributes)
  params = vala(type_parameters, ANTI_LIST)
  id = mkrhs(lident_vala)
  kind_priv_manifest = kind
  cstrs = vaval(constraints)
  attrs2 = post_item_attributes_vala
    {
      let (kind, priv, manifest) = kind_priv_manifest in
      let docs = symbol_docs $sloc in
      let attrs = append_list_vala attrs1 attrs2 in
      let loc = make_loc $sloc in
      let text = symbol_text $symbolstartpos in
      Type.mk id ~params ~cstrs ~kind ~priv ~manifest ~attrs ~loc ~docs ~text
    }
;
%inline constraints:
  llist(preceded(CONSTRAINT, constrain))
    { $1 }
;
(* Lots of %inline expansion are required for [nonempty_type_kind] to be
   LR(1). At the cost of some manual expansion, it would be possible to give a
   definition that leads to a smaller grammar (after expansion) and therefore
   a smaller automaton. *)
nonempty_type_kind:
  | priv = inline_private_flag_vala
    ty = vaval(core_type)
      { (Ptype_abstract, priv, vaval (Some ty)) }
  | oty = type_synonym
    priv = inline_private_flag_vala
    cs = vala(constructor_declarations, ANTI_CONSTRUCTORLIST)
      { (Ptype_variant cs, priv, oty) }
  | oty = type_synonym
    priv = inline_private_flag_vala
    DOTDOT
      { (Ptype_open, priv, oty) }
  | oty = type_synonym
    priv = inline_private_flag_vala
    LBRACE ls = vala(label_declarations, ANTI_LIST) RBRACE
      { (Ptype_record ls, priv, oty) }
;
%inline type_synonym:
  ioption(terminated(vaval(core_type), EQUAL))
    { vaval $1 }
/*-*/| ANTI_OPT
/*-*/    { vaant $1 }
;
type_kind:
    /*empty*/
      { (Ptype_abstract, vaval Public, vaval None) }
  | EQUAL nonempty_type_kind
      { $2 }
;
%inline type_subst_kind:
    COLONEQUAL nonempty_type_kind
      { $2 }
;
type_parameters:
    /* empty */
      { [] }
  | p = type_parameter
      { [p] }
  | LPAREN ps = separated_nonempty_llist(COMMA, type_parameter) RPAREN
      { ps }
;
type_parameter:
    type_variance type_variable        { $2, $1 }
;
type_variable:
  mktyp(
    QUOTE tyvar = ident_vala
      { Ptyp_var tyvar }
  | UNDERSCORE
      { Ptyp_any }
  ) { $1 }
;

type_variance:
    /* empty */                             { NoVariance, NoInjectivity }
  | PLUS                                    { Covariant, NoInjectivity }
  | MINUS                                   { Contravariant, NoInjectivity }
  | BANG                                    { NoVariance, Injective }
  | PLUS BANG | BANG PLUS                   { Covariant, Injective }
  | MINUS BANG | BANG MINUS                 { Contravariant, Injective }
  | INFIXOP2
      { if $1 = "+!" then Covariant, Injective else
        if $1 = "-!" then Contravariant, Injective else
        expecting $loc($1) "type_variance" }
  | PREFIXOP
      { if $1 = "!+" then Covariant, Injective else
        if $1 = "!-" then Contravariant, Injective else
        expecting $loc($1) "type_variance" }
;

(* A sequence of constructor declarations is either a single BAR, which
   means that the list is empty, or a nonempty BAR-separated list of
   declarations, with an optional leading BAR. *)
constructor_declarations:
  | BAR
      { [] }
  | cs = bar_llist(constructor_declaration)
      { cs }
;
(* A constructor declaration begins with an opening symbol, which can
   be either epsilon or BAR. Note that this opening symbol is included
   in the footprint $sloc. *)
(* Because [constructor_declaration] and [extension_constructor_declaration]
   are identical except for their semantic actions, we introduce the symbol
   [generic_constructor_declaration], whose semantic action is neutral -- it
   merely returns a tuple. *)
generic_constructor_declaration(opening):
  opening
  cid = mkrhs(constr_ident_vala)
  args_res = generalized_constructor_arguments
  attrs = attributes_vala
    {
      let args, res = args_res in
      let info = symbol_info $endpos in
      let loc = make_loc $sloc in
      cid, args, res, attrs, loc, info
    }
;
%inline constructor_declaration(opening):
  d = generic_constructor_declaration(opening)
    {
      let cid, args, res, attrs, loc, info = d in
      Type.constructor cid ~args ~res ~attrs ~loc ~info
    }
;
str_exception_declaration:
  sig_exception_declaration
    { $1 }
| EXCEPTION
  ext = ext
  attrs1 = vaval(attributes)
  id = mkrhs(constr_ident_vala)
  EQUAL
  lid = mkrhs(constr_longident_vala)
  attrs2 = attributes_vala
  attrs = post_item_attributes_vala
  { let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Te.mk_exception ~attrs
      (Te.rebind id lid ~attrs:( append_list_vala attrs1 attrs2) ~loc ~docs)
    , ext }
;
sig_exception_declaration:
  EXCEPTION
  ext = ext
  attrs1 = vaval(attributes)
  id = mkrhs(constr_ident_vala)
  args_res = generalized_constructor_arguments
  attrs2 = attributes_vala
  attrs = post_item_attributes_vala
    { let args, res = args_res in
      let loc = make_loc ($startpos, $endpos(attrs2)) in
      let docs = symbol_docs $sloc in
      Te.mk_exception ~attrs
        (Te.decl id ~args ~res ~attrs:(append_list_vala attrs1 attrs2) ~loc ~docs)
      , ext }
;
%inline let_exception_declaration:
    mkrhs(constr_ident_vala) generalized_constructor_arguments attributes_vala
      { let args, res = $2 in
        Te.decl $1 ~args ~res ~attrs:$3 ~loc:(make_loc $sloc) }
;
generalized_constructor_arguments:
    /*empty*/                     { (Pcstr_tuple (Ploc.VaVal []),vaval None) }
  | OF constructor_arguments      { ($2,vaval None) }
  | COLON constructor_arguments MINUSGREATER atomic_type %prec below_HASH
                                  { ($2,vaval (Some $4)) }
/*-*/  | COLON constructor_arguments ANTI_OPT
/*-*/     %prec below_HASH
/*-*/                                  { ($2,vaant $3) }
  | COLON atomic_type %prec below_HASH
                                  { (Pcstr_tuple (Ploc.VaVal []),vaval (Some $2)) }
;

constructor_arguments:
  | tys = vala(inline_separated_nonempty_llist(STAR, atomic_type), ANTI_LIST)
    %prec below_HASH
      { Pcstr_tuple tys }
  | LBRACE vala(label_declarations, ANTI_LIST) RBRACE
      { Pcstr_record $2 }
;
label_declarations:
    label_declaration                           { [$1] }
  | label_declaration_semi                      { [$1] }
  | label_declaration_semi label_declarations   { $1 :: $2 }
;
label_declaration:
    mutable_flag_vala mkrhs(label_vala) COLON vala(poly_type_no_attr, ANTI_TYP) attributes_vala
      { let info = symbol_info $endpos in
        Type.field $2 $4 ~mut:$1 ~attrs:$5 ~loc:(make_loc $sloc) ~info }
;
label_declaration_semi:
    mutable_flag_vala mkrhs(label_vala) COLON vala(poly_type_no_attr, ANTI_TYP) vaval(attributes) SEMI vaval(attributes)
      { let info =
          match rhs_info $endpos($5) with
          | Some _ as info_before_semi -> info_before_semi
          | None -> symbol_info $endpos
       in
       Type.field $2 $4 ~mut:$1 ~attrs:(append_list_vala $5 $7) ~loc:(make_loc $sloc) ~info }
;

/* Type Extensions */

%inline str_type_extension:
  type_extension(extension_constructor)
    { $1 }
;
%inline sig_type_extension:
  type_extension(extension_constructor_declaration)
    { $1 }
;
%inline type_extension(declaration):
  TYPE
  ext = ext
  attrs1 = vaval(attributes)
  no_nonrec_flag
  params = vala(type_parameters, ANTI_LIST)
  tid = mkrhs(vala(type_longident, ANTI_LONGLID))
  PLUSEQ
  priv = private_flag_vala
  cs = vala(bar_llist(declaration), ANTI_LIST)
  attrs2 = post_item_attributes_vala
    { let docs = symbol_docs $sloc in
      let attrs = append_list_vala attrs1 attrs2 in
      Te.mk tid cs ~params ~priv ~attrs ~docs,
      ext }
;
%inline extension_constructor(opening):
    extension_constructor_declaration(opening)
      { $1 }
  | extension_constructor_rebind(opening)
      { $1 }
;
%inline extension_constructor_declaration(opening):
  d = generic_constructor_declaration(opening)
    {
      let cid, args, res, attrs, loc, info = d in
      Te.decl cid ~args ~res ~attrs ~loc ~info
    }
;
extension_constructor_rebind(opening):
  opening
  cid = mkrhs(constr_ident_vala)
  EQUAL
  lid = mkrhs(constr_longident_vala)
  attrs = attributes_vala
      { let info = symbol_info $endpos in
        Te.rebind cid lid ~attrs ~loc:(make_loc $sloc) ~info }
;

/* "with" constraints (additional type equations over signature components) */

with_constraint:
    TYPE vala(type_parameters, ANTI_LIST)
      mkrhs(vala(label_longident, ANTI_LONGLID))
      vala(with_type_binder, ANTI_PRIV)
    core_type_no_attr vala(constraints, ANTI_LIST)
      { let lident = loc_last_vala (loc_map unvala $3) in
        Pwith_type
          ($3,
           vaval (Type.mk lident
              ~params:$2
              ~cstrs:$6
              ~manifest:(vaval (Some (vaval $5)))
              ~priv:$4
              ~loc:(make_loc $sloc))) }
    /* used label_longident instead of type_longident to disallow
       functor applications in type path */
/*-*/  | TYPE tpl = type_parameters li = mkrhs(vala(label_longident, ANTI_LONGLID)) EQUAL td = ANTI_TYPEDECL
/*-*/      {
(*-*)        assert (tpl = []) ;
(*-*)        Pwith_type (li, vaant td)
(*-*)      }
  | TYPE vaval(type_parameters) mkrhs(vaval(label_longident))
    COLONEQUAL core_type_no_attr
      { let lident = loc_last (loc_map unvala $3) in
        Pwith_typesubst
         ($3,
           vaval (Type.mk (loc_map vaval lident)
              ~params:$2
              ~manifest:(vaval (Some (vaval $5)))
              ~loc:(make_loc $sloc))) }
/*-*/  | TYPE tpl = type_parameters li = mkrhs(vala(label_longident, ANTI_LONGLID)) COLONEQUAL td = ANTI_TYPEDECL
(*-*)      {
(*-*)        assert (tpl = []) ;
(*-*)        Pwith_typesubst (li, vaant td)
(*-*)      }
  | MODULE mkrhs(mod_longident_vala) EQUAL mkrhs(mod_ext_longident_vala)
      { Pwith_module ($2, $4) }
  | MODULE mkrhs(mod_longident_vala) COLONEQUAL mkrhs(mod_ext_longident_vala)
      { Pwith_modsubst ($2, $4) }
  | MODULE TYPE l=mkrhs(mty_longident) EQUAL rhs=module_type
      { Pwith_modtype (l, rhs) }
  | MODULE TYPE l=mkrhs(mty_longident) COLONEQUAL rhs=module_type
      { Pwith_modtypesubst (l, rhs) }
;
with_type_binder:
    EQUAL          { Public }
  | EQUAL PRIVATE  { Private }
;

/* Polymorphic types */

%inline typevar:
  QUOTE mkrhs(ident_vala)
    { $2 }
;
%inline typevar_list:
  nonempty_llist(typevar)
    { $1 }
;
%inline poly(X):
  vala(typevar_list, ANTI_LIST) DOT X
    { Ptyp_poly($1, $3) }
;
possibly_poly(X):
  X
    { $1 }
| mktyp(poly(X))
    { $1 }
;
%inline poly_type:
  possibly_poly(core_type)
    { $1 }
;
%inline poly_type_no_attr:
  possibly_poly(core_type_no_attr)
    { $1 }
;

(* -------------------------------------------------------------------------- *)

(* Core language types. *)

(* A core type (core_type) is a core type without attributes (core_type_no_attr)
   followed with a list of attributes. *)
core_type:
    core_type_no_attr
      { $1 }
/*-*/  | core_type_no_attr ANTI_ALGATTRS
/*-*/      { Typ.attrs $1 (vaant $2) }
  | core_type attribute
      { Typ.attr $1 $2 }
;

(* A core type without attributes is currently defined as an alias type, but
   this could change in the future if new forms of types are introduced. From
   the outside, one should use core_type_no_attr. *)
%inline core_type_no_attr:
  alias_type
    { $1 }
;

(* Alias types include:
   - function types (see below);
   - proper alias types:                  'a -> int as 'a
 *)
alias_type:
    function_type
      { $1 }
  | mktyp(
      ty = alias_type AS QUOTE tyvar = ident_vala
        { Ptyp_alias(ty, tyvar) }
    )
    { $1 }
;

(* Function types include:
   - tuple types (see below);
   - proper function types:               int -> int
                                          foo: int -> int
                                          ?foo: int -> int
 *)
function_type:
  | ty = tuple_type
    %prec MINUSGREATER
      { ty }
  | mktyp(
      label = arg_label_vala
      domain = extra_rhs(tuple_type)
      MINUSGREATER
      codomain = function_type
        { Ptyp_arrow(label, domain, codomain) }
    )
    { $1 }
;
%inline arg_label:
  | label = optlabel
      { Optional label }
  | label = lident_vala COLON
      { Labelled label }
  | /* empty */
      { Nolabel }
;
/*-*/%inline arg_label_vala:
/*-*/   vala(arg_label, ANTI_LABEL) { $1 }
/*-*/;
(* Tuple types include:
   - atomic types (see below);
   - proper tuple types:                  int * int * int list
   A proper tuple type is a star-separated list of at least two atomic types.
 *)
tuple_type:
  | ty = atomic_type
    %prec below_HASH
      { ty }
  | mktyp(
      tys = separated_nontrivial_llist(STAR, atomic_type)
        { Ptyp_tuple (vaval tys) }
/*-*/    | ANTI_TUPLELIST
/*-*/      { Ptyp_tuple (vaant $1) }
    )
    { $1 }
;

(* Atomic types are the most basic level in the syntax of types.
   Atomic types include:
   - types between parentheses:           (int -> int)
   - first-class module types:            (module S)
   - type variables:                      'a
   - applications of type constructors:   int, int list, int option list
   - variant types:                       [`A]
 *)
atomic_type:
  | LPAREN core_type RPAREN
      { $2 }
  | LPAREN MODULE ext_attributes package_type RPAREN
      { wrap_typ_attrs ~loc:$sloc (reloc_typ ~loc:$sloc $4) $3 }
  | mktyp( /* begin mktyp group */
      QUOTE ident_vala
        { Ptyp_var $2 }
/*-*/    | xtr_antis { Ptyp_xtr (Location.mkloc $1 (make_loc $sloc)) }
    | UNDERSCORE
        { Ptyp_any }
    | tys = actual_type_parameters
      tid = mkrhs(vala(type_longident, ANTI_LONGLID))
        { Ptyp_constr(tid, vaval tys) }
/*-*/    | ANTI_LIST
/*-*/      tid = mkrhs(vala(type_longident, ANTI_LONGLID))
/*-*/        { Ptyp_constr(tid, vaant $1) }
    | LESS meth_list GREATER
        { let (f, c) = $2 in Ptyp_object (vaval f, vaval c) }
/*-*/    | LESS l = ANTI_LIST c = ANTI_CLOSEDFLAG GREATER
/*-*/        { Ptyp_object (vaant l, vaant c) }
    | LESS GREATER
        { Ptyp_object (vaval [], vaval Closed) }
    | tys = vala(actual_type_parameters, ANTI_LIST)
      HASH
      cid = mkrhs(clty_longident)
        { Ptyp_class(cid, tys) }
    | LBRACKET tag_field RBRACKET
        (* not row_field; see CONFLICTS *)
        { Ptyp_variant(vaval [$2], vaval Closed, vaval None) }
    | LBRACKET BAR vala(row_field_list, ANTI_LIST) RBRACKET
        { Ptyp_variant($3, vaval  Closed, vaval None) }
    | LBRACKET row_field BAR row_field_list RBRACKET
        { Ptyp_variant(vaval ($2 :: $4), vaval Closed, vaval None) }
    | LBRACKETGREATER BAR? vala(row_field_list, ANTI_LIST) RBRACKET
        { Ptyp_variant($3, vaval Open, vaval None) }
    | LBRACKETGREATER RBRACKET
        { Ptyp_variant(vaval [], vaval Open, vaval None) }
    | LBRACKETLESS BAR? vala(row_field_list, ANTI_LIST) RBRACKET
        { Ptyp_variant($3, vaval Closed, vaval (Some(vaval []))) }
    | LBRACKETLESS BAR? vala(row_field_list, ANTI_LIST)
        GREATER vala(name_tag_list, ANTI_LIST) RBRACKET
        { Ptyp_variant($3, vaval Closed, vaval (Some $5)) }
/*-*/    | LBRACKETLESS BAR? vala(row_field_list, ANTI_LIST) ANTI_OPT RBRACKET
/*-*/        { Ptyp_variant($3, vaval Closed, vaant $4) }
/*-*/    | LBRACKET c = ANTI_CLOSEDFLAG r = ANTI_LIST l = ANTI_OPT RBRACKET
/*-*/        { Ptyp_variant(vaant r, vaant c, vaant l) }
/*-*/    | LBRACKET r = ANTI_LIST l = ANTI_OPT RBRACKET
/*-*/        { Ptyp_variant(vaant r, vaval Closed, vaant l) }
/*-*/    | LBRACKET r = ANTI_LIST RBRACKET
/*-*/        { Ptyp_variant(vaant r, vaval Closed, vaval None) }
/*-*/    | LBRACKET c = ANTI_CLOSEDFLAG r = ANTI_LIST GREATER l = vala(name_tag_list, ANTI_LIST) RBRACKET
/*-*/        { Ptyp_variant(vaant r, vaant c, vaval (Some l)) }
    | extension
        { Ptyp_extension $1 }
  )
  { $1 } /* end mktyp group */
;

(* This is the syntax of the actual type parameters in an application of
   a type constructor, such as int, int list, or (int, bool) Hashtbl.t.
   We allow one of the following:
   - zero parameters;
   - one parameter:
     an atomic type;
     among other things, this can be an arbitrary type between parentheses;
   - two or more parameters:
     arbitrary types, between parentheses, separated with commas.
 *)
%inline actual_type_parameters:
  | /* empty */
      { [] }
  | ty = atomic_type
      { [ty] }
  | LPAREN tys = separated_nontrivial_llist(COMMA, core_type) RPAREN
      { tys }
;

%inline package_type: module_type
      { let ((lid, cstrs), attrs) = package_type_of_module_type $1 in
        let descr = Ptyp_package (lid, cstrs) in
        mktyp ~loc:$sloc ~attrs descr }
;
%inline row_field_list:
  separated_nonempty_llist(BAR, row_field)
    { $1 }
;
row_field:
    tag_field
      { $1 }
  | core_type
      { Rf.inherit_ ~loc:(make_loc $sloc) $1 }
;
tag_field:
    mkrhs(name_tag_vala) OF vala(opt_ampersand, ANTI_ISCONST)
      vala(amper_type_list, ANTI_LIST) attributes_vala
      { let info = symbol_info $endpos in
        let attrs = add_info_attrs info $5 in
        Rf.tag ~loc:(make_loc $sloc) ~attrs $1 $3 $4 }
  | mkrhs(name_tag_vala) attributes_vala
      { let info = symbol_info $endpos in
        let attrs = add_info_attrs info $2 in
        Rf.tag ~loc:(make_loc $sloc) ~attrs $1 (vaval true) (vaval[]) }
;
opt_ampersand:
    AMPERSAND                                   { true }
  | /* empty */                                 { false }
;
%inline amper_type_list:
  separated_nonempty_llist(AMPERSAND, core_type_no_attr)
    { $1 }
;
%inline name_tag_list:
  nonempty_llist(name_tag)
    { $1 }
;
(* A method list (in an object type). *)
meth_list:
    head = field_semi         tail = meth_list
  | head = inherit_field SEMI tail = meth_list
      { let (f, c) = tail in (head :: f, c) }
  | head = field_semi
  | head = inherit_field SEMI
      { [head], Closed }
  | head = field
  | head = inherit_field
      { [head], Closed }
  | DOTDOT
      { [], Open }
;
%inline field:
  mkrhs(label_vala) COLON poly_type_no_attr attributes_vala
    { let info = symbol_info $endpos in
      let attrs = add_info_attrs info $4 in
      Of.tag ~loc:(make_loc $sloc) ~attrs $1 $3 }
;

%inline field_semi:
  mkrhs(label_vala) COLON poly_type_no_attr vaval(attributes) SEMI vaval(attributes)
    { let info =
        match rhs_info $endpos($4) with
        | Some _ as info_before_semi -> info_before_semi
        | None -> symbol_info $endpos
      in
      let attrs = add_info_attrs info (append_list_vala $4 $6) in
      Of.tag ~loc:(make_loc $sloc) ~attrs $1 $3 }
;

%inline inherit_field:
  ty = atomic_type
    { Of.inherit_ ~loc:(make_loc $sloc) ty }
;

%inline label:
    LIDENT                                      { $1 }
;

/*-*/%inline label_vala:
/*-*/    vala(label, ANTI_LID)                      { $1 }
/*-*/;
/*-*/
/* Constants */

constant:
  | INT          { let (n, m) = $1 in Pconst_integer (vaval n, m) }
  | CHAR         { Pconst_char (vaval $1) }
  | STRING       { let (s, strloc, d) = $1 in Pconst_string (vaval s, strloc, Option.map vaval d) }
  | FLOAT        { let (f, m) = $1 in Pconst_float (vaval f, m) }
/*-*/  | ANTI_INT     { Pconst_integer (vaant $1, None) }
/*-*/  | ANTI_INT32     { Pconst_integer (vaant $1, Some 'l') }
/*-*/  | ANTI_INT64     { Pconst_integer (vaant $1, Some 'L') }
/*-*/  | ANTI_NATIVEINT     { Pconst_integer (vaant $1, Some 'n') }
/*-*/  | ANTI_CHAR     { Pconst_char (vaant $1) }
/*-*/  | ANTI_STRING     { Pconst_string (vaant $1, make_loc $sloc, None) }
/*-*/  | ANTI_STRING ANTI_DELIM     { Pconst_string (vaant $1, make_loc $sloc, Some (vaant $2)) }
/*-*/  | ANTI_FLOAT     { Pconst_float (vaant $1, None) }
;
signed_constant:
    constant     { $1 }
  | MINUS INT    { let (n, m) = $2 in Pconst_integer(vaval("-" ^ n), m) }
  | MINUS FLOAT  { let (f, m) = $2 in Pconst_float(vaval("-" ^ f), m) }
  | PLUS INT     { let (n, m) = $2 in Pconst_integer (vaval n, m) }
  | PLUS FLOAT   { let (f, m) = $2 in Pconst_float(vaval f, m) }
;

/* Identifiers and long identifiers */

/*-*/%inline lident_vala:
/*-*/  vala(LIDENT, ANTI_LID)
/*-*/    { $1 }
/*-*/;
/*-*/
/*-*/%inline uident_vala:
/*-*/  vala(UIDENT, ANTI_UID)
/*-*/    { $1 }
/*-*/;
/*-*/
ident:
    UIDENT    { $1 }
  | LIDENT    { $1 }
;
/*-*/ident_vala:
/*-*/    uident_vala    { $1 }
/*-*/  | lident_vala    { $1 }
/*-*/;
val_extra_ident:
  | LPAREN operator RPAREN    { $2 }
  | LPAREN operator error     { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN error              { expecting $loc($2) "operator" }
  | LPAREN MODULE error       { expecting $loc($3) "module-expr" }
;
val_ident:
    LIDENT                    { $1 }
  | val_extra_ident           { $1 }
;
/*-*/%inline val_ident_vala:
/*-*/   vala(val_ident, ANTI_LID) { $1 }
/*-*/;
operator:
    PREFIXOP                                    { $1 }
  | LETOP                                       { $1 }
  | ANDOP                                       { $1 }
  | DOTOP LPAREN index_mod RPAREN               { "."^ $1 ^"(" ^ $3 ^ ")" }
  | DOTOP LPAREN index_mod RPAREN LESSMINUS     { "."^ $1 ^ "(" ^ $3 ^ ")<-" }
  | DOTOP LBRACKET index_mod RBRACKET           { "."^ $1 ^"[" ^ $3 ^ "]" }
  | DOTOP LBRACKET index_mod RBRACKET LESSMINUS { "."^ $1 ^ "[" ^ $3 ^ "]<-" }
  | DOTOP LBRACE index_mod RBRACE               { "."^ $1 ^"{" ^ $3 ^ "}" }
  | DOTOP LBRACE index_mod RBRACE LESSMINUS     { "."^ $1 ^ "{" ^ $3 ^ "}<-" }
  | HASHOP                                      { $1 }
  | BANG                                        { "!" }
  | infix_operator                              { $1 }
;
%inline infix_operator:
  | op = INFIXOP0 { op }
  | op = INFIXOP1 { op }
  | op = INFIXOP2 { op }
  | op = INFIXOP3 { op }
  | op = INFIXOP4 { op }
  | PLUS           {"+"}
  | PLUSDOT       {"+."}
  | PLUSEQ        {"+="}
  | MINUS          {"-"}
  | MINUSDOT      {"-."}
  | STAR           {"*"}
  | PERCENT        {"%"}
  | EQUAL          {"="}
  | LESS           {"<"}
  | GREATER        {">"}
  | OR            {"or"}
  | BARBAR        {"||"}
  | AMPERSAND      {"&"}
  | AMPERAMPER    {"&&"}
  | COLONEQUAL    {":="}
;
index_mod:
| { "" }
| SEMI DOTDOT { ";.." }
;

%inline constr_extra_ident:
  | LPAREN COLONCOLON RPAREN                    { "::" }
;
constr_extra_nonprefix_ident:
  | LBRACKET RBRACKET                           { "[]" }
  | LPAREN RPAREN                               { "()" }
  | FALSE                                       { "false" }
  | TRUE                                        { "true" }
;
constr_ident:
    UIDENT                                      { $1 }
  | constr_extra_ident                          { $1 }
  | constr_extra_nonprefix_ident                { $1 }
;
/*-*/%inline constr_ident_vala:
/*-*/   vala(constr_ident, ANTI_UID) { $1 }
/*-*/;
constr_longident:
    mod_longident       %prec below_DOT  { $1 } /* A.B.x vs (A).B.x */
  | mod_longident_vala DOT constr_extra_ident { Ldot($1,vaval $3) }
  | constr_extra_ident                   { Lident (vaval $1) }
  | constr_extra_nonprefix_ident         { Lident (vaval $1) }
;
/*-*/%inline constr_longident_vala:
/*-*/  vala(constr_longident, ANTI_LONGID) { $1 }
/*-*/;

mk_longident(prefix,final):
   | final            { Lident ($1) }
   | prefix DOT final { Ldot($1,$3) }
;
val_longident:
    mk_longident(mod_longident_vala, val_ident_vala) { $1 }
;
label_longident:
    mk_longident(mod_longident_vala, lident_vala) { $1 }
;
type_longident:
    mk_longident(mod_ext_longident_vala, lident_vala)  { $1 }
;
mod_longident:
    mk_longident(mod_longident_vala, uident_vala)  { $1 }
;
/*-*/%inline mod_longident_vala:
/*-*/  vala(mod_longident, ANTI_LONGID) { $1 }
/*-*/;
mod_ext_longident:
    mk_longident(mod_ext_longident_vala, uident_vala) { $1 }
  | mod_ext_longident_vala LPAREN mod_ext_longident_vala RPAREN
      { lapply ~loc:$sloc $1 $3 }
  | vala(mod_ext_longident, ANTI_LONGID) LPAREN error
      { expecting $loc($3) "module path" }
;
/*-*/%inline mod_ext_longident_vala:
/*-*/   vala(mod_ext_longident, ANTI_LONGID) { $1 }
/*-*/;
mty_longident:
    mk_longident(mod_ext_longident_vala,ident_vala) { $1 }
;
clty_longident:
    mk_longident(mod_ext_longident_vala,lident_vala) { $1 }
;
class_longident:
   mk_longident(mod_longident_vala,lident_vala) { $1 }
;

/* BEGIN AVOID */
/* For compiler-libs: parse all valid longidents and a little more:
   final identifiers which are value specific are accepted even when
   the path prefix is only valid for types: (e.g. F(X).(::)) */
any_longident:
  | mk_longident (mod_ext_longident_vala,
     ident_vala | vaval(constr_extra_ident) | vaval(val_extra_ident) { $1 }
    ) { $1 }
  | constr_extra_nonprefix_ident { Lident (vaval $1) }
;
/* END AVOID */

/* Toplevel directives */

toplevel_directive:
  HASH dir = mkrhs(ident)
  arg = ioption(mk_directive_arg(toplevel_directive_argument))
    { mk_directive ~loc:$sloc dir arg }
;

%inline toplevel_directive_argument:
  | STRING        { let (s, _, _) = $1 in Pdir_string s }
  | INT           { let (n, m) = $1 in Pdir_int (n ,m) }
  | val_longident { Pdir_ident $1 }
  | mod_longident { Pdir_ident $1 }
  | FALSE         { Pdir_bool false }
  | TRUE          { Pdir_bool true }
;

/* Miscellaneous */

(* The symbol epsilon can be used instead of an /* empty */ comment. *)
%inline epsilon:
  /* empty */
    { () }
;

%inline raw_string:
  s = STRING
    { let body, _, _ = s in body }
;

name_tag:
    BACKQUOTE ident                             { $2 }
;
/*-*/name_tag_vala:
/*-*/    BACKQUOTE vala(ident, ANTI_ID)              { $2 }
/*-*/;
rec_flag:
    /* empty */                                 { Nonrecursive }
  | REC                                         { Recursive }
;
/*-*/%inline rec_flag_vala:
/*-*/   vala(rec_flag, ANTI_RECFLAG) { $1 }
/*-*/;
%inline nonrec_flag:
    /* empty */                                 { Recursive }
  | NONREC                                      { Nonrecursive }
;
/*-*/%inline nonrec_flag_vala:
/*-*/   vala(nonrec_flag, ANTI_NONRECFLAG) { $1 }
/*-*/;
%inline no_nonrec_flag:
    /* empty */ { Recursive }
/* BEGIN AVOID */
  | NONREC      { not_expecting $loc "nonrec flag" }
/* END AVOID */
;
direction_flag:
    TO                                          { Upto }
  | DOWNTO                                      { Downto }
;
/*-*/%inline direction_flag_vala:
/*-*/   vala(direction_flag, ANTI_DIRFLAG) { $1 }
/*-*/;
private_flag:
  inline_private_flag
    { $1 }
;
/*-*/%inline private_flag_vala:
/*-*/   vala(private_flag, ANTI_PRIV) { $1 }
/*-*/;
%inline inline_private_flag:
    /* empty */                                 { Public }
  | PRIVATE                                     { Private }
;
/*-*/%inline inline_private_flag_vala:
/*-*/  vala(inline_private_flag, ANTI_PRIV) { $1 }
/*-*/;
mutable_flag:
    /* empty */                                 { Immutable }
  | MUTABLE                                     { Mutable }
;
/*-*/%inline mutable_flag_vala:
/*-*/   vala(mutable_flag, ANTI_MUTABLE) { $1 }
/*-*/;
virtual_flag:
    /* empty */                                 { Concrete }
  | VIRTUAL                                     { Virtual }
;
/*-*/%inline virtual_flag_vala:
/*-*/   vala(virtual_flag, ANTI_VIRTUAL) { $1 }
/*-*/;
mutable_virtual_flags:
    /* empty */
      { Immutable, Concrete }
  | MUTABLE
      { Mutable, Concrete }
  | VIRTUAL
      { Immutable, Virtual }
  | MUTABLE VIRTUAL
  | VIRTUAL MUTABLE
      { Mutable, Virtual }
;
private_virtual_flags:
    /* empty */  { Public, Concrete }
  | PRIVATE { Private, Concrete }
  | VIRTUAL { Public, Virtual }
  | PRIVATE VIRTUAL { Private, Virtual }
  | VIRTUAL PRIVATE { Private, Virtual }
;
(* This nonterminal symbol indicates the definite presence of a VIRTUAL
   keyword and the possible presence of a MUTABLE keyword. *)
virtual_with_mutable_flag:
  | VIRTUAL { Immutable }
  | MUTABLE VIRTUAL { Mutable }
  | VIRTUAL MUTABLE { Mutable }
;
(* This nonterminal symbol indicates the definite presence of a VIRTUAL
   keyword and the possible presence of a PRIVATE keyword. *)
virtual_with_private_flag:
  | VIRTUAL { Public }
  | PRIVATE VIRTUAL { Private }
  | VIRTUAL PRIVATE { Private }
;
%inline no_override_flag:
    /* empty */                                 { Fresh }
;
%inline override_flag:
    /* empty */                                 { Fresh }
  | BANG                                        { Override }
;
/*-*/%inline override_flag_vala:
/*-*/    vala(override_flag, ANTI_OVERRIDEFLAG)      { $1 }
/*-*/;
subtractive:
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
;
additive:
  | PLUS                                        { "+" }
  | PLUSDOT                                     { "+." }
;
optlabel:
   | OPTLABEL                                   { vaval($1) }
   | QUESTION lident_vala COLON      { $2 }
;

/* Vaval(Attributes) and extensions */

single_attr_id:
    LIDENT { $1 }
  | UIDENT { $1 }
  | AND { "and" }
  | AS { "as" }
  | ASSERT { "assert" }
  | BEGIN { "begin" }
  | CLASS { "class" }
  | CONSTRAINT { "constraint" }
  | DO { "do" }
  | DONE { "done" }
  | DOWNTO { "downto" }
  | ELSE { "else" }
  | END { "end" }
  | EXCEPTION { "exception" }
  | EXTERNAL { "external" }
  | FALSE { "false" }
  | FOR { "for" }
  | FUN { "fun" }
  | FUNCTION { "function" }
  | FUNCTOR { "functor" }
  | IF { "if" }
  | IN { "in" }
  | INCLUDE { "include" }
  | INHERIT { "inherit" }
  | INITIALIZER { "initializer" }
  | LAZY { "lazy" }
  | LET { "let" }
  | MATCH { "match" }
  | METHOD { "method" }
  | MODULE { "module" }
  | MUTABLE { "mutable" }
  | NEW { "new" }
  | NONREC { "nonrec" }
  | OBJECT { "object" }
  | OF { "of" }
  | OPEN { "open" }
  | OR { "or" }
  | PRIVATE { "private" }
  | REC { "rec" }
  | SIG { "sig" }
  | STRUCT { "struct" }
  | THEN { "then" }
  | TO { "to" }
  | TRUE { "true" }
  | TRY { "try" }
  | TYPE { "type" }
  | VAL { "val" }
  | VIRTUAL { "virtual" }
  | WHEN { "when" }
  | WHILE { "while" }
  | WITH { "with" }
/* mod/land/lor/lxor/lsl/lsr/asr are not supported for now */
;

attr_id:
  mkloc(
      single_attr_id { vaval $1 }
    | single_attr_id DOT attr_id { vaval ($1 ^ "." ^ (unvala $3.txt)) }
/*-*/    | ANTI_ATTRID { vaant $1 }
  ) { $1 }
;
attribute:
  LBRACKETAT attr_id payload RBRACKET
    { Attr.mk ~loc:(make_loc $sloc) $2 $3 }
;
post_item_attribute:
  LBRACKETATAT attr_id payload RBRACKET
    { Attr.mk ~loc:(make_loc $sloc) $2 $3 }
;
floating_attribute:
  LBRACKETATATAT attr_id payload RBRACKET
    { mark_symbol_docs $sloc;
      Attr.mk ~loc:(make_loc $sloc) $2 $3 }
;
%inline post_item_attributes:
   post_item_attribute*
    { $1 }
;
/*-*/%inline post_item_attributes_vala:
/*-*/  vala(post_item_attributes, ANTI_ITEMATTRS)
/*-*/    { $1 }
/*-*/;
%inline attributes:
   attribute*
    { $1 }
;
/*-*/%inline attributes_vala:
/*-*/  vala(attributes, ANTI_ALGATTRS)
/*-*/    { $1 }
/*-*/;
ext:
  | /* empty */     { None }
  | PERCENT attr_id { Some $2 }
;
%inline no_ext:
  | /* empty */     { None }
/* BEGIN AVOID */
  | PERCENT attr_id { not_expecting $loc "extension" }
/* END AVOID */
;
%inline ext_attributes:
  ext attributes_vala    { $1, $2 }
;
extension:
  | LBRACKETPERCENT attr_id payload RBRACKET { ($2, $3) }
  | QUOTED_STRING_EXPR
    { mk_quotedext ~loc:$sloc $1 }
;
item_extension:
  | LBRACKETPERCENTPERCENT attr_id payload RBRACKET { ($2, $3) }
  | QUOTED_STRING_ITEM
    { mk_quotedext ~loc:$sloc $1 }
;
payload:
    structure { PStr $1 }
  | COLON signature { PSig $2 }
  | COLON core_type { PTyp $2 }
  | QUESTION pattern { PPat ($2, vaval None) }
  | QUESTION pattern WHEN seq_expr { PPat ($2, vaval (Some $4)) }
/*-*/  | QUESTION pattern ANTI_EXPROPT { PPat ($2, vaant $3) }
;
%%
