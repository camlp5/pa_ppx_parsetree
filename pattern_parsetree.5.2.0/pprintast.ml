(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Thomas Gazagnaire, OCamlPro                       *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*               Hongbo Zhang, University of Pennsylvania                 *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Original Code from Ber-metaocaml, modified for 3.12.0 and fixed *)
(* Printing code expressions *)
(* Authors:  Ed Pizzi, Fabrice Le Fessant *)
(* Extensive Rewrite: Hongbo Zhang: University of Pennsylvania *)
(* TODO more fine-grained precedence pretty-printing *)

open Asttypes
open Format
open Location
open Longident
open Parsetree
open Ast_helper

let prefix_symbols  = [ '!'; '?'; '~' ]
let infix_symbols = [ '='; '<'; '>'; '@'; '^'; '|'; '&'; '+'; '-'; '*'; '/';
                      '$'; '%'; '#' ]

(* type fixity = Infix| Prefix  *)
let special_infix_strings =
  ["asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"; ":="; "!="; "::" ]

let letop s =
  String.length s > 3
  && s.[0] = 'l'
  && s.[1] = 'e'
  && s.[2] = 't'
  && List.mem s.[3] infix_symbols

let andop s =
  String.length s > 3
  && s.[0] = 'a'
  && s.[1] = 'n'
  && s.[2] = 'd'
  && List.mem s.[3] infix_symbols

(* determines if the string is an infix string.
   checks backwards, first allowing a renaming postfix ("_102") which
   may have resulted from Pexp -> Texp -> Pexp translation, then checking
   if all the characters in the beginning of the string are valid infix
   characters. *)
let fixity_of_string  = function
  | "" -> `Normal
  | s when List.mem s special_infix_strings -> `Infix s
  | s when List.mem s.[0] infix_symbols -> `Infix s
  | s when List.mem s.[0] prefix_symbols -> `Prefix s
  | s when s.[0] = '.' -> `Mixfix s
  | s when letop s -> `Letop s
  | s when andop s -> `Andop s
  | _ -> `Normal

let view_fixity_of_exp = function
  | {pexp_desc = Pexp_ident {txt=Ploc.VaVal  (Lident l);_}; pexp_attributes = Ploc.VaVal []} ->
      fixity_of_string (unvala l)
  | _ -> `Normal

let is_infix  = function `Infix _ -> true | _  -> false
let is_mixfix = function `Mixfix _ -> true | _ -> false
let is_kwdop = function `Letop _ | `Andop _ -> true | _ -> false

let first_is c str =
  str <> "" && str.[0] = c
let last_is c str =
  str <> "" && str.[String.length str - 1] = c

let first_is_in cs str =
  str <> "" && List.mem str.[0] cs

(* which identifiers are in fact operators needing parentheses *)
let needs_parens txt =
  let fix = fixity_of_string txt in
  is_infix fix
  || is_mixfix fix
  || is_kwdop fix
  || first_is_in prefix_symbols txt

(* some infixes need spaces around parens to avoid clashes with comment
   syntax *)
let needs_spaces txt =
  first_is '*' txt || last_is '*' txt

(* Turn an arbitrary variable name into a valid OCaml identifier by adding \#
  in case it is a keyword, or parenthesis when it is an infix or prefix
  operator. *)
let ident_of_name ppf txt =
  let format : (_, _, _) format =
    if Lexer.is_keyword txt then "\\#%s"
    else if not (needs_parens txt) then "%s"
    else if needs_spaces txt then "(@;%s@;)"
    else "(%s)"
  in fprintf ppf format txt

let ident_of_name_loc ppf s = ident_of_name ppf s.txt
let ident_of_name_vala_loc ppf s = ident_of_name ppf (unvala s.txt)

let protect_longident ppf print_longident longprefix txt =
    if not (needs_parens txt) then
      fprintf ppf "%a.%a" print_longident longprefix ident_of_name txt
    else if needs_spaces txt then
      fprintf ppf "%a.(@;%s@;)" print_longident longprefix txt
    else
      fprintf ppf "%a.(%s)" print_longident longprefix txt

type space_formatter = (unit, Format.formatter, unit) format

let override = function
  | Override -> "!"
  | Fresh -> ""

(*-*)let override_vala x = override (unvala x)

(* variance encoding: need to sync up with the [parser.mly] *)
let type_variance = function
  | NoVariance -> ""
  | Covariant -> "+"
  | Contravariant -> "-"

let type_injectivity = function
  | NoInjectivity -> ""
  | Injective -> "!"

type construct =
  [ `cons of expression list
  | `list of expression list
  | `nil
  | `normal
  | `simple of Longident.t Ploc.vala
  | `tuple
  | `btrue
  | `bfalse ]

let view_expr x =
  match x.pexp_desc with
  | Pexp_construct ( {txt= Ploc.VaVal (Lident (Ploc.VaVal "()")); _},_) -> `tuple
  | Pexp_construct ( {txt= Ploc.VaVal (Lident (Ploc.VaVal "true")); _},_) -> `btrue
  | Pexp_construct ( {txt= Ploc.VaVal (Lident (Ploc.VaVal "false")); _},_) -> `bfalse
  | Pexp_construct ( {txt= Ploc.VaVal (Lident (Ploc.VaVal "[]"));_},_) -> `nil
  | Pexp_construct ( {txt= Ploc.VaVal (Lident (Ploc.VaVal "::"));_},Ploc.VaVal (Some _)) ->
      let rec loop exp acc = match exp with
          | {pexp_desc=Pexp_construct ({txt=Ploc.VaVal (Lident (Ploc.VaVal "[]"));_},_);
             pexp_attributes = Ploc.VaVal []} ->
              (List.rev acc,true)
          | {pexp_desc=
             Pexp_construct ({txt=Ploc.VaVal (Lident (Ploc.VaVal "::"));_},
                             Ploc.VaVal (Some ({pexp_desc= Pexp_tuple(Ploc.VaVal [e1;e2]);
                                    pexp_attributes = Ploc.VaVal []})));
             pexp_attributes = Ploc.VaVal []}
            ->
              loop e2 (e1::acc)
          | e -> (List.rev (e::acc),false) in
      let (ls,b) = loop x []  in
      if b then
        `list ls
      else `cons ls
  | Pexp_construct (x,Ploc.VaVal None) -> `simple (x.txt)
  | _ -> `normal

let is_simple_construct :construct -> bool = function
  | `nil | `tuple | `list _ | `simple _ | `btrue | `bfalse -> true
  | `cons _ | `normal -> false

let pp = fprintf

type ctxt = {
  pipe : bool;
  semi : bool;
  ifthenelse : bool;
  functionrhs : bool;
}

let reset_ctxt = { pipe=false; semi=false; ifthenelse=false; functionrhs=false }
let under_pipe ctxt = { ctxt with pipe=true }
let under_semi ctxt = { ctxt with semi=true }
let under_ifthenelse ctxt = { ctxt with ifthenelse=true }
let under_functionrhs ctxt = { ctxt with functionrhs = true }
(*
let reset_semi ctxt = { ctxt with semi=false }
let reset_ifthenelse ctxt = { ctxt with ifthenelse=false }
let reset_pipe ctxt = { ctxt with pipe=false }
*)

let list : 'a . ?sep:space_formatter -> ?first:space_formatter ->
  ?last:space_formatter -> (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit
  = fun ?sep ?first ?last fu f xs ->
    let first = match first with Some x -> x |None -> ("": _ format6)
    and last = match last with Some x -> x |None -> ("": _ format6)
    and sep = match sep with Some x -> x |None -> ("@ ": _ format6) in
    let aux f = function
      | [] -> ()
      | [x] -> fu f x
      | xs ->
          let rec loop  f = function
            | [x] -> fu f x
            | x::xs ->  fu f x; pp f sep; loop f xs;
            | _ -> assert false in begin
            pp f first; loop f xs; pp f last;
          end in
    aux f xs

let option : 'a. ?first:space_formatter -> ?last:space_formatter ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit
  = fun  ?first  ?last fu f a ->
    let first = match first with Some x -> x | None -> ("": _ format6)
    and last = match last with Some x -> x | None -> ("": _ format6) in
    match a with
    | None -> ()
    | Some x -> pp f first; fu f x; pp f last

let paren: 'a . ?first:space_formatter -> ?last:space_formatter ->
  bool -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
  = fun  ?(first=("": _ format6)) ?(last=("": _ format6)) b fu f x ->
    if b then (pp f "("; pp f first; fu f x; pp f last; pp f ")")
    else fu f x

let rec longident f = function
  | Lident s -> ident_of_name f (unvala s)
  | Ldot(y,s) -> protect_longident f longident (unvala y) (unvala s)
  | Lapply (y,s) ->
      pp f "%a(%a)" longident (unvala y) longident (unvala s)

let longident_loc f x = pp f "%a" longident x.txt
(*-*)let longident_vala_loc f x = pp f "%a" longident (unvala x.txt)

let constant f = function
  | Pconst_char (Ploc.VaVal i) ->
      pp f "%C"  i
  | Pconst_string (Ploc.VaVal i, _, None) ->
      pp f "%S" i
  | Pconst_string (Ploc.VaVal i, _, Some (Ploc.VaVal delim)) ->
      pp f "{%s|%s|%s}" delim i delim
  | Pconst_integer (Ploc.VaVal i, None) ->
      paren (first_is '-' i) (fun f -> pp f "%s") f i
  | Pconst_integer (Ploc.VaVal i, Some m) ->
      paren (first_is '-' i) (fun f (i, m) -> pp f "%s%c" i m) f (i,m)
  | Pconst_float (Ploc.VaVal i, None) ->
      paren (first_is '-' i) (fun f -> pp f "%s") f i
  | Pconst_float (Ploc.VaVal i, Some m) ->
      paren (first_is '-' i) (fun f (i,m) -> pp f "%s%c" i m) f (i,m)

(* trailing space*)
let mutable_flag f = function
  | Immutable -> ()
  | Mutable -> pp f "mutable@;"
(*-*)let mutable_flag_vala f x = mutable_flag f (unvala x)

let virtual_flag f  = function
  | Concrete -> ()
  | Virtual -> pp f "virtual@;"
(*-*)let virtual_flag_vala f x = virtual_flag f (unvala x)

(* trailing space added *)
let rec_flag f rf =
  match rf with
  | Nonrecursive -> ()
  | Recursive -> pp f "rec "
(*-*)let rec_flag_vala f x = rec_flag f (unvala x)

let nonrec_flag f rf =
  match rf with
  | Nonrecursive -> pp f "nonrec "
  | Recursive -> ()
(*-*)let nonrec_flag_vala f x = nonrec_flag f (unvala x)

let direction_flag f = function
  | Upto -> pp f "to@ "
  | Downto -> pp f "downto@ "
(*-*)let direction_flag_vala f x = direction_flag f (unvala x)

let private_flag f = function
  | Public -> ()
  | Private -> pp f "private@ "
(*-*)let private_flag_vala f x = private_flag f (unvala x)

let iter_loc f ctxt {txt; loc = _} = f ctxt txt
(*-*)let iter_vala_loc f ctxt {txt; loc = _} = f ctxt (unvala txt)

let constant_string f s = pp f "%S" s

let tyvar_of_name s =
  if String.length s >= 2 && s.[1] = '\'' then
    (* without the space, this would be parsed as
       a character literal *)
    "' " ^ s
  else if Lexer.is_keyword s then
    "'\\#" ^ s
  else if String.equal s "_" then
    s
  else
    "'" ^ s

let tyvar ppf s =
  Format.fprintf ppf "%s" (tyvar_of_name s)

let tyvar_loc f str = tyvar f (unvala str.txt)
let string_quot f x = pp f "`%a" ident_of_name x

(* c ['a,'b] *)
let rec class_params_def ctxt f =  function
  | [] -> ()
  | l ->
      pp f "[%a] " (* space *)
        (list (type_param ctxt) ~sep:",") l

and type_with_label ctxt f (label, c) =
  match label with
  | Nolabel    -> core_type1 ctxt f c (* otherwise parenthesize *)
  | Labelled s -> pp f "%a:%a" ident_of_name (unvala s) (core_type1 ctxt) c
  | Optional s -> pp f "?%a:%a" ident_of_name (unvala s) (core_type1 ctxt) c

and core_type ctxt f x =
  if unvala x.ptyp_attributes <> [] then begin
    pp f "((%a)%a)" (core_type ctxt) {x with ptyp_attributes=Ploc.VaVal []}
      (attributes ctxt) x.ptyp_attributes
  end
  else match x.ptyp_desc with
    | Ptyp_arrow (l, ct1, ct2) ->
        pp f "@[<2>%a@;->@;%a@]" (* FIXME remove parens later *)
          (type_with_label ctxt) (unvala l,ct1) (core_type ctxt) ct2
    | Ptyp_alias (ct, s) ->
        pp f "@[<2>%a@;as@;%a@]" (core_type1 ctxt) ct tyvar (unvala s.txt)
    | Ptyp_poly (Ploc.VaVal [], ct) ->
        core_type ctxt f ct
    | Ptyp_poly (Ploc.VaVal sl, ct) ->
        pp f "@[<2>%a%a@]"
               (fun f l -> match l with
                  | [] -> ()
                  | _ ->
                      pp f "%a@;.@;"
                        (list tyvar_loc ~sep:"@;")  l)
          sl (core_type ctxt) ct
    | _ -> pp f "@[<2>%a@]" (core_type1 ctxt) x

and core_type1 ctxt f x =
  if unvala x.ptyp_attributes <> [] then core_type ctxt f x
  else match x.ptyp_desc with
    | Ptyp_any -> pp f "_";
    | Ptyp_var s -> tyvar f  (unvala s);
    | Ptyp_tuple (Ploc.VaVal l) ->  pp f "(%a)" (list (core_type1 ctxt) ~sep:"@;*@;") l
    | Ptyp_constr (li, Ploc.VaVal l) ->
        pp f (* "%a%a@;" *) "%a%a"
          (fun f l -> match l with
             |[] -> ()
             |[x]-> pp f "%a@;" (core_type1 ctxt)  x
             | _ -> list ~first:"(" ~last:")@;" (core_type ctxt) ~sep:",@;" f l)
          l longident_vala_loc li
    | Ptyp_variant (l, closed, low) ->
        let first_is_inherit = match unvala l with
          | {Parsetree.prf_desc = Rinherit _}::_ -> true
          | _ -> false in
        let type_variant_helper f x =
          match x.prf_desc with
          | Rtag (l, _, ctl) ->
              pp f "@[<2>%a%a@;%a@]" (iter_vala_loc string_quot) l
                (fun f l -> match l with
                   |[] -> ()
                   | _ -> pp f "@;of@;%a"
                            (list (core_type ctxt) ~sep:"&")  (unvala ctl)) (unvala ctl)
                (attributes ctxt) x.prf_attributes
          | Rinherit ct -> core_type ctxt f ct in
        pp f "@[<2>[%a%a]@]"
          (fun f l ->
             match unvala l, unvala closed with
             | [], Closed -> ()
             | [], Open -> pp f ">" (* Cf #7200: print [>] correctly *)
             | _ ->
                 pp f "%s@;%a"
                   (match (unvala closed,unvala low) with
                    | (Closed,None) -> if first_is_inherit then " |" else ""
                    | (Closed,Some _) -> "<" (* FIXME desugar the syntax sugar*)
                    | (Open,_) -> ">")
                   (list type_variant_helper ~sep:"@;<1 -2>| ") (unvala l)) l
          (fun f low -> match low with
             |Some [] |None -> ()
             |Some xs ->
                 pp f ">@ %a"
                   (list string_quot) xs) (Option.map unvala (unvala low))
    | Ptyp_object (l, o) ->
        let core_field_type f x = match x.pof_desc with
          | Otag (l, ct) ->
            (* Cf #7200 *)
            pp f "@[<hov2>%a: %a@ %a@ @]" ident_of_name (unvala l.txt)
              (core_type ctxt) ct (attributes ctxt) x.pof_attributes
          | Oinherit ct ->
            pp f "@[<hov2>%a@ @]" (core_type ctxt) ct
        in
        let field_var f = function
          | Asttypes.Closed -> ()
          | Asttypes.Open ->
              match unvala l with
              | [] -> pp f ".."
              | _ -> pp f " ;.."
        in
        pp f "@[<hov2><@ %a%a@ > @]"
          (list core_field_type ~sep:";") (unvala l)
          field_var (unvala o) (* Cf #7200 *)
    | Ptyp_class (li, l) ->   (*FIXME*)
        pp f "@[<hov2>%a#%a@]"
          (list (core_type ctxt) ~sep:"," ~first:"(" ~last:")") (unvala l)
          longident_loc li
    | Ptyp_package (lid, cstrs) ->
        let aux f (s, ct) =
          pp f "type %a@ =@ %a" longident_vala_loc s (core_type ctxt) ct  in
        (match unvala cstrs with
         |[] -> pp f "@[<hov2>(module@ %a)@]" longident_vala_loc lid
         |_ ->
             pp f "@[<hov2>(module@ %a@ with@ %a)@]" longident_vala_loc lid
               (list aux  ~sep:"@ and@ ")  (unvala cstrs))
    | Ptyp_open(li, ct) ->
       pp f "@[<hov2>%a.(%a)@]" longident_vala_loc li (core_type ctxt) ct
    | Ptyp_extension e -> extension ctxt f e
    | (Ptyp_arrow _ | Ptyp_alias _ | Ptyp_poly _) ->
       paren true (core_type ctxt) f x

(********************pattern********************)
(* be cautious when use [pattern], [pattern1] is preferred *)
and pattern ctxt f x =
  if unvala x.ppat_attributes <> [] then begin
    pp f "((%a)%a)" (pattern ctxt) {x with ppat_attributes=Ploc.VaVal []}
      (attributes ctxt) x.ppat_attributes
  end
  else match x.ppat_desc with
    | Ppat_alias (p, s) ->
        pp f "@[<2>%a@;as@;%a@]" (pattern ctxt) p ident_of_name (unvala s.txt)
    | _ -> pattern_or ctxt f x

and pattern_or ctxt f x =
  let rec left_associative x acc = match x with
    | {ppat_desc=Ppat_or (p1,p2); ppat_attributes = Ploc.VaVal []} ->
        left_associative p1 (p2 :: acc)
    | x -> x :: acc
  in
  match left_associative x [] with
  | [] -> assert false
  | [x] -> pattern1 ctxt f x
  | orpats ->
      pp f "@[<hov0>%a@]" (list ~sep:"@ | " (pattern1 ctxt)) orpats

and pattern1 ctxt (f:Format.formatter) (x:pattern) : unit =
  let rec pattern_list_helper f = function
    | {ppat_desc =
         Ppat_construct
           ({ txt = Ploc.VaVal (Lident(Ploc.VaVal "::")) ;_},
            Ploc.VaVal (Some (Ploc.VaVal [], {ppat_desc = Ppat_tuple(Ploc.VaVal [pat1; pat2]);_})));
       ppat_attributes = Ploc.VaVal []}

      ->
        pp f "%a::%a" (simple_pattern ctxt) pat1 pattern_list_helper pat2 (*RA*)
    | p -> pattern1 ctxt f p
  in
  if unvala x.ppat_attributes <> [] then pattern ctxt f x
  else match x.ppat_desc with
    | Ppat_variant (l, Ploc.VaVal (Some p)) ->
        pp f "@[<2>`%a@;%a@]" ident_of_name (unvala l) (simple_pattern ctxt) p
    | Ppat_construct (({txt=Ploc.VaVal (Lident(Ploc.VaVal ("()"|"[]"|"true"|"false")));_}), _) ->
        simple_pattern ctxt f x
    | Ppat_construct (({txt;_} as li), po) ->
        (* FIXME The third field always false *)
        if unvala txt = Lident (Ploc.VaVal "::") then
          pp f "%a" pattern_list_helper x
        else
          (match unvala po with
           | Some (Ploc.VaVal [], x) ->
               pp f "%a@;%a"  longident_vala_loc li (simple_pattern ctxt) x
           | Some (Ploc.VaVal vl, x) ->
               pp f "%a@ (type %a)@;%a" longident_vala_loc li
                 (list ~sep:"@ " ident_of_name_vala_loc) vl
                 (simple_pattern ctxt) x
           | None -> pp f "%a" longident_vala_loc li)
    | _ -> simple_pattern ctxt f x

and simple_pattern ctxt (f:Format.formatter) (x:pattern) : unit =
  if unvala x.ppat_attributes <> [] then pattern ctxt f x
  else match x.ppat_desc with
    | Ppat_construct (({txt=Ploc.VaVal (Lident (Ploc.VaVal ("()"|"[]"|"true"|"false" as x)));_}), Ploc.VaVal None) ->
        pp f  "%s" x
    | Ppat_any -> pp f "_";
    | Ppat_var ({txt = txt;_}) -> ident_of_name f (unvala txt)
    | Ppat_array l ->
        pp f "@[<2>[|%a|]@]"  (list (pattern1 ctxt) ~sep:";") (unvala l)
    | Ppat_unpack { txt = Ploc.VaVal None } ->
        pp f "(module@ _)@ "
    | Ppat_unpack { txt = Ploc.VaVal (Some s) } ->
        pp f "(module@ %s)@ " (unvala s)
    | Ppat_type li ->
        pp f "#%a" longident_vala_loc li
    | Ppat_record (l, closed) ->
        let longident_x_pattern f (li, p) =
          match (li,p) with
          | ({txt=Ploc.VaVal (Lident s);_ },
             {ppat_desc=Ppat_var {txt;_};
              ppat_attributes=Ploc.VaVal []; _})
            when s = txt ->
              pp f "@[<2>%a@]"  longident_vala_loc li
          | _ ->
              pp f "@[<2>%a@;=@;%a@]" longident_vala_loc li (pattern1 ctxt) p
        in
        begin match unvala closed with
        | Closed ->
            pp f "@[<2>{@;%a@;}@]" (list longident_x_pattern ~sep:";@;") (unvala l)
        | _ ->
            pp f "@[<2>{@;%a;_}@]" (list longident_x_pattern ~sep:";@;") (unvala l)
        end
    | Ppat_tuple (Ploc.VaVal l) ->
        pp f "@[<1>(%a)@]" (list  ~sep:",@;" (pattern1 ctxt))  l (* level1*)
    | Ppat_constant (c) -> pp f "%a" constant (unvala c)
    | Ppat_interval (c1, c2) -> pp f "%a..%a" constant (unvala c1) constant (unvala c2)
    | Ppat_variant (l,Ploc.VaVal None) ->  pp f "`%a" ident_of_name (unvala l)
    | Ppat_constraint (p, ct) ->
        pp f "@[<2>(%a@;:@;%a)@]" (pattern1 ctxt) p (core_type ctxt) ct
    | Ppat_lazy p ->
        pp f "@[<2>(lazy@;%a)@]" (simple_pattern ctxt) p
    | Ppat_exception p ->
        pp f "@[<2>exception@;%a@]" (pattern1 ctxt) p
    | Ppat_extension e -> extension ctxt f e
    | Ppat_open (lid, p) ->
        let with_paren =
        match p.ppat_desc with
        | Ppat_array _ | Ppat_record _
        | Ppat_construct (({txt=Ploc.VaVal (Lident (Ploc.VaVal ("()"|"[]"|"true"|"false")));_}), Ploc.VaVal None) ->
            false
        | _ -> true in
        pp f "@[<2>%a.%a @]" longident_vala_loc lid
          (paren with_paren @@ pattern1 ctxt) p
    | _ -> paren true (pattern ctxt) f x

and label_exp ctxt f (l,opt,p) =
  match l with
  | Nolabel ->
      (* single case pattern parens needed here *)
      pp f "%a@ " (simple_pattern ctxt) p
  | Optional rest ->
      begin match p with
      | {ppat_desc = Ppat_var {txt;_}; ppat_attributes = Ploc.VaVal []}
        when txt = rest ->
          (match opt with
           | Some o ->
              pp f "?(%a=@;%a)@;" ident_of_name (unvala rest)  (expression ctxt) o
           | None -> pp f "?%a@ " ident_of_name (unvala rest))
      | _ ->
          (match opt with
           | Some o ->
               pp f "?%a:(%a=@;%a)@;"
                 ident_of_name (unvala rest) (pattern1 ctxt) p (expression ctxt) o
           | None -> pp f "?%a:%a@;" ident_of_name (unvala rest) (simple_pattern ctxt) p)
      end
  | Labelled l -> match p with
    | {ppat_desc  = Ppat_var {txt;_}; ppat_attributes = Ploc.VaVal []}
      when unvala txt = unvala l ->
        pp f "~%a@;" ident_of_name (unvala l)
    | _ ->  pp f "~%a:%a@;" ident_of_name (unvala l) (simple_pattern ctxt) p

and sugar_expr ctxt f e =
  if unvala e.pexp_attributes <> [] then false
  else match e.pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_ident {txt = id; _};
                  pexp_attributes=Ploc.VaVal []; _}, args)
    when List.for_all (fun (lab, _) -> lab = Nolabel) (unvala args) -> begin
      let print_indexop a path_prefix assign left sep right print_index indices
          rem_args =
        let print_path ppf = function
          | None -> ()
          | Some m -> pp ppf ".%a" longident m in
        match assign, rem_args with
            | false, [] ->
              pp f "@[%a%a%s%a%s@]"
                (simple_expr ctxt) a print_path path_prefix
                left (list ~sep print_index) indices right; true
            | true, [v] ->
              pp f "@[%a%a%s%a%s@ <-@;<1 2>%a@]"
                (simple_expr ctxt) a print_path path_prefix
                left (list ~sep print_index) indices right
                (simple_expr ctxt) v; true
            | _ -> false in
      match unvala id, List.map snd (unvala args) with
      | Lident (Ploc.VaVal "!"), [e] ->
        pp f "@[<hov>!%a@]" (simple_expr ctxt) e; true
      | Ldot (path, (Ploc.VaVal ("get"|"set" as func))), a :: other_args -> begin
          let assign = func = "set" in
          let print = print_indexop a None assign in
          match unvala path, other_args with
          | Lident (Ploc.VaVal "Array"), i :: rest ->
            print ".(" "" ")" (expression ctxt) [i] rest
          | Lident (Ploc.VaVal "String"), i :: rest ->
            print ".[" "" "]" (expression ctxt) [i] rest
          | Ldot (Ploc.VaVal (Lident (Ploc.VaVal "Bigarray")), (Ploc.VaVal "Array1")), i1 :: rest ->
            print ".{" "," "}" (simple_expr ctxt) [i1] rest
          | Ldot (Ploc.VaVal (Lident (Ploc.VaVal "Bigarray")), (Ploc.VaVal "Array2")), i1 :: i2 :: rest ->
            print ".{" "," "}" (simple_expr ctxt) [i1; i2] rest
          | Ldot (Ploc.VaVal (Lident (Ploc.VaVal "Bigarray")), (Ploc.VaVal "Array3")), i1 :: i2 :: i3 :: rest ->
            print ".{" "," "}" (simple_expr ctxt) [i1; i2; i3] rest
          | Ldot (Ploc.VaVal (Lident (Ploc.VaVal "Bigarray")), (Ploc.VaVal "Genarray")),
            {pexp_desc = Pexp_array indexes; pexp_attributes = Ploc.VaVal []} :: rest ->
              print ".{" "," "}" (simple_expr ctxt) (unvala indexes) rest
          | _ -> false
        end
      | (Lident (Ploc.VaVal s) | Ldot(_,Ploc.VaVal s)) , a :: i :: rest
        when first_is '.' s ->
          (* extract operator:
             assignment operators end with [right_bracket ^ "<-"],
             access operators end with [right_bracket] directly
          *)
          let multi_indices = String.contains s ';' in
          let i =
              match i.pexp_desc with
                | Pexp_array l when multi_indices -> unvala l
                | _ -> [ i ] in
          let assign = last_is '-' s in
          let kind =
            (* extract the right end bracket *)
            let n = String.length s in
            if assign then s.[n - 3] else s.[n - 1] in
          let left, right = match kind with
            | ')' -> '(', ")"
            | ']' -> '[', "]"
            | '}' -> '{', "}"
            | _ -> assert false in
          let path_prefix = match unvala id with
            | Ldot(m,_) -> Some m
            | _ -> None in
          let left = String.sub s 0 (1+String.index s left) in
          print_indexop a (Option.map unvala path_prefix) assign left ";" right
            (if multi_indices then expression ctxt else simple_expr ctxt)
            i rest
      | _ -> false
    end
  | _ -> false

and function_param ctxt f param =
  match param.pparam_desc with
  | Pparam_val (a, b, c) -> label_exp ctxt f (unvala a, unvala b, c)
  | Pparam_newtype ty -> pp f "(type %a)@;" ident_of_name (unvala ty.txt)

and function_body ctxt f function_body =
  match function_body with
  | Pfunction_body body -> expression ctxt f body
  | Pfunction_cases (cases, _, attrs) ->
      pp f "@[<hv>function%a%a@]"
        (item_attributes ctxt) attrs
        (case_list ctxt) (unvala cases)

and type_constraint ctxt f constraint_ =
  match constraint_ with
  | Pconstraint ty ->
      pp f ":@;%a" (core_type ctxt) ty
  | Pcoerce (ty1, ty2) ->
      pp f "%a:>@;%a"
        (option ~first:":@;" (core_type ctxt)) ty1
        (core_type ctxt) ty2

and function_params_then_body ctxt f params constraint_ body ~delimiter =
  pp f "%a%a%s@;%a"
    (list (function_param ctxt) ~sep:"") params
    (option (type_constraint ctxt)) constraint_
    delimiter
    (function_body (under_functionrhs ctxt)) body

and expression ctxt f x =
  if unvala x.pexp_attributes <> [] then
    pp f "((%a)@,%a)" (expression ctxt) {x with pexp_attributes= vaval []}
      (attributes ctxt) x.pexp_attributes
  else match x.pexp_desc with
    | Pexp_function _ | Pexp_match _ | Pexp_try _ | Pexp_sequence _
    | Pexp_newtype _
      when ctxt.pipe || ctxt.semi ->
        paren true (expression reset_ctxt) f x
    | Pexp_ifthenelse _ | Pexp_sequence _ when ctxt.ifthenelse ->
        paren true (expression reset_ctxt) f x
    | Pexp_let _ | Pexp_letmodule _ | Pexp_open _
      | Pexp_letexception _ | Pexp_letop _
        when ctxt.semi ->
        paren true (expression reset_ctxt) f x
    | Pexp_newtype (lid, e) ->
        pp f "@[<2>fun@;(type@;%a)@;->@;%a@]" ident_of_name (unvala lid.txt)
          (expression ctxt) e
    | Pexp_function (params, c, body) ->
        begin match params, c with
        (* Omit [fun] if there are no params. *)
        | Ploc.VaVal [], Ploc.VaVal None ->
            (* If function cases are a direct body of a function,
               the function node should be wrapped in parens so
               it doesn't become part of the enclosing function. *)
            let should_paren =
              match body with
              | Pfunction_cases _ -> ctxt.functionrhs
              | Pfunction_body _ -> false
            in
            let ctxt' = if should_paren then reset_ctxt else ctxt in
            pp f "@[<2>%a@]" (paren should_paren (function_body ctxt')) body
        | Ploc.VaVal [], Ploc.VaVal (Some c) ->
            pp f "@[<2>(%a@;%a)@]"
              (function_body ctxt) body
              (type_constraint ctxt) (unvala c)
        | Ploc.VaVal (_ :: _), _ ->
          pp f "@[<2>fun@;%a@]"
            (fun f () ->
               function_params_then_body ctxt f (unvala params) (Option.map unvala (unvala c)) body ~delimiter:"->")
            ();

        end
    | Pexp_match (e, l) ->
        pp f "@[<hv0>@[<hv0>@[<2>match %a@]@ with@]%a@]"
          (expression reset_ctxt) e (case_list ctxt) (unvala l)

    | Pexp_try (e, l) ->
        pp f "@[<0>@[<hv2>try@ %a@]@ @[<0>with%a@]@]"
             (* "try@;@[<2>%a@]@\nwith@\n%a"*)
          (expression reset_ctxt) e  (case_list ctxt) (unvala l)
    | Pexp_let (rf, l, e) ->
        (* pp f "@[<2>let %a%a in@;<1 -2>%a@]"
           (*no indentation here, a new line*) *)
        (*   rec_flag rf *)
        pp f "@[<2>%a in@;<1 -2>%a@]"
          (bindings reset_ctxt) (rf,l)
          (expression ctxt) e
    | Pexp_apply (e, l) ->
        begin if not (sugar_expr ctxt f x) then
            match view_fixity_of_exp e with
            | `Infix s ->
                begin match unvala l with
                | [ (Nolabel, _) as arg1; (Nolabel, _) as arg2 ] ->
                    (* FIXME associativity label_x_expression_param *)
                    pp f "@[<2>%a@;%s@;%a@]"
                      (label_x_expression_param reset_ctxt) arg1 s
                      (label_x_expression_param ctxt) arg2
                | _ ->
                    pp f "@[<2>%a %a@]"
                      (simple_expr ctxt) e
                      (list (label_x_expression_param ctxt)) (unvala l)
                end
            | `Prefix s ->
                let s =
                  if List.mem s ["~+";"~-";"~+.";"~-."] &&
                   (match unvala l with
                    (* See #7200: avoid turning (~- 1) into (- 1) which is
                       parsed as an int literal *)
                    |[(_,{pexp_desc=Pexp_constant _})] -> false
                    | _ -> true)
                  then String.sub s 1 (String.length s -1)
                  else s in
                begin match unvala l with
                | [(Nolabel, x)] ->
                  pp f "@[<2>%s@;%a@]" s (simple_expr ctxt) x
                | _   ->
                  pp f "@[<2>%a %a@]" (simple_expr ctxt) e
                    (list (label_x_expression_param ctxt)) (unvala l)
                end
            | _ ->
                pp f "@[<hov2>%a@]" begin fun f (e,l) ->
                  pp f "%a@ %a" (expression2 ctxt) e
                    (list (label_x_expression_param reset_ctxt))  l
                    (* reset here only because [function,match,try,sequence]
                       are lower priority *)
                end (e,unvala l)
        end

    | Pexp_construct (li, Ploc.VaVal (Some eo))
      when not (is_simple_construct (view_expr x))-> (* Not efficient FIXME*)
        (match view_expr x with
         | `cons ls -> list (simple_expr ctxt) f ls ~sep:"@;::@;"
         | `normal ->
             pp f "@[<2>%a@;%a@]" longident_vala_loc li
               (simple_expr ctxt) eo
         | _ -> assert false)
    | Pexp_setfield (e1, li, e2) ->
        pp f "@[<2>%a.%a@ <-@ %a@]"
          (simple_expr ctxt) e1 longident_vala_loc li (simple_expr ctxt) e2
    | Pexp_ifthenelse (e1, e2, eo) ->
        (* @;@[<2>else@ %a@]@] *)
        let fmt:(_,_,_)format ="@[<hv0>@[<2>if@ %a@]@;@[<2>then@ %a@]%a@]" in
        let expression_under_ifthenelse = expression (under_ifthenelse ctxt) in
        pp f fmt expression_under_ifthenelse e1 expression_under_ifthenelse e2
          (fun f eo -> match eo with
             | Some x ->
                 pp f "@;@[<2>else@;%a@]" (expression (under_semi ctxt)) x
             | None -> () (* pp f "()" *)) (unvala eo)
    | Pexp_sequence _ ->
        let rec sequence_helper acc = function
          | {pexp_desc=Pexp_sequence(e1,e2); pexp_attributes = Ploc.VaVal []} ->
              sequence_helper (e1::acc) e2
          | v -> List.rev (v::acc) in
        let lst = sequence_helper [] x in
        pp f "@[<hv>%a@]"
          (list (expression (under_semi ctxt)) ~sep:";@;") lst
    | Pexp_new (li) ->
        pp f "@[<hov2>new@ %a@]" longident_vala_loc li;
    | Pexp_setinstvar (s, e) ->
        pp f "@[<hov2>%a@ <-@ %a@]" ident_of_name (unvala s.txt) (expression ctxt) e
    | Pexp_override l -> (* FIXME *)
        let string_vala_x_expression f (s, e) =
          pp f "@[<hov2>%a@ =@ %a@]" ident_of_name (unvala s.txt) (expression ctxt) e in
        pp f "@[<hov2>{<%a>}@]"
          (list string_vala_x_expression  ~sep:";"  )  (unvala l);
    | Pexp_letmodule (s, me, e) ->
        pp f "@[<hov2>let@ module@ %s@ =@ %a@ in@ %a@]"
          (unvala (Option.value (unvala s.txt) ~default:(vaval "_")))
          (module_expr reset_ctxt) me (expression ctxt) e
    | Pexp_letexception (cd, e) ->
        pp f "@[<hov2>let@ exception@ %a@ in@ %a@]"
          (extension_constructor ctxt) (unvala cd)
          (expression ctxt) e
    | Pexp_assert e ->
        pp f "@[<hov2>assert@ %a@]" (simple_expr ctxt) e
    | Pexp_lazy (e) ->
        pp f "@[<hov2>lazy@ %a@]" (simple_expr ctxt) e
    (* Pexp_poly: impossible but we should print it anyway, rather than
       assert false *)
    | Pexp_poly (e, None) ->
        pp f "@[<hov2>!poly!@ %a@]" (simple_expr ctxt) e
    | Pexp_poly (e, Some ct) ->
        pp f "@[<hov2>(!poly!@ %a@ : %a)@]"
          (simple_expr ctxt) e (core_type ctxt) ct
    | Pexp_open (o, e) ->
        pp f "@[<2>let open%s %a in@;%a@]"
          (override_vala o.popen_override) (module_expr ctxt) o.popen_expr
          (expression ctxt) e
    | Pexp_variant (l,Ploc.VaVal (Some eo)) ->
        pp f "@[<2>`%a@;%a@]" ident_of_name (unvala l) (simple_expr ctxt) eo
    | Pexp_letop {let_; ands; body} ->
        pp f "@[<2>@[<v>%a@,%a@] in@;<1 -2>%a@]"
          (binding_op ctxt) (unvala let_)
          (list ~sep:"@," (binding_op ctxt)) (unvala ands)
          (expression ctxt) body
    | Pexp_extension e -> extension ctxt f e
    | Pexp_unreachable -> pp f "."
    | _ -> expression1 ctxt f x

and expression1 ctxt f x =
  if unvala x.pexp_attributes <> [] then expression ctxt f x
  else match x.pexp_desc with
    | Pexp_object cs -> pp f "%a" (class_structure ctxt) cs
    | _ -> expression2 ctxt f x
(* used in [Pexp_apply] *)

and expression2 ctxt f x =
  if unvala x.pexp_attributes <> [] then expression ctxt f x
  else match x.pexp_desc with
    | Pexp_field (e, li) ->
        pp f "@[<hov2>%a.%a@]" (simple_expr ctxt) e longident_vala_loc li
    | Pexp_send (e, s) ->
        pp f "@[<hov2>%a#%a@]" (simple_expr ctxt) e ident_of_name (unvala s.txt)

    | _ -> simple_expr ctxt f x

and simple_expr ctxt f x =
  if unvala x.pexp_attributes <> [] then expression ctxt f x
  else match x.pexp_desc with
    | Pexp_construct _  when is_simple_construct (view_expr x) ->
        (match view_expr x with
         | `nil -> pp f "[]"
         | `tuple -> pp f "()"
         | `btrue -> pp f "true"
         | `bfalse -> pp f "false"
         | `list xs ->
             pp f "@[<hv0>[%a]@]"
               (list (expression (under_semi ctxt)) ~sep:";@;") xs
         | `simple x -> longident f (unvala x)
         | _ -> assert false)
    | Pexp_ident li ->
        longident_vala_loc f li
    (* (match view_fixity_of_exp x with *)
    (* |`Normal -> longident_loc f li *)
    (* | `Prefix _ | `Infix _ -> pp f "( %a )" longident_loc li) *)
    | Pexp_constant c -> constant f (unvala c);
    | Pexp_pack me ->
        pp f "(module@;%a)" (module_expr ctxt) me
    | Pexp_tuple l ->
        pp f "@[<hov2>(%a)@]" (list (simple_expr ctxt) ~sep:",@;") (unvala l)
    | Pexp_constraint (e, ct) ->
        pp f "(%a : %a)" (expression ctxt) e (core_type ctxt) ct
    | Pexp_coerce (e, cto1, ct) ->
        pp f "(%a%a :> %a)" (expression ctxt) e
          (option (core_type ctxt) ~first:" : " ~last:" ") (unvala cto1) (* no sep hint*)
          (core_type ctxt) ct
    | Pexp_variant (l, Ploc.VaVal None) -> pp f "`%a" ident_of_name (unvala l)
    | Pexp_record (l, eo) ->
        let longident_x_expression f ( li, e) =
          match e with
          |  {pexp_desc=Pexp_ident {txt;_};
              pexp_attributes=Ploc.VaVal []; _} when li.txt = txt ->
              pp f "@[<hov2>%a@]" longident_vala_loc li
          | _ ->
              pp f "@[<hov2>%a@;=@;%a@]" longident_vala_loc li (simple_expr ctxt) e
        in
        pp f "@[<hv0>@[<hv2>{@;%a%a@]@;}@]"(* "@[<hov2>{%a%a}@]" *)
          (option ~last:" with@;" (simple_expr ctxt)) (unvala eo)
          (list longident_x_expression ~sep:";@;") (unvala l)
    | Pexp_array (l) ->
        pp f "@[<0>@[<2>[|%a|]@]@]"
          (list (simple_expr (under_semi ctxt)) ~sep:";") (unvala l)
    | Pexp_while (e1, e2) ->
        let fmt : (_,_,_) format = "@[<2>while@;%a@;do@;%a@;done@]" in
        pp f fmt (expression ctxt) e1 (expression ctxt) e2
    | Pexp_for (s, e1, e2, df, e3) ->
        let fmt:(_,_,_)format =
          "@[<hv0>@[<hv2>@[<2>for %a =@;%a@;%a%a@;do@]@;%a@]@;done@]" in
        let expression = expression ctxt in
        pp f fmt (pattern ctxt) s expression e1 direction_flag
          (unvala df) expression e2 expression e3
    | _ ->  paren true (expression ctxt) f x

and attributes ctxt f l =
  List.iter (attribute ctxt f) (unvala l)

and item_attributes ctxt f l =
  List.iter (item_attribute ctxt f) (unvala l)

and attribute ctxt f a =
  pp f "@[<2>[@@%s@ %a]@]" (unvala a.attr_name.txt) (payload ctxt) a.attr_payload

and item_attribute ctxt f a =
  pp f "@[<2>[@@@@%s@ %a]@]" (unvala a.attr_name.txt) (payload ctxt) a.attr_payload

and floating_attribute ctxt f a =
  pp f "@[<2>[@@@@@@%s@ %a]@]" (unvala a.attr_name.txt) (payload ctxt) a.attr_payload

and value_description ctxt f x =
  (* note: value_description has an attribute field,
           but they're already printed by the callers this method *)
  pp f "@[<hov2>%a%a@]" (core_type ctxt) x.pval_type
    (fun f x ->
       if unvala x.pval_prim <> []
       then pp f "@ =@ %a" (list constant_string) (unvala x.pval_prim)
    ) x

and extension ctxt f (s, e) =
  pp f "@[<2>[%%%s@ %a]@]" (unvala s.txt) (payload ctxt) e

and item_extension ctxt f (s, e) =
  pp f "@[<2>[%%%%%s@ %a]@]" (unvala s.txt) (payload ctxt) e

and exception_declaration ctxt f x =
  pp f "@[<hov2>exception@ %a@]%a"
    (extension_constructor ctxt) x.ptyexn_constructor
    (item_attributes ctxt) x.ptyexn_attributes

and class_type_field ctxt f x =
  match x.pctf_desc with
  | Pctf_inherit (ct) ->
      pp f "@[<2>inherit@ %a@]%a" (class_type ctxt) ct
        (item_attributes ctxt) x.pctf_attributes
  | Pctf_val (s, mf, vf, ct) ->
      pp f "@[<2>val @ %a%a%a@ :@ %a@]%a"
        mutable_flag_vala mf virtual_flag_vala vf
        ident_of_name (unvala s.txt) (core_type ctxt) ct
        (item_attributes ctxt) x.pctf_attributes
  | Pctf_method (s, pf, vf, ct) ->
      pp f "@[<2>method %a %a%a :@;%a@]%a"
        private_flag_vala pf virtual_flag_vala vf
        ident_of_name (unvala s.txt) (core_type ctxt) ct
        (item_attributes ctxt) x.pctf_attributes
  | Pctf_constraint (ct1, ct2) ->
      pp f "@[<2>constraint@ %a@ =@ %a@]%a"
        (core_type ctxt) ct1 (core_type ctxt) ct2
        (item_attributes ctxt) x.pctf_attributes
  | Pctf_attribute a -> floating_attribute ctxt f a
  | Pctf_extension e ->
      item_extension ctxt f e;
      item_attributes ctxt f x.pctf_attributes

and class_signature ctxt f { pcsig_self = ct; pcsig_fields = l ;_} =
  pp f "@[<hv0>@[<hv2>object@[<1>%a@]@ %a@]@ end@]"
    (fun f -> function
         {ptyp_desc=Ptyp_any; ptyp_attributes= Ploc.VaVal []; _} -> ()
       | ct -> pp f " (%a)" (core_type ctxt) ct) ct
    (list (class_type_field ctxt) ~sep:"@;") (unvala l)

(* call [class_signature] called by [class_signature] *)
and class_type ctxt f x =
  match x.pcty_desc with
  | Pcty_signature cs ->
      class_signature ctxt f cs;
      attributes ctxt f x.pcty_attributes
  | Pcty_constr (li, l) ->
      pp f "%a%a%a"
        (fun f l -> match l with
           | [] -> ()
           | _  -> pp f "[%a]@ " (list (core_type ctxt) ~sep:"," ) l) (unvala l)
        longident_loc li
        (attributes ctxt) x.pcty_attributes
  | Pcty_arrow (l, co, cl) ->
      pp f "@[<2>%a@;->@;%a@]" (* FIXME remove parens later *)
        (type_with_label ctxt) (unvala l,co)
        (class_type ctxt) cl
  | Pcty_extension e ->
      extension ctxt f e;
      attributes ctxt f x.pcty_attributes
  | Pcty_open (o, e) ->
      pp f "@[<2>let open%s %a in@;%a@]"
        (override_vala o.popen_override) longident_vala_loc o.popen_expr
        (class_type ctxt) e

(* [class type a = object end] *)
and class_type_declaration_list ctxt f l =
  let class_type_declaration kwd f x =
    let { pci_params=ls; pci_name={ txt; _ }; _ } = x in
    pp f "@[<2>%s %a%a%a@ =@ %a@]%a" kwd
      virtual_flag (unvala x.pci_virt)
      (class_params_def ctxt) (unvala ls)
      ident_of_name (unvala txt)
      (class_type ctxt) x.pci_expr
      (item_attributes ctxt) x.pci_attributes
  in
  match l with
  | [] -> ()
  | [x] -> class_type_declaration "class type" f x
  | x :: xs ->
      pp f "@[<v>%a@,%a@]"
        (class_type_declaration "class type") x
        (list ~sep:"@," (class_type_declaration "and")) xs

and class_field ctxt f x =
  match x.pcf_desc with
  | Pcf_inherit (ovf, ce, so) ->
      pp f "@[<2>inherit@ %s@ %a%a@]%a" (override_vala ovf)
        (class_expr ctxt) ce
        (fun f so -> match so with
           | None -> ();
           | Some (s) -> pp f "@ as %a" ident_of_name (unvala s.txt) ) (unvala so)
        (item_attributes ctxt) x.pcf_attributes
  | Pcf_val (s, mf, Cfk_concrete (ovf, e)) ->
      pp f "@[<2>val%s %a%a =@;%a@]%a" (override_vala ovf)
        mutable_flag (unvala mf)
        ident_of_name (unvala s.txt)
        (expression ctxt) e
        (item_attributes ctxt) x.pcf_attributes
  | Pcf_method (s, pf, Cfk_virtual ct) ->
      pp f "@[<2>method virtual %a %a :@;%a@]%a"
        private_flag (unvala pf)
        ident_of_name (unvala s.txt)
        (core_type ctxt) ct
        (item_attributes ctxt) x.pcf_attributes
  | Pcf_val (s, mf, Cfk_virtual ct) ->
      pp f "@[<2>val virtual %a%a :@ %a@]%a"
        mutable_flag (unvala mf)
        ident_of_name (unvala s.txt)
        (core_type ctxt) ct
        (item_attributes ctxt) x.pcf_attributes
  | Pcf_method (s, pf, Cfk_concrete (ovf, e)) ->
      let bind e =
        binding ctxt f
          {pvb_pat=
             {ppat_desc=Ppat_var s;
              ppat_loc=Location.none;
              ppat_loc_stack=[];
              ppat_attributes=vaval []};
           pvb_expr=e;
           pvb_constraint=None;
           pvb_attributes=vaval [];
           pvb_loc=Location.none;
          }
      in
      pp f "@[<2>method%s %a%a@]%a"
        (override_vala ovf)
        private_flag (unvala pf)
        (fun f -> function
           | {pexp_desc=Pexp_poly (e, Some ct); pexp_attributes=Ploc.VaVal []; _} ->
               pp f "%a :@;%a=@;%a"
                 ident_of_name (unvala s.txt) (core_type ctxt) ct (expression ctxt) e
           | {pexp_desc=Pexp_poly (e, None); pexp_attributes=Ploc.VaVal []; _} ->
               bind e
           | _ -> bind e) e
        (item_attributes ctxt) x.pcf_attributes
  | Pcf_constraint (ct1, ct2) ->
      pp f "@[<2>constraint %a =@;%a@]%a"
        (core_type ctxt) ct1
        (core_type ctxt) ct2
        (item_attributes ctxt) x.pcf_attributes
  | Pcf_initializer (e) ->
      pp f "@[<2>initializer@ %a@]%a"
        (expression ctxt) e
        (item_attributes ctxt) x.pcf_attributes
  | Pcf_attribute a -> floating_attribute ctxt f a
  | Pcf_extension e ->
      item_extension ctxt f e;
      item_attributes ctxt f x.pcf_attributes

and class_structure ctxt f { pcstr_self = p; pcstr_fields =  l } =
  pp f "@[<hv0>@[<hv2>object%a@;%a@]@;end@]"
    (fun f p -> match p.ppat_desc with
       | Ppat_any -> ()
       | Ppat_constraint _ -> pp f " %a" (pattern ctxt) p
       | _ -> pp f " (%a)" (pattern ctxt) p) (unvala p)
    (list (class_field ctxt)) (unvala l)

and class_expr ctxt f x =
  if unvala x.pcl_attributes <> [] then begin
    pp f "((%a)%a)" (class_expr ctxt) {x with pcl_attributes= vaval []}
      (attributes ctxt) x.pcl_attributes
  end else
    match x.pcl_desc with
    | Pcl_structure (cs) -> class_structure ctxt f cs
    | Pcl_fun (l, eo, p, e) ->
        pp f "fun@ %a@ ->@ %a"
          (label_exp ctxt) (unvala l,unvala eo,p)
          (class_expr ctxt) e
    | Pcl_let (rf, l, ce) ->
        pp f "%a@ in@ %a"
          (bindings ctxt) (rf,l)
          (class_expr ctxt) ce
    | Pcl_apply (ce, l) ->
        pp f "((%a)@ %a)" (* Cf: #7200 *)
          (class_expr ctxt) ce
          (list (label_x_expression_param ctxt)) (unvala l)
    | Pcl_constr (li, l) ->
        pp f "%a%a"
          (fun f l-> if l <>[] then
              pp f "[%a]@ "
                (list (core_type ctxt) ~sep:",") l) (unvala l)
          longident_loc li
    | Pcl_constraint (ce, ct) ->
        pp f "(%a@ :@ %a)"
          (class_expr ctxt) ce
          (class_type ctxt) ct
    | Pcl_extension e -> extension ctxt f e
    | Pcl_open (o, e) ->
        pp f "@[<2>let open%s %a in@;%a@]"
          (override_vala o.popen_override) longident_vala_loc o.popen_expr
          (class_expr ctxt) e

and module_type ctxt f x =
  if unvala x.pmty_attributes <> [] then begin
    pp f "((%a)%a)" (module_type ctxt) {x with pmty_attributes=vaval []}
      (attributes ctxt) x.pmty_attributes
  end else
    match x.pmty_desc with
    | Pmty_functor (Ploc.VaVal Unit, mt2) ->
        pp f "@[<hov2>() ->@ %a@]" (module_type ctxt) mt2
    | Pmty_functor (Ploc.VaVal (Named (s, mt1)), mt2) ->
        begin match unvala s.txt with
        | None ->
            pp f "@[<hov2>%a@ ->@ %a@]"
              (module_type1 ctxt) mt1 (module_type ctxt) mt2
        | Some name ->
            pp f "@[<hov2>functor@ (%s@ :@ %a)@ ->@ %a@]" (unvala name)
              (module_type ctxt) mt1 (module_type ctxt) mt2
        end
    | Pmty_with (mt, Ploc.VaVal []) -> module_type ctxt f mt
    | Pmty_with (mt, l) ->
        pp f "@[<hov2>%a@ with@ %a@]"
          (module_type1 ctxt) mt
          (list (with_constraint ctxt) ~sep:"@ and@ ") (unvala l)
    | _ -> module_type1 ctxt f x

and with_constraint ctxt f = function
  | Pwith_type (li, (Ploc.VaVal ({ptype_params= ls ;_} as td))) ->
      pp f "type@ %a %a =@ %a"
        (type_params ctxt) ls
        longident_vala_loc li (type_declaration ctxt) td
  | Pwith_module (li, li2) ->
      pp f "module %a =@ %a" longident_vala_loc li longident_vala_loc li2;
  | Pwith_modtype (li, mty) ->
      pp f "module type %a =@ %a" longident_loc li (module_type ctxt) mty;
  | Pwith_typesubst (li, (Ploc.VaVal ({ptype_params=ls;_} as td))) ->
      pp f "type@ %a %a :=@ %a"
        (type_params ctxt) ls
        longident_vala_loc li
        (type_declaration ctxt) td
  | Pwith_modsubst (li, li2) ->
      pp f "module %a :=@ %a" longident_vala_loc li longident_vala_loc li2
  | Pwith_modtypesubst (li, mty) ->
      pp f "module type %a :=@ %a" longident_loc li (module_type ctxt) mty;


and module_type1 ctxt f x =
  if unvala x.pmty_attributes <> [] then module_type ctxt f x
  else match x.pmty_desc with
    | Pmty_ident li ->
        pp f "%a" longident_loc li;
    | Pmty_alias li ->
        pp f "(module %a)" longident_vala_loc li;
    | Pmty_signature (s) ->
        pp f "@[<hv0>@[<hv2>sig@ %a@]@ end@]" (* "@[<hov>sig@ %a@ end@]" *)
          (list (signature_item ctxt)) (unvala s) (* FIXME wrong indentation*)
    | Pmty_typeof me ->
        pp f "@[<hov2>module@ type@ of@ %a@]" (module_expr ctxt) me
    | Pmty_extension e -> extension ctxt f e
    | _ -> paren true (module_type ctxt) f x

and signature ctxt f x =  list ~sep:"@\n" (signature_item ctxt) f (unvala x)

and signature_item ctxt f x : unit =
  match x.psig_desc with
  | Psig_type (rf, Ploc.VaVal l) ->
      type_def_list ctxt f (rf, true, l)
  | Psig_typesubst (Ploc.VaVal l) ->
      (* Psig_typesubst is never recursive, but we specify [Recursive] here to
         avoid printing a [nonrec] flag, which would be rejected by the parser.
      *)
      type_def_list ctxt f (vaval Recursive, false, l)
  | Psig_value vd ->
      let intro = if unvala vd.pval_prim = [] then "val" else "external" in
      pp f "@[<2>%s@ %a@ :@ %a@]%a" intro
        ident_of_name (unvala vd.pval_name.txt)
        (value_description ctxt) vd
        (item_attributes ctxt) vd.pval_attributes
  | Psig_typext te ->
      type_extension ctxt f te
  | Psig_exception ed ->
      exception_declaration ctxt f ed
  | Psig_class l ->
      let class_description kwd f ({pci_params=ls;pci_name={txt;_};_} as x) =
        pp f "@[<2>%s %a%a%a@;:@;%a@]%a" kwd
          virtual_flag (unvala x.pci_virt)
          (class_params_def ctxt) (unvala ls)
          ident_of_name (unvala txt)
          (class_type ctxt) x.pci_expr
          (item_attributes ctxt) x.pci_attributes
      in begin
        match unvala l with
        | [] -> ()
        | [x] -> class_description "class" f x
        | x :: xs ->
            pp f "@[<v>%a@,%a@]"
              (class_description "class") x
              (list ~sep:"@," (class_description "and")) xs
      end
  | Psig_module ({pmd_type={pmty_desc=Pmty_alias alias;
                            pmty_attributes= Ploc.VaVal []; _};_} as pmd) ->
      pp f "@[<hov>module@ %s@ =@ %a@]%a"
        (unvala (Option.value (unvala pmd.pmd_name.txt) ~default:(vaval "_")))
        longident_vala_loc alias
        (item_attributes ctxt) pmd.pmd_attributes
  | Psig_module pmd ->
      pp f "@[<hov>module@ %s@ :@ %a@]%a"
        (unvala (Option.value (unvala pmd.pmd_name.txt) ~default:(vaval "_")))
        (module_type ctxt) pmd.pmd_type
        (item_attributes ctxt) pmd.pmd_attributes
  | Psig_modsubst pms ->
      pp f "@[<hov>module@ %s@ :=@ %a@]%a" (unvala pms.pms_name.txt)
        longident_vala_loc pms.pms_manifest
        (item_attributes ctxt) pms.pms_attributes
  | Psig_open od ->
      pp f "@[<hov2>open%s@ %a@]%a"
        (override_vala od.popen_override)
        longident_vala_loc od.popen_expr
        (item_attributes ctxt) od.popen_attributes
  | Psig_include incl ->
      pp f "@[<hov2>include@ %a@]%a"
        (module_type ctxt) incl.pincl_mod
        (item_attributes ctxt) incl.pincl_attributes
  | Psig_modtype {pmtd_name=s; pmtd_type=md; pmtd_attributes=attrs} ->
      pp f "@[<hov2>module@ type@ %s%a@]%a"
        (unvala s.txt)
        (fun f md -> match md with
           | None -> ()
           | Some mt ->
               pp_print_space f () ;
               pp f "@ =@ %a" (module_type ctxt) mt
        ) (unvala md)
        (item_attributes ctxt) attrs
  | Psig_modtypesubst {pmtd_name=s; pmtd_type=md; pmtd_attributes=attrs} ->
      let md = match unvala md with
        | None -> assert false (* ast invariant *)
        | Some mt -> mt in
      pp f "@[<hov2>module@ type@ %s@ :=@ %a@]%a"
        (unvala s.txt) (module_type ctxt) md
        (item_attributes ctxt) attrs
  | Psig_class_type (l) -> class_type_declaration_list ctxt f (unvala l)
  | Psig_recmodule decls ->
      let rec  string_x_module_type_list f ?(first=true) l =
        match l with
        | [] -> () ;
        | pmd :: tl ->
            if not first then
              pp f "@ @[<hov2>and@ %s:@ %a@]%a"
                (unvala (Option.value (unvala pmd.pmd_name.txt) ~default:(vaval "_")))
                (module_type1 ctxt) pmd.pmd_type
                (item_attributes ctxt) pmd.pmd_attributes
            else
              pp f "@[<hov2>module@ rec@ %s:@ %a@]%a"
                (unvala (Option.value (unvala pmd.pmd_name.txt) ~default:(vaval "_")))
                (module_type1 ctxt) pmd.pmd_type
                (item_attributes ctxt) pmd.pmd_attributes;
            string_x_module_type_list f ~first:false tl
      in
      string_x_module_type_list f (unvala decls)
  | Psig_attribute a -> floating_attribute ctxt f a
  | Psig_extension(e, a) ->
      item_extension ctxt f e;
      item_attributes ctxt f a

and module_expr ctxt f x =
  if unvala x.pmod_attributes <> [] then
    pp f "((%a)%a)" (module_expr ctxt) {x with pmod_attributes= vaval []}
      (attributes ctxt) x.pmod_attributes
  else match x.pmod_desc with
    | Pmod_structure (s) ->
        pp f "@[<hv2>struct@;@[<0>%a@]@;<1 -2>end@]"
          (list (structure_item ctxt) ~sep:"@\n") (unvala s);
    | Pmod_constraint (me, mt) ->
        pp f "@[<hov2>(%a@ :@ %a)@]"
          (module_expr ctxt) me
          (module_type ctxt) mt
    | Pmod_ident (li) ->
        pp f "%a" longident_vala_loc li;
    | Pmod_functor (Ploc.VaVal Unit, me) ->
        pp f "functor ()@;->@;%a" (module_expr ctxt) me
    | Pmod_functor (Ploc.VaVal (Named (s, mt)), me) ->
        pp f "functor@ (%s@ :@ %a)@;->@;%a"
          (unvala (Option.value (unvala s.txt) ~default:(vaval "_")))
          (module_type ctxt) mt (module_expr ctxt) me
    | Pmod_apply (me1, me2) ->
        pp f "(%a)(%a)" (module_expr ctxt) me1 (module_expr ctxt) me2
        (* Cf: #7200 *)
    | Pmod_apply_unit me1 ->
        pp f "(%a)()" (module_expr ctxt) me1
    | Pmod_unpack e ->
        pp f "(val@ %a)" (expression ctxt) e
    | Pmod_extension e -> extension ctxt f e

and structure ctxt f x = list ~sep:"@\n" (structure_item ctxt) f (unvala x)

and payload ctxt f = function
  | PStr (Ploc.VaVal [{pstr_desc = Pstr_eval (e, attrs)}]) ->
      pp f "@[<2>%a@]%a"
        (expression ctxt) (unvala e)
        (item_attributes ctxt) attrs
  | PStr x -> structure ctxt f x
  | PTyp x -> pp f ":@ "; core_type ctxt f x
  | PSig x -> pp f ":@ "; signature ctxt f x
  | PPat (x, Ploc.VaVal None) -> pp f "?@ "; pattern ctxt f x
  | PPat (x, Ploc.VaVal (Some e)) ->
      pp f "?@ "; pattern ctxt f x;
      pp f " when "; expression ctxt f e

(* transform [f = fun g h -> ..] to [f g h = ... ] could be improved *)
and binding ctxt f {pvb_pat=p; pvb_expr=x; pvb_constraint = ct; _} =
  (* .pvb_attributes have already been printed by the caller, #bindings *)
  let rec pp_print_pexp_function f x =
    if unvala x.pexp_attributes <> [] then pp f "=@;%a" (expression ctxt) x
    else match x.pexp_desc with
      | Pexp_function (params, c, body) ->
          function_params_then_body ctxt f (unvala params) (Option.map unvala (unvala c)) body ~delimiter:"="
      | Pexp_newtype (str,e) ->
          pp f "(type@ %a)@ %a" ident_of_name (unvala str.txt) pp_print_pexp_function e
      | _ -> pp f "=@;%a" (expression ctxt) x
  in
  match ct with
  | Some (Pvc_constraint { locally_abstract_univars = Ploc.VaVal []; typ }) ->
      pp f "%a@;:@;%a@;=@;%a"
        (simple_pattern ctxt) p (core_type ctxt) typ (expression ctxt) x
  | Some (Pvc_constraint { locally_abstract_univars = Ploc.VaVal vars; typ }) ->
      pp f "%a@;: type@;%a.@;%a@;=@;%a"
        (simple_pattern ctxt) p (list pp_print_string ~sep:"@;")
        (List.map (fun x -> unvala x.txt) vars)
        (core_type ctxt) typ (expression ctxt) x
  | Some (Pvc_coercion {ground=None; coercion }) ->
      pp f "%a@;:>@;%a@;=@;%a"
        (simple_pattern ctxt) p (core_type ctxt) coercion (expression ctxt) x
  | Some (Pvc_coercion {ground=Some ground; coercion }) ->
      pp f "%a@;:%a@;:>@;%a@;=@;%a"
        (simple_pattern ctxt) p
        (core_type ctxt) ground
        (core_type ctxt) coercion
        (expression ctxt) x
  | None -> begin
      match p with
      | {ppat_desc=Ppat_var _; ppat_attributes= Ploc.VaVal []} ->
          pp f "%a@ %a" (simple_pattern ctxt) p pp_print_pexp_function x
      | _ ->
          pp f "%a@;=@;%a" (pattern ctxt) p (expression ctxt) x
    end

(* [in] is not printed *)
and bindings ctxt f (rf,l) =
  let binding kwd rf f x =
    pp f "@[<2>%s %a%a@]%a" kwd rec_flag rf
      (binding ctxt) x (item_attributes ctxt) x.pvb_attributes
  in
  match unvala l with
  | [] -> ()
  | [x] -> binding "let" (unvala rf) f x
  | x::xs ->
      pp f "@[<v>%a@,%a@]"
        (binding "let" (unvala rf)) x
        (list ~sep:"@," (binding "and" Nonrecursive)) xs

and binding_op ctxt f x =
  match x.pbop_pat, x.pbop_exp with
  | {ppat_desc = Ppat_var { txt=pvar; _ }; ppat_attributes = Ploc.VaVal []; _},
    {pexp_desc = Pexp_ident { txt=Ploc.VaVal (Lident evar); _}; pexp_attributes = Ploc.VaVal []; _}
       when pvar = evar ->
     pp f "@[<2>%s %s@]" (unvala x.pbop_op.txt) (unvala evar)
  | pat, exp ->
     pp f "@[<2>%s %a@;=@;%a@]"
       (unvala x.pbop_op.txt) (pattern ctxt) pat (expression ctxt) exp

and structure_item ctxt f x =
  match x.pstr_desc with
  | Pstr_eval (e, attrs) ->
      pp f "@[<hov2>;;%a@]%a"
        (expression ctxt) (unvala e)
        (item_attributes ctxt) attrs
  | Pstr_type (_, Ploc.VaVal []) -> assert false
  | Pstr_type (rf, Ploc.VaVal l)  -> type_def_list ctxt f (rf, true, l)
  | Pstr_value (rf, l) ->
      (* pp f "@[<hov2>let %a%a@]"  rec_flag rf bindings l *)
      pp f "@[<2>%a@]" (bindings ctxt) (rf,l)
  | Pstr_typext te -> type_extension ctxt f te
  | Pstr_exception ed -> exception_declaration ctxt f ed
  | Pstr_module x ->
      let rec module_helper = function
        | {pmod_desc=Pmod_functor(arg_opt,me'); pmod_attributes = Ploc.VaVal []} ->
            begin match arg_opt with
            | Ploc.VaVal Unit -> pp f "()"
            | Ploc.VaVal (Named (s, mt)) ->
              pp f "(%s:%a)" (unvala (Option.value (unvala s.txt) ~default:(vaval"_")))
                (module_type ctxt) mt
            end;
            module_helper me'
        | me -> me
      in
      pp f "@[<hov2>module %s%a@]%a"
        (unvala (Option.value (unvala x.pmb_name.txt) ~default:(vaval "_")))
        (fun f me ->
           let me = module_helper me in
           match me with
           | {pmod_desc=
                Pmod_constraint
                  (me',
                   ({pmty_desc=(Pmty_ident (_)
                               | Pmty_signature (_));_} as mt));
              pmod_attributes = Ploc.VaVal []} ->
               pp f " :@;%a@;=@;%a@;"
                 (module_type ctxt) mt (module_expr ctxt) me'
           | _ -> pp f " =@ %a" (module_expr ctxt) me
        ) x.pmb_expr
        (item_attributes ctxt) x.pmb_attributes
  | Pstr_open od ->
      pp f "@[<2>open%s@;%a@]%a"
        (override_vala od.popen_override)
        (module_expr ctxt) od.popen_expr
        (item_attributes ctxt) od.popen_attributes
  | Pstr_modtype {pmtd_name=s; pmtd_type=md; pmtd_attributes=attrs} ->
      pp f "@[<hov2>module@ type@ %s%a@]%a"
        (unvala s.txt)
        (fun f md -> match md with
           | None -> ()
           | Some mt ->
               pp_print_space f () ;
               pp f "@ =@ %a" (module_type ctxt) mt
        ) (unvala md)
        (item_attributes ctxt) attrs
  | Pstr_class l ->
      let extract_class_args cl =
        let rec loop acc = function
          | {pcl_desc=Pcl_fun (l, eo, p, cl'); pcl_attributes = Ploc.VaVal []} ->
              loop ((unvala l,unvala eo,p) :: acc) cl'
          | cl -> List.rev acc, cl
        in
        let args, cl = loop [] cl in
        let constr, cl =
          match cl with
          | {pcl_desc=Pcl_constraint (cl', ct); pcl_attributes = Ploc.VaVal []} ->
              Some ct, cl'
          | _ -> None, cl
        in
        args, constr, cl
      in
      let class_constraint f ct = pp f ": @[%a@] " (class_type ctxt) ct in
      let class_declaration kwd f
          ({pci_params=ls; pci_name={txt;_}; _} as x) =
        let args, constr, cl = extract_class_args x.pci_expr in
        pp f "@[<2>%s %a%a%a %a%a=@;%a@]%a" kwd
          virtual_flag (unvala x.pci_virt)
          (class_params_def ctxt) (unvala ls)
          ident_of_name (unvala txt)
          (list (label_exp ctxt)) args
          (option class_constraint) constr
          (class_expr ctxt) cl
          (item_attributes ctxt) x.pci_attributes
      in begin
        match unvala l with
        | [] -> ()
        | [x] -> class_declaration "class" f x
        | x :: xs ->
            pp f "@[<v>%a@,%a@]"
              (class_declaration "class") x
              (list ~sep:"@," (class_declaration "and")) xs
      end
  | Pstr_class_type l -> class_type_declaration_list ctxt f (unvala l)
  | Pstr_primitive vd ->
      pp f "@[<hov2>external@ %a@ :@ %a@]%a"
        ident_of_name (unvala vd.pval_name.txt)
        (value_description ctxt) vd
        (item_attributes ctxt) vd.pval_attributes
  | Pstr_include incl ->
      pp f "@[<hov2>include@ %a@]%a"
        (module_expr ctxt) incl.pincl_mod
        (item_attributes ctxt) incl.pincl_attributes
  | Pstr_recmodule decls -> (* 3.07 *)
      let aux f = function
        | ({pmb_expr={pmod_desc=Pmod_constraint (expr, typ)}} as pmb) ->
            pp f "@[<hov2>@ and@ %s:%a@ =@ %a@]%a"
              (unvala (Option.value (unvala pmb.pmb_name.txt) ~default:(vaval "_")))
              (module_type ctxt) typ
              (module_expr ctxt) expr
              (item_attributes ctxt) pmb.pmb_attributes
        | pmb ->
            pp f "@[<hov2>@ and@ %s@ =@ %a@]%a"
              (unvala (Option.value (unvala pmb.pmb_name.txt) ~default:(vaval "_")))
              (module_expr ctxt) pmb.pmb_expr
              (item_attributes ctxt) pmb.pmb_attributes
      in
      begin match unvala decls with
      | ({pmb_expr={pmod_desc=Pmod_constraint (expr, typ)}} as pmb) :: l2 ->
          pp f "@[<hv>@[<hov2>module@ rec@ %s:%a@ =@ %a@]%a@ %a@]"
            (unvala (Option.value (unvala pmb.pmb_name.txt) ~default:(vaval "_")))
            (module_type ctxt) typ
            (module_expr ctxt) expr
            (item_attributes ctxt) pmb.pmb_attributes
            (fun f l2 -> List.iter (aux f) l2) l2
      | pmb :: l2 ->
          pp f "@[<hv>@[<hov2>module@ rec@ %s@ =@ %a@]%a@ %a@]"
            (unvala (Option.value (unvala pmb.pmb_name.txt) ~default:(vaval "_")))
            (module_expr ctxt) pmb.pmb_expr
            (item_attributes ctxt) pmb.pmb_attributes
            (fun f l2 -> List.iter (aux f) l2) l2
      | _ -> assert false
      end
  | Pstr_attribute a -> floating_attribute ctxt f a
  | Pstr_extension(e, a) ->
      item_extension ctxt f e;
      item_attributes ctxt f a

and type_param ctxt f (ct, (a,b)) =
  pp f "%s%s%a" (type_variance a) (type_injectivity b) (core_type ctxt) ct

and type_params ctxt f = function
  | Ploc.VaVal [] -> ()
  | Ploc.VaVal l -> pp f "%a " (list (type_param ctxt) ~first:"(" ~last:")" ~sep:",@;") l

and type_def_list ctxt f (rf, exported, l) =
  let type_decl kwd rf f x =
    let eq =
      if (x.ptype_kind = Ptype_abstract)
         && (unvala x.ptype_manifest = None) then ""
      else if exported then " ="
      else " :="
    in
    pp f "@[<2>%s %a%a%a%s%a@]%a" kwd
      nonrec_flag rf
      (type_params ctxt) x.ptype_params
      ident_of_name (unvala x.ptype_name.txt)
      eq
      (type_declaration ctxt) x
      (item_attributes ctxt) x.ptype_attributes
  in
  match l with
  | [] -> assert false
  | [x] -> type_decl "type" (unvala rf) f x
  | x :: xs -> pp f "@[<v>%a@,%a@]"
                 (type_decl "type" (unvala rf)) x
                 (list ~sep:"@," (type_decl "and" Recursive)) xs

and record_declaration ctxt f lbls =
  let type_record_field f pld =
    pp f "@[<2>%a%a:@;%a@;%a@]"
      mutable_flag (unvala pld.pld_mutable)
      ident_of_name (unvala pld.pld_name.txt)
      (core_type ctxt) (unvala pld.pld_type)
      (attributes ctxt) pld.pld_attributes
  in
  pp f "{@\n%a}"
    (list type_record_field ~sep:";@\n" )  lbls

and type_declaration ctxt f x =
  (* type_declaration has an attribute field,
     but it's been printed by the caller of this method *)
  let priv f =
    match unvala x.ptype_private with
    | Public -> ()
    | Private -> pp f "@;private"
  in
  let manifest f =
    match unvala x.ptype_manifest with
    | None -> ()
    | Some y ->
        if x.ptype_kind = Ptype_abstract then
          pp f "%t@;%a" priv (core_type ctxt) (unvala y)
        else
          pp f "@;%a" (core_type ctxt) (unvala y)
  in
  let constructor_declaration f pcd =
    pp f "|@;";
    constructor_declaration ctxt f
      (pcd.pcd_name.txt, pcd.pcd_vars,
       pcd.pcd_args, pcd.pcd_res, pcd.pcd_attributes)
  in
  let repr f =
    let intro f =
      if unvala x.ptype_manifest = None then ()
      else pp f "@;="
    in
    match x.ptype_kind with
    | Ptype_variant xs ->
      let variants fmt xs =
        if xs = [] then pp fmt " |" else
          pp fmt "@\n%a" (list ~sep:"@\n" constructor_declaration) xs
      in pp f "%t%t%a" intro priv variants (unvala xs)
    | Ptype_abstract -> ()
    | Ptype_record l ->
        pp f "%t%t@;%a" intro priv (record_declaration ctxt) (unvala l)
    | Ptype_open -> pp f "%t%t@;.." intro priv
  in
  let constraints f =
    List.iter
      (fun (ct1,ct2,_) ->
         pp f "@[<hov2>@ constraint@ %a@ =@ %a@]"
           (core_type ctxt) ct1 (core_type ctxt) ct2)
      (unvala x.ptype_cstrs)
  in
  pp f "%t%t%t" manifest repr constraints

and type_extension ctxt f x =
  let extension_constructor f x =
    pp f "@\n|@;%a" (extension_constructor ctxt) x
  in
  pp f "@[<2>type %a%a += %a@ %a@]%a"
    (fun f -> function
       | [] -> ()
       | l ->
           pp f "%a@;" (list (type_param ctxt) ~first:"(" ~last:")" ~sep:",") l)
    (unvala  x.ptyext_params)
    longident_vala_loc x.ptyext_path
    private_flag (unvala x.ptyext_private) (* Cf: #7200 *)
    (list ~sep:"" extension_constructor)
    (unvala x.ptyext_constructors)
    (item_attributes ctxt) x.ptyext_attributes

and constructor_declaration ctxt f (name, vars, args, res, attrs) =
  let name =
    match unvala name with
    | "::" -> "(::)"
    | s -> s in
  let pp_vars f vs =
    match vs with
    | [] -> ()
    | vs -> pp f "%a@;.@;" (list tyvar_loc ~sep:"@;") vs in
  match unvala res with
  | None ->
      pp f "%s%a@;%a" name
        (fun f -> function
           | Pcstr_tuple (Ploc.VaVal []) -> ()
           | Pcstr_tuple l ->
             pp f "@;of@;%a" (list (core_type1 ctxt) ~sep:"@;*@;") (unvala l)
           | Pcstr_record l -> pp f "@;of@;%a" (record_declaration ctxt) (unvala l)
        ) args
        (attributes ctxt) attrs
  | Some r ->
      pp f "%s:@;%a%a@;%a" name
        pp_vars (unvala vars)
        (fun f -> function
           | Pcstr_tuple (Ploc.VaVal []) -> core_type1 ctxt f r
           | Pcstr_tuple l -> pp f "%a@;->@;%a"
                                (list (core_type1 ctxt) ~sep:"@;*@;") (unvala l)
                                (core_type1 ctxt) r
           | Pcstr_record l ->
               pp f "%a@;->@;%a" (record_declaration ctxt) (unvala l) (core_type1 ctxt) r
        )
        args
        (attributes ctxt) attrs

and extension_constructor ctxt f x =
  (* Cf: #7200 *)
  match x.pext_kind with
  | Pext_decl(v, l, r) ->
      constructor_declaration ctxt f
        (x.pext_name.txt, v, l, r, x.pext_attributes)
  | Pext_rebind li ->
      pp f "%s@;=@;%a%a" (unvala x.pext_name.txt)
        longident_vala_loc li
        (attributes ctxt) x.pext_attributes

and case_list ctxt f l : unit =
  let aux f {pc_lhs; pc_guard; pc_rhs} =
    pp f "@;| @[<2>%a%a@;->@;%a@]"
      (pattern ctxt) (unvala pc_lhs) (option (expression ctxt) ~first:"@;when@;")
      (Option.map unvala (unvala pc_guard)) (expression (under_pipe ctxt)) (unvala pc_rhs)
  in
  list aux f l ~sep:""

and label_x_expression_param ctxt f (l,e) =
  let simple_name = match e with
    | {pexp_desc=Pexp_ident {txt=Ploc.VaVal (Lident (Ploc.VaVal l));_};
       pexp_attributes=Ploc.VaVal []} -> Some l
    | _ -> None
  in match l with
  | Nolabel  -> expression2 ctxt f e (* level 2*)
  | Optional (Ploc.VaVal str) ->
      if Some str = simple_name then
        pp f "?%a" ident_of_name str
      else
        pp f "?%a:%a" ident_of_name str (simple_expr ctxt) e
  | Labelled lbl ->
      if Some (unvala lbl) = simple_name then
        pp f "~%a" ident_of_name (unvala lbl)
      else
        pp f "~%a:%a" ident_of_name (unvala lbl) (simple_expr ctxt) e

and directive_argument f x =
  match x.pdira_desc with
  | Pdir_string (s) -> pp f "@ %S" s
  | Pdir_int (n, None) -> pp f "@ %s" n
  | Pdir_int (n, Some m) -> pp f "@ %s%c" n m
  | Pdir_ident (li) -> pp f "@ %a" longident li
  | Pdir_bool (b) -> pp f "@ %s" (string_of_bool b)

let toplevel_phrase f x =
  match x with
  | Ptop_def (s) ->pp f "@[<hov0>%a@]"  (list (structure_item reset_ctxt)) (unvala s)
   (* pp_open_hvbox f 0; *)
   (* pp_print_list structure_item f s ; *)
   (* pp_close_box f (); *)
  | Ptop_dir {pdir_name; pdir_arg = None; _} ->
   pp f "@[<hov2>#%s@]" pdir_name.txt
  | Ptop_dir {pdir_name; pdir_arg = Some pdir_arg; _} ->
   pp f "@[<hov2>#%s@ %a@]" pdir_name.txt directive_argument pdir_arg

let expression f x =
  pp f "@[%a@]" (expression reset_ctxt) x

let string_of_expression x =
  ignore (flush_str_formatter ()) ;
  let f = str_formatter in
  expression f x;
  flush_str_formatter ()

let string_of_structure x =
  ignore (flush_str_formatter ());
  let f = str_formatter in
  structure reset_ctxt f x;
  flush_str_formatter ()

let top_phrase f x =
  pp_print_newline f ();
  toplevel_phrase f x;
  pp f ";;";
  pp_print_newline f ()

let core_type = core_type reset_ctxt
let pattern = pattern reset_ctxt
let signature = signature reset_ctxt
let structure = structure reset_ctxt
let module_expr = module_expr reset_ctxt
let module_type = module_type reset_ctxt
let class_field = class_field reset_ctxt
let class_type_field = class_type_field reset_ctxt
let class_expr = class_expr reset_ctxt
let class_type = class_type reset_ctxt
let structure_item = structure_item reset_ctxt
let signature_item = signature_item reset_ctxt
let binding = binding reset_ctxt
let payload = payload reset_ctxt
