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

(** Abstract syntax tree produced by parsing

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

open Asttypes

type constant =
    Pconst_integer of string Ploc.vala * char option
  (* 3 3l 3L 3n

     Suffixes [g-z][G-Z] are accepted by the parser.
     Suffixes except 'l', 'L' and 'n' are rejected by the typechecker
  *)
  | Pconst_char of char Ploc.vala
  (* 'c' *)
  | Pconst_string of string Ploc.vala * Location.t * string Ploc.vala option
  (* "constant"
     {delim|other constant|delim}

     The location span the content of the string, without the delimiters.
  *)
  | Pconst_float of string Ploc.vala * char option
  (* 3.4 2e5 1.4e-4

     Suffixes [g-z][G-Z] are accepted by the parser.
     Suffixes are rejected by the typechecker.
  *)

type location_stack = Location.t list

(** {1 Extension points} *)

type attribute = {
    attr_name : string Ploc.vala loc;
    attr_payload : payload;
    attr_loc : Location.t;
  }
       (* [@id ARG]
          [@@id ARG]

          Metadata containers passed around within the AST.
          The compiler ignores unknown attributes.
       *)

and extension = string Ploc.vala loc * payload
      (* [%id ARG]
         [%%id ARG]

         Sub-language placeholder -- rejected by the typechecker.
      *)

and attributes = attribute list Ploc.vala

and payload =
  | PStr of structure
  | PSig of signature (* : SIG *)
  | PTyp of core_type  (* : T *)
  | PPat of pattern * expression option Ploc.vala  (* ? P  or  ? P when E *)

(** {1 Core language} *)

(* Type expressions *)

and core_type =
    {
     ptyp_desc: core_type_desc;
     ptyp_loc: Location.t;
     ptyp_loc_stack: location_stack;
     ptyp_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and core_type_desc =
(*-*)  | Ptyp_xtr of string loc
  | Ptyp_any
        (*  _ *)
  | Ptyp_var of string Ploc.vala
        (* 'a *)
  | Ptyp_arrow of arg_label Ploc.vala * core_type * core_type
        (* T1 -> T2       Simple
           ~l:T1 -> T2    Labelled
           ?l:T1 -> T2    Optional
         *)
  | Ptyp_tuple of core_type list Ploc.vala
        (* T1 * ... * Tn

           Invariant: n >= 2
        *)
  | Ptyp_constr of Longident.t loc * core_type list Ploc.vala
        (* tconstr
           T tconstr
           (T1, ..., Tn) tconstr
         *)
  | Ptyp_object of object_field list Ploc.vala * closed_flag Ploc.vala
        (* < l1:T1; ...; ln:Tn >     (flag = Closed)
           < l1:T1; ...; ln:Tn; .. > (flag = Open)
         *)
  | Ptyp_class of Longident.t loc * core_type list Ploc.vala
        (* #tconstr
           T #tconstr
           (T1, ..., Tn) #tconstr
         *)
  | Ptyp_alias of core_type * string Ploc.vala
        (* T as 'a *)
  | Ptyp_variant of row_field list Ploc.vala * closed_flag Ploc.vala * label list Ploc.vala option Ploc.vala
        (* [ `A|`B ]         (flag = Closed; labels = None)
           [> `A|`B ]        (flag = Open;   labels = None)
           [< `A|`B ]        (flag = Closed; labels = Some [])
           [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])
         *)
  | Ptyp_poly of string Ploc.vala loc list Ploc.vala * core_type
        (* 'a1 ... 'an. T

           Can only appear in the following context:

           - As the core_type of a Ppat_constraint node corresponding
             to a constraint on a let-binding: let x : 'a1 ... 'an. T
             = e ...

           - Under Cfk_virtual for methods (not values).

           - As the core_type of a Pctf_method node.

           - As the core_type of a Pexp_poly node.

           - As the pld_type field of a label_declaration.

           - As a core_type of a Ptyp_object node.
         *)

  | Ptyp_package of package_type
        (* (module S) *)
  | Ptyp_extension of extension
        (* [%id] *)

and package_type = Longident.t loc * (Longident.t loc * core_type) list Ploc.vala
      (*
        (module S)
        (module S with type t1 = T1 and ... and tn = Tn)
       *)

and row_field = {
  prf_desc : row_field_desc;
  prf_loc : Location.t;
  prf_attributes : attributes;
}

and row_field_desc =
  | Rtag of label Ploc.vala loc * bool Ploc.vala * core_type list Ploc.vala
        (* [`A]                   ( true,  [] )
           [`A of T]              ( false, [T] )
           [`A of T1 & .. & Tn]   ( false, [T1;...Tn] )
           [`A of & T1 & .. & Tn] ( true,  [T1;...Tn] )

          - The 'bool' field is true if the tag contains a
            constant (empty) constructor.
          - '&' occurs when several types are used for the same constructor
            (see 4.2 in the manual)
        *)
  | Rinherit of core_type
        (* [ | t ] *)

and object_field = {
  pof_desc : object_field_desc;
  pof_loc : Location.t;
  pof_attributes : attributes;
}

and object_field_desc =
  | Otag of label Ploc.vala loc * core_type
  | Oinherit of core_type

(* Patterns *)

and pattern =
    {
     ppat_desc: pattern_desc;
     ppat_loc: Location.t;
     ppat_loc_stack: location_stack;
     ppat_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and pattern_desc =
(*-*)  | Ppat_xtr of string loc
  | Ppat_any
        (* _ *)
  | Ppat_var of string Ploc.vala loc
        (* x *)
  | Ppat_alias of pattern * string Ploc.vala loc
        (* P as 'a *)
  | Ppat_constant of constant Ploc.vala
        (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Ppat_interval of constant Ploc.vala * constant Ploc.vala
        (* 'a'..'z'

           Other forms of interval are recognized by the parser
           but rejected by the type-checker. *)
  | Ppat_tuple of pattern list Ploc.vala
        (* (P1, ..., Pn)

           Invariant: n >= 2
        *)
  | Ppat_construct of
      Longident.t Ploc.vala loc * (string Ploc.vala loc list Ploc.vala * pattern) option Ploc.vala
        (* C                    None
           C P                  Some ([], P)
           C (P1, ..., Pn)      Some ([], Ppat_tuple [P1; ...; Pn])
           C (type a b) P       Some ([a; b], P)
         *)
  | Ppat_variant of label Ploc.vala * pattern option Ploc.vala
        (* `A             (None)
           `A P           (Some P)
         *)
  | Ppat_record of (Longident.t loc * pattern) list Ploc.vala * closed_flag Ploc.vala
        (* { l1=P1; ...; ln=Pn }     (flag = Closed)
           { l1=P1; ...; ln=Pn; _}   (flag = Open)

           Invariant: n > 0
         *)
  | Ppat_array of pattern list Ploc.vala
        (* [| P1; ...; Pn |] *)
  | Ppat_or of pattern * pattern
        (* P1 | P2 *)
  | Ppat_constraint of pattern * core_type
        (* (P : T) *)
  | Ppat_type of Longident.t loc
        (* #tconst *)
  | Ppat_lazy of pattern
        (* lazy P *)
  | Ppat_unpack of string Ploc.vala option Ploc.vala loc
        (* (module P)        Some "P"
           (module _)        None

           Note: (module P : S) is represented as
           Ppat_constraint(Ppat_unpack, Ptyp_package)
         *)
  | Ppat_exception of pattern
        (* exception P *)
  | Ppat_extension of extension
        (* [%id] *)
  | Ppat_open of Longident.t Ploc.vala loc * pattern
        (* M.(P) *)

(* Value expressions *)

and expression =
    {
     pexp_desc: expression_desc;
     pexp_loc: Location.t;
     pexp_loc_stack: location_stack;
     pexp_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and expression_desc =
(*-*)  | Pexp_xtr of string loc
  | Pexp_ident of Longident.t loc
        (* x
           M.x
         *)
  | Pexp_constant of constant Ploc.vala
        (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Pexp_let of rec_flag Ploc.vala * value_binding list Ploc.vala * expression
        (* let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
         *)
  | Pexp_function of case list Ploc.vala
        (* function P1 -> E1 | ... | Pn -> En *)
  | Pexp_fun of arg_label Ploc.vala * expression option Ploc.vala * pattern * expression
        (* fun P -> E1                          (Simple, None)
           fun ~l:P -> E1                       (Labelled l, None)
           fun ?l:P -> E1                       (Optional l, None)
           fun ?l:(P = E0) -> E1                (Optional l, Some E0)

           Notes:
           - If E0 is provided, only Optional is allowed.
           - "fun P1 P2 .. Pn -> E1" is represented as nested Pexp_fun.
           - "let f P = E" is represented using Pexp_fun.
         *)
  | Pexp_apply of expression * (arg_label * expression) list Ploc.vala
        (* E0 ~l1:E1 ... ~ln:En
           li can be empty (non labeled argument) or start with '?'
           (optional argument).

           Invariant: n > 0
         *)
  | Pexp_match of expression * case list Ploc.vala
        (* match E0 with P1 -> E1 | ... | Pn -> En *)
  | Pexp_try of expression * case list Ploc.vala
        (* try E0 with P1 -> E1 | ... | Pn -> En *)
  | Pexp_tuple of expression list Ploc.vala
        (* (E1, ..., En)

           Invariant: n >= 2
        *)
  | Pexp_construct of Longident.t Ploc.vala loc * expression option Ploc.vala
        (* C                None
           C E              Some E
           C (E1, ..., En)  Some (Pexp_tuple[E1;...;En])
        *)
  | Pexp_variant of label Ploc.vala * expression option Ploc.vala
        (* `A             (None)
           `A E           (Some E)
         *)
  | Pexp_record of (Longident.t loc * expression) list Ploc.vala * expression option Ploc.vala
        (* { l1=P1; ...; ln=Pn }     (None)
           { E0 with l1=P1; ...; ln=Pn }   (Some E0)

           Invariant: n > 0
         *)
  | Pexp_field of expression * Longident.t Ploc.vala loc
        (* E.l *)
  | Pexp_setfield of expression * Longident.t Ploc.vala loc * expression
        (* E1.l <- E2 *)
  | Pexp_array of expression list Ploc.vala
        (* [| E1; ...; En |] *)
  | Pexp_ifthenelse of expression * expression * expression option Ploc.vala
        (* if E1 then E2 else E3 *)
  | Pexp_sequence of expression * expression
        (* E1; E2 *)
  | Pexp_while of expression * expression
        (* while E1 do E2 done *)
  | Pexp_for of
      pattern *  expression * expression * direction_flag Ploc.vala * expression
        (* for i = E1 to E2 do E3 done      (flag = Upto)
           for i = E1 downto E2 do E3 done  (flag = Downto)
         *)
  | Pexp_constraint of expression * core_type
        (* (E : T) *)
  | Pexp_coerce of expression * core_type option Ploc.vala * core_type
        (* (E :> T)        (None, T)
           (E : T0 :> T)   (Some T0, T)
         *)
  | Pexp_send of expression * label Ploc.vala loc
        (*  E # m *)
  | Pexp_new of Longident.t Ploc.vala loc
        (* new M.c *)
  | Pexp_setinstvar of label Ploc.vala loc * expression
        (* x <- 2 *)
  | Pexp_override of (label Ploc.vala loc * expression) list Ploc.vala
        (* {< x1 = E1; ...; Xn = En >} *)
  | Pexp_letmodule of string Ploc.vala option Ploc.vala loc * module_expr * expression
        (* let module M = ME in E *)
  | Pexp_letexception of extension_constructor Ploc.vala * expression
        (* let exception C in E *)
  | Pexp_assert of expression
        (* assert E
           Note: "assert false" is treated in a special way by the
           type-checker. *)
  | Pexp_lazy of expression
        (* lazy E *)
  | Pexp_poly of expression * core_type option
        (* Used for method bodies.

           Can only be used as the expression under Cfk_concrete
           for methods (not values). *)
  | Pexp_object of class_structure
        (* object ... end *)
  | Pexp_newtype of string Ploc.vala loc * expression
        (* fun (type t) -> E *)
  | Pexp_pack of module_expr
        (* (module ME)

           (module ME : S) is represented as
           Pexp_constraint(Pexp_pack, Ptyp_package S) *)
  | Pexp_open of open_declaration * expression
        (* M.(E)
           let open M in E
           let! open M in E *)
  | Pexp_letop of letop
        (* let* P = E in E
           let* P = E and* P = E in E *)
  | Pexp_extension of extension
        (* [%id] *)
  | Pexp_unreachable
        (* . *)

and case =   (* (P -> E) or (P when E0 -> E) *)
    {
     pc_lhs: pattern  Ploc.vala;
     pc_guard: expression Ploc.vala option Ploc.vala;
     pc_rhs: expression Ploc.vala;
   }

and letop =
  {
    let_ : binding_op Ploc.vala;
    ands : binding_op list Ploc.vala;
    body : expression;
  }

and binding_op =
  {
    pbop_op : string Ploc.vala loc;
    pbop_pat : pattern;
    pbop_exp : expression;
    pbop_loc : Location.t;
  }

(* Value descriptions *)

and value_description =
    {
     pval_name: string Ploc.vala loc;
     pval_type: core_type;
     pval_prim: string list Ploc.vala;
     pval_attributes: attributes;  (* ... [@@id1] [@@id2] *)
     pval_loc: Location.t;
    }

(*
  val x: T                            (prim = [])
  external x: T = "s1" ... "sn"       (prim = ["s1";..."sn"])
*)

(* Type declarations *)

and type_declaration =
    {
     ptype_name: string Ploc.vala loc;
     ptype_params: (core_type * (variance * injectivity)) list Ploc.vala;
           (* ('a1,...'an) t; None represents  _*)
     ptype_cstrs: (core_type * core_type * Location.t) list Ploc.vala;
           (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
     ptype_kind: type_kind;
     ptype_private: private_flag Ploc.vala;   (* = private ... *)
     ptype_manifest: core_type Ploc.vala option Ploc.vala;  (* = T *)
     ptype_attributes: attributes;   (* ... [@@id1] [@@id2] *)
     ptype_loc: Location.t;
    }

(*
  type t                     (abstract, no manifest)
  type t = T0                (abstract, manifest=T0)
  type t = C of T | ...      (variant,  no manifest)
  type t = T0 = C of T | ... (variant,  manifest=T0)
  type t = {l: T; ...}       (record,   no manifest)
  type t = T0 = {l : T; ...} (record,   manifest=T0)
  type t = ..                (open,     no manifest)
*)

and type_kind =
  | Ptype_abstract
  | Ptype_variant of constructor_declaration list Ploc.vala
  | Ptype_record of label_declaration list Ploc.vala
        (* Invariant: non-empty list *)
  | Ptype_open

and label_declaration =
    {
     pld_name: string Ploc.vala loc;
     pld_mutable: mutable_flag Ploc.vala;
     pld_type: core_type Ploc.vala;
     pld_loc: Location.t;
     pld_attributes: attributes; (* l : T [@id1] [@id2] *)
    }

(*  { ...; l: T; ... }            (mutable=Immutable)
    { ...; mutable l: T; ... }    (mutable=Mutable)

    Note: T can be a Ptyp_poly.
*)

and constructor_declaration =
    {
     pcd_name: string Ploc.vala loc;
     pcd_args: constructor_arguments;
     pcd_res: core_type option Ploc.vala;
     pcd_loc: Location.t;
     pcd_attributes: attributes; (* C of ... [@id1] [@id2] *)
    }

and constructor_arguments =
  | Pcstr_tuple of core_type list Ploc.vala
  | Pcstr_record of label_declaration list Ploc.vala

(*
  | C of T1 * ... * Tn     (res = None,    args = Pcstr_tuple [])
  | C: T0                  (res = Some T0, args = [])
  | C: T1 * ... * Tn -> T0 (res = Some T0, args = Pcstr_tuple)
  | C of {...}             (res = None,    args = Pcstr_record)
  | C: {...} -> T0         (res = Some T0, args = Pcstr_record)
  | C of {...} as t        (res = None,    args = Pcstr_record)
*)

and type_extension =
    {
     ptyext_path: Longident.t loc;
     ptyext_params: (core_type * (variance * injectivity)) list Ploc.vala;
     ptyext_constructors: extension_constructor list Ploc.vala;
     ptyext_private: private_flag Ploc.vala;
     ptyext_loc: Location.t;
     ptyext_attributes: attributes;   (* ... [@@id1] [@@id2] *)
    }
(*
  type t += ...
*)

and extension_constructor =
    {
     pext_name: string Ploc.vala loc;
     pext_kind: extension_constructor_kind;
     pext_loc: Location.t;
     pext_attributes: attributes; (* C of ... [@id1] [@id2] *)
   }

(* exception E *)
and type_exception =
  {
    ptyexn_constructor : extension_constructor;
    ptyexn_loc : Location.t;
    ptyexn_attributes: attributes; (* ... [@@id1] [@@id2] *)
  }

and extension_constructor_kind =
    Pext_decl of constructor_arguments * core_type option Ploc.vala
      (*
         | C of T1 * ... * Tn     ([T1; ...; Tn], None)
         | C: T0                  ([], Some T0)
         | C: T1 * ... * Tn -> T0 ([T1; ...; Tn], Some T0)
       *)
  | Pext_rebind of Longident.t Ploc.vala loc
      (*
         | C = D
       *)

(** {1 Class language} *)

(* Type expressions for the class language *)

and class_type =
    {
     pcty_desc: class_type_desc;
     pcty_loc: Location.t;
     pcty_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and class_type_desc =
(*-*)  | Pcty_xtr of string loc
  | Pcty_constr of Longident.t loc * core_type list Ploc.vala
        (* c
           ['a1, ..., 'an] c *)
  | Pcty_signature of class_signature
        (* object ... end *)
  | Pcty_arrow of arg_label Ploc.vala * core_type * class_type
        (* T -> CT       Simple
           ~l:T -> CT    Labelled l
           ?l:T -> CT    Optional l
         *)
  | Pcty_extension of extension
        (* [%id] *)
  | Pcty_open of open_description * class_type
        (* let open M in CT *)

and class_signature =
    {
     pcsig_self: core_type;
     pcsig_fields: class_type_field list Ploc.vala;
    }
(* object('selfpat) ... end
   object ... end             (self = Ptyp_any)
*)

and class_type_field =
    {
     pctf_desc: class_type_field_desc;
     pctf_loc: Location.t;
     pctf_attributes: attributes; (* ... [@@id1] [@@id2] *)
    }

and class_type_field_desc =
  | Pctf_inherit of class_type
        (* inherit CT *)
  | Pctf_val of (label Ploc.vala loc * mutable_flag Ploc.vala * virtual_flag Ploc.vala * core_type)
        (* val x: T *)
  | Pctf_method of (label Ploc.vala loc * private_flag Ploc.vala * virtual_flag Ploc.vala * core_type)
        (* method x: T

           Note: T can be a Ptyp_poly.
        *)
  | Pctf_constraint  of (core_type * core_type)
        (* constraint T1 = T2 *)
  | Pctf_attribute of attribute
        (* [@@@id] *)
  | Pctf_extension of extension
        (* [%%id] *)

and 'a class_infos =
    {
     pci_virt: virtual_flag Ploc.vala;
     pci_params: (core_type * (variance * injectivity)) list Ploc.vala;
     pci_name: string Ploc.vala loc;
     pci_expr: 'a;
     pci_loc: Location.t;
     pci_attributes: attributes;  (* ... [@@id1] [@@id2] *)
    }
(* class c = ...
   class ['a1,...,'an] c = ...
   class virtual c = ...

   Also used for "class type" declaration.
*)

and class_description = class_type class_infos

and class_type_declaration = class_type class_infos

(* Value expressions for the class language *)

and class_expr =
    {
     pcl_desc: class_expr_desc;
     pcl_loc: Location.t;
     pcl_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and class_expr_desc =
(*-*)  | Pcl_xtr of string loc
  | Pcl_constr of Longident.t loc * core_type list Ploc.vala
        (* c
           ['a1, ..., 'an] c *)
  | Pcl_structure of class_structure
        (* object ... end *)
  | Pcl_fun of arg_label Ploc.vala * expression option Ploc.vala * pattern * class_expr
        (* fun P -> CE                          (Simple, None)
           fun ~l:P -> CE                       (Labelled l, None)
           fun ?l:P -> CE                       (Optional l, None)
           fun ?l:(P = E0) -> CE                (Optional l, Some E0)
        *)
  | Pcl_apply of class_expr * (arg_label * expression) list Ploc.vala
        (* CE ~l1:E1 ... ~ln:En
           li can be empty (non labeled argument) or start with '?'
            (optional argument).

           Invariant: n > 0
        *)
  | Pcl_let of rec_flag Ploc.vala * value_binding list Ploc.vala * class_expr
        (* let P1 = E1 and ... and Pn = EN in CE      (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN in CE  (flag = Recursive)
         *)
  | Pcl_constraint of class_expr * class_type
        (* (CE : CT) *)
  | Pcl_extension of extension
  (* [%id] *)
  | Pcl_open of open_description * class_expr
  (* let open M in CE *)


and class_structure =
    {
     pcstr_self: pattern Ploc.vala;
     pcstr_fields: class_field list Ploc.vala;
    }
(* object(selfpat) ... end
   object ... end           (self = Ppat_any)
*)

and class_field =
    {
     pcf_desc: class_field_desc;
     pcf_loc: Location.t;
     pcf_attributes: attributes; (* ... [@@id1] [@@id2] *)
    }

and class_field_desc =
  | Pcf_inherit of override_flag Ploc.vala * class_expr * string Ploc.vala loc option Ploc.vala
        (* inherit CE
           inherit CE as x
           inherit! CE
           inherit! CE as x
  *)
  | Pcf_val of (label Ploc.vala loc * mutable_flag Ploc.vala * class_field_kind)
        (* val x = E
           val virtual x: T
  *)
  | Pcf_method of (label Ploc.vala loc * private_flag Ploc.vala * class_field_kind)
        (* method x = E            (E can be a Pexp_poly)
           method virtual x: T     (T can be a Ptyp_poly)
         *)
  | Pcf_constraint of (core_type * core_type)
        (* constraint T1 = T2 *)
  | Pcf_initializer of expression
        (* initializer E *)
  | Pcf_attribute of attribute
        (* [@@@id] *)
  | Pcf_extension of extension
        (* [%%id] *)

and class_field_kind =
  | Cfk_virtual of core_type
  | Cfk_concrete of override_flag Ploc.vala * expression

and class_declaration = class_expr class_infos

(** {1 Module language} *)

(* Type expressions for the module language *)

and module_type =
    {
     pmty_desc: module_type_desc;
     pmty_loc: Location.t;
     pmty_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and module_type_desc =
  | Pmty_ident of Longident.t loc
        (* S *)
  | Pmty_signature of signature
        (* sig ... end *)
  | Pmty_functor of functor_parameter Ploc.vala * module_type
        (* functor(X : MT1) -> MT2 *)
  | Pmty_with of module_type * with_constraint list Ploc.vala
        (* MT with ... *)
  | Pmty_typeof of module_expr
        (* module type of ME *)
  | Pmty_extension of extension
        (* [%id] *)
  | Pmty_alias of Longident.t Ploc.vala loc
        (* (module M) *)
(*-*)  | Pmty_xtr of string loc

and functor_parameter =
  | Unit
        (* () *)
  | Named of string Ploc.vala option Ploc.vala loc * module_type
        (* (X : MT)          Some X, MT
           (_ : MT)          None, MT *)

and signature = signature_item list Ploc.vala

and signature_item =
    {
     psig_desc: signature_item_desc;
     psig_loc: Location.t;
    }

and signature_item_desc =
  | Psig_value of value_description
        (*
          val x: T
          external x: T = "s1" ... "sn"
         *)
  | Psig_type of rec_flag Ploc.vala * type_declaration list Ploc.vala
        (* type t1 = ... and ... and tn  = ... *)
  | Psig_typesubst of type_declaration list Ploc.vala
        (* type t1 := ... and ... and tn := ...  *)
  | Psig_typext of type_extension
        (* type t1 += ... *)
  | Psig_exception of type_exception
        (* exception C of T *)
  | Psig_module of module_declaration
        (* module X = M
           module X : MT *)
  | Psig_modsubst of module_substitution
        (* module X := M *)
  | Psig_recmodule of module_declaration list Ploc.vala
        (* module rec X1 : MT1 and ... and Xn : MTn *)
  | Psig_modtype of module_type_declaration
        (* module type S = MT
           module type S *)
  | Psig_modtypesubst of module_type_declaration
        (* module type S :=  ...  *)
  | Psig_open of open_description
        (* open X *)
  | Psig_include of include_description
        (* include MT *)
  | Psig_class of class_description list Ploc.vala
        (* class c1 : ... and ... and cn : ... *)
  | Psig_class_type of class_type_declaration list Ploc.vala
        (* class type ct1 = ... and ... and ctn = ... *)
  | Psig_attribute of attribute
        (* [@@@id] *)
  | Psig_extension of extension * attributes
        (* [%%id] *)

and module_declaration =
    {
     pmd_name: string Ploc.vala option Ploc.vala loc;
     pmd_type: module_type;
     pmd_attributes: attributes; (* ... [@@id1] [@@id2] *)
     pmd_loc: Location.t;
    }
(* S : MT *)

and module_substitution =
    {
     pms_name: string Ploc.vala loc;
     pms_manifest: Longident.t Ploc.vala loc;
     pms_attributes: attributes; (* ... [@@id1] [@@id2] *)
     pms_loc: Location.t;
    }

and module_type_declaration =
    {
     pmtd_name: string Ploc.vala loc;
     pmtd_type: module_type option Ploc.vala;
     pmtd_attributes: attributes; (* ... [@@id1] [@@id2] *)
     pmtd_loc: Location.t;
    }
(* S = MT
   S       (abstract module type declaration, pmtd_type = None)
*)

and 'a open_infos =
    {
     popen_expr: 'a;
     popen_override: override_flag Ploc.vala;
     popen_loc: Location.t;
     popen_attributes: attributes;
    }
(* open! X - popen_override = Override (silences the 'used identifier
                              shadowing' warning)
   open  X - popen_override = Fresh
*)

and open_description = Longident.t Ploc.vala loc open_infos
(* open M.N
   open M(N).O *)

and open_declaration = module_expr open_infos
(* open M.N
   open M(N).O
   open struct ... end *)

and 'a include_infos =
    {
     pincl_mod: 'a;
     pincl_loc: Location.t;
     pincl_attributes: attributes;
    }

and include_description = module_type include_infos
(* include MT *)

and include_declaration = module_expr include_infos
(* include ME *)

and with_constraint =
  | Pwith_type of Longident.t loc * type_declaration Ploc.vala
        (* with type X.t = ...

            Note: the last component of the longident must match
            the name of the type_declaration. *)
  | Pwith_module of Longident.t Ploc.vala loc * Longident.t Ploc.vala loc
        (* with module X.Y = Z *)
  | Pwith_modtype of Longident.t loc * module_type
        (* with module type X.Y = Z *)
  | Pwith_modtypesubst of Longident.t loc * module_type
        (* with module type X.Y := sig end *)
  | Pwith_typesubst of Longident.t loc * type_declaration Ploc.vala
        (* with type X.t := ..., same format as [Pwith_type] *)
  | Pwith_modsubst of Longident.t Ploc.vala loc * Longident.t Ploc.vala loc
        (* with module X.Y := Z *)

(* Value expressions for the module language *)

and module_expr =
    {
     pmod_desc: module_expr_desc;
     pmod_loc: Location.t;
     pmod_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and module_expr_desc =
  | Pmod_ident of Longident.t Ploc.vala loc
        (* X *)
  | Pmod_structure of structure
        (* struct ... end *)
  | Pmod_functor of functor_parameter Ploc.vala * module_expr
        (* functor(X : MT1) -> ME *)
  | Pmod_apply of module_expr * module_expr
        (* ME1(ME2) *)
  | Pmod_constraint of module_expr * module_type
        (* (ME : MT) *)
  | Pmod_unpack of expression
        (* (val E) *)
  | Pmod_extension of extension
        (* [%id] *)
(*-*)  | Pmod_xtr of string loc

and structure = structure_item list Ploc.vala

and structure_item =
    {
     pstr_desc: structure_item_desc;
     pstr_loc: Location.t;
    }

and structure_item_desc =
  | Pstr_eval of expression Ploc.vala * attributes
        (* E *)
  | Pstr_value of rec_flag Ploc.vala * value_binding list Ploc.vala
        (* let P1 = E1 and ... and Pn = EN       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN   (flag = Recursive)
        *)
  | Pstr_primitive of value_description
        (*  val x: T
            external x: T = "s1" ... "sn" *)
  | Pstr_type of rec_flag Ploc.vala * type_declaration list Ploc.vala
        (* type t1 = ... and ... and tn = ... *)
  | Pstr_typext of type_extension
        (* type t1 += ... *)
  | Pstr_exception of type_exception
        (* exception C of T
           exception C = M.X *)
  | Pstr_module of module_binding
        (* module X = ME *)
  | Pstr_recmodule of module_binding list Ploc.vala
        (* module rec X1 = ME1 and ... and Xn = MEn *)
  | Pstr_modtype of module_type_declaration
        (* module type S = MT *)
  | Pstr_open of open_declaration
        (* open X *)
  | Pstr_class of class_declaration list Ploc.vala
        (* class c1 = ... and ... and cn = ... *)
  | Pstr_class_type of class_type_declaration list Ploc.vala
        (* class type ct1 = ... and ... and ctn = ... *)
  | Pstr_include of include_declaration
        (* include ME *)
  | Pstr_attribute of attribute
        (* [@@@id] *)
  | Pstr_extension of extension * attributes
        (* [%%id] *)

and value_binding =
  {
    pvb_pat: pattern;
    pvb_expr: expression;
    pvb_attributes: attributes;
    pvb_loc: Location.t;
  }

and module_binding =
    {
     pmb_name: string Ploc.vala option Ploc.vala loc;
     pmb_expr: module_expr;
     pmb_attributes: attributes;
     pmb_loc: Location.t;
    }
(* X = ME *)

(** {1 Toplevel} *)

(* Toplevel phrases *)

type toplevel_phrase =
  | Ptop_def of structure
  | Ptop_dir of toplevel_directive
     (* #use, #load ... *)

and toplevel_directive =
  {
    pdir_name: string loc;
    pdir_arg: directive_argument option;
    pdir_loc: Location.t;
  }

and directive_argument =
  {
    pdira_desc: directive_argument_desc;
    pdira_loc: Location.t;
  }

and directive_argument_desc =
  | Pdir_string of string
  | Pdir_int of string * char option
  | Pdir_ident of Longident.t
  | Pdir_bool of bool
