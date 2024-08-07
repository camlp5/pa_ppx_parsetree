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
  | Pconst_integer of string Ploc.vala * char option
      (** Integer constants such as [3] [3l] [3L] [3n].

     Suffixes [[g-z][G-Z]] are accepted by the parser.
     Suffixes except ['l'], ['L'] and ['n'] are rejected by the typechecker
  *)
  | Pconst_char of char Ploc.vala  (** Character such as ['c']. *)
  | Pconst_string of string Ploc.vala * Location.t * string Ploc.vala option
      (** Constant string such as ["constant"] or
          [{delim|other constant|delim}].

     The location span the content of the string, without the delimiters.
  *)
  | Pconst_float of string Ploc.vala * char option
      (** Float constant such as [3.4], [2e5] or [1.4e-4].

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
(** Attributes such as [[\@id ARG]] and [[\@\@id ARG]].

          Metadata containers passed around within the AST.
          The compiler ignores unknown attributes.
       *)

and extension = string Ploc.vala loc * payload
(** Extension points such as [[%id ARG] and [%%id ARG]].

         Sub-language placeholder -- rejected by the typechecker.
      *)

and attributes = attribute list Ploc.vala

and payload =
  | PStr of structure
  | PSig of signature  (** [: SIG] in an attribute or an extension point *)
  | PTyp of core_type  (** [: T] in an attribute or an extension point *)
  | PPat of pattern * expression option Ploc.vala
      (** [? P]  or  [? P when E], in an attribute or an extension point *)

(** {1 Core language} *)
(** {2 Type expressions} *)

and core_type =
    {
     ptyp_desc: core_type_desc;
     ptyp_loc: Location.t;
     ptyp_loc_stack: location_stack;
     ptyp_attributes: attributes;  (** [... [\@id1] [\@id2]] *)
    }

and core_type_desc =
(*-*)  | Ptyp_xtr of string loc
  | Ptyp_any  (** [_] *)
  | Ptyp_var of string Ploc.vala  (** A type variable such as ['a] *)
  | Ptyp_arrow of arg_label Ploc.vala * core_type * core_type
      (** [Ptyp_arrow(lbl, T1, T2)] represents:
            - [T1 -> T2]    when [lbl] is
                                     {{!Asttypes.arg_label.Nolabel}[Nolabel]},
            - [~l:T1 -> T2] when [lbl] is
                                     {{!Asttypes.arg_label.Labelled}[Labelled]},
            - [?l:T1 -> T2] when [lbl] is
                                     {{!Asttypes.arg_label.Optional}[Optional]}.
         *)
  | Ptyp_tuple of core_type list Ploc.vala
      (** [Ptyp_tuple([T1 ; ... ; Tn])]
          represents a product type [T1 * ... * Tn].

           Invariant: [n >= 2].
        *)
  | Ptyp_constr of Longident.t Ploc.vala loc * core_type list Ploc.vala
      (** [Ptyp_constr(lident, l)] represents:
            - [tconstr]               when [l=[]],
            - [T tconstr]             when [l=[T]],
            - [(T1, ..., Tn) tconstr] when [l=[T1 ; ... ; Tn]].
         *)
  | Ptyp_object of object_field list Ploc.vala * closed_flag Ploc.vala
      (** [Ptyp_object([ l1:T1; ...; ln:Tn ], flag)] represents:
            - [< l1:T1; ...; ln:Tn >]     when [flag] is
                                       {{!Asttypes.closed_flag.Closed}[Closed]},
            - [< l1:T1; ...; ln:Tn; .. >] when [flag] is
                                           {{!Asttypes.closed_flag.Open}[Open]}.
         *)
  | Ptyp_class of Longident.t loc * core_type list Ploc.vala
      (** [Ptyp_class(tconstr, l)] represents:
            - [#tconstr]               when [l=[]],
            - [T #tconstr]             when [l=[T]],
            - [(T1, ..., Tn) #tconstr] when [l=[T1 ; ... ; Tn]].
         *)
  | Ptyp_alias of core_type * string Ploc.vala loc  (** [T as 'a]. *)
  | Ptyp_variant of row_field list Ploc.vala * closed_flag Ploc.vala * label list Ploc.vala option Ploc.vala
      (** [Ptyp_variant([`A;`B], flag, labels)] represents:
            - [[ `A|`B ]]
                      when [flag]   is {{!Asttypes.closed_flag.Closed}[Closed]},
                       and [labels] is [None],
            - [[> `A|`B ]]
                      when [flag]   is {{!Asttypes.closed_flag.Open}[Open]},
                       and [labels] is [None],
            - [[< `A|`B ]]
                      when [flag]   is {{!Asttypes.closed_flag.Closed}[Closed]},
                       and [labels] is [Some []],
            - [[< `A|`B > `X `Y ]]
                      when [flag]   is {{!Asttypes.closed_flag.Closed}[Closed]},
                       and [labels] is [Some ["X";"Y"]].
         *)
  | Ptyp_poly of string Ploc.vala loc list Ploc.vala * core_type
      (** ['a1 ... 'an. T]

           Can only appear in the following context:

           - As the {!core_type} of a
          {{!pattern_desc.Ppat_constraint}[Ppat_constraint]} node corresponding
             to a constraint on a let-binding:
            {[let x : 'a1 ... 'an. T = e ...]}

           - Under {{!class_field_kind.Cfk_virtual}[Cfk_virtual]} for methods
          (not values).

           - As the {!core_type} of a
           {{!class_type_field_desc.Pctf_method}[Pctf_method]} node.

           - As the {!core_type} of a {{!expression_desc.Pexp_poly}[Pexp_poly]}
           node.

           - As the {{!label_declaration.pld_type}[pld_type]} field of a
           {!label_declaration}.

           - As a {!core_type} of a {{!core_type_desc.Ptyp_object}[Ptyp_object]}
           node.

           - As the {{!value_description.pval_type}[pval_type]} field of a
           {!value_description}.
         *)
  | Ptyp_package of package_type  (** [(module S)]. *)
  | Ptyp_open of Longident.t Ploc.vala loc * core_type (** [M.(T)] *)
  | Ptyp_extension of extension  (** [[%id]]. *)

and package_type = Longident.t Ploc.vala loc * (Longident.t Ploc.vala loc * core_type) list Ploc.vala
(** As {!package_type} typed values:
         - [(S, [])] represents [(module S)],
         - [(S, [(t1, T1) ; ... ; (tn, Tn)])]
          represents [(module S with type t1 = T1 and ... and tn = Tn)].
       *)

and row_field = {
  prf_desc : row_field_desc;
  prf_loc : Location.t;
  prf_attributes : attributes;
}

and row_field_desc =
  | Rtag of label Ploc.vala loc * bool Ploc.vala * core_type list Ploc.vala
      (** [Rtag(`A, b, l)] represents:
           - [`A]                   when [b] is [true]  and [l] is [[]],
           - [`A of T]              when [b] is [false] and [l] is [[T]],
           - [`A of T1 & .. & Tn]   when [b] is [false] and [l] is [[T1;...Tn]],
           - [`A of & T1 & .. & Tn] when [b] is [true]  and [l] is [[T1;...Tn]].

          - The [bool] field is true if the tag contains a
            constant (empty) constructor.
          - [&] occurs when several types are used for the same constructor
            (see 4.2 in the manual)
        *)
  | Rinherit of core_type  (** [[ | t ]] *)

and object_field = {
  pof_desc : object_field_desc;
  pof_loc : Location.t;
  pof_attributes : attributes;
}

and object_field_desc =
  | Otag of label Ploc.vala loc * core_type
  | Oinherit of core_type

(** {2 Patterns} *)

and pattern =
    {
     ppat_desc: pattern_desc;
     ppat_loc: Location.t;
     ppat_loc_stack: location_stack;
     ppat_attributes: attributes;  (** [... [\@id1] [\@id2]] *)
    }

and pattern_desc =
(*-*)  | Ppat_xtr of string loc
  | Ppat_any  (** The pattern [_]. *)
  | Ppat_var of string Ploc.vala loc  (** A variable pattern such as [x] *)
  | Ppat_alias of pattern * string Ploc.vala loc
      (** An alias pattern such as [P as 'a] *)
  | Ppat_constant of constant Ploc.vala
      (** Patterns such as [1], ['a'], ["true"], [1.0], [1l], [1L], [1n] *)
  | Ppat_interval of constant Ploc.vala * constant Ploc.vala
      (** Patterns such as ['a'..'z'].

           Other forms of interval are recognized by the parser
           but rejected by the type-checker. *)
  | Ppat_tuple of pattern list Ploc.vala
      (** Patterns [(P1, ..., Pn)].

           Invariant: [n >= 2]
        *)
  | Ppat_construct of Longident.t Ploc.vala loc * (string Ploc.vala loc list Ploc.vala * pattern) option Ploc.vala
      (** [Ppat_construct(C, args)] represents:
            - [C]               when [args] is [None],
            - [C P]             when [args] is [Some ([], P)]
            - [C (P1, ..., Pn)] when [args] is
                                           [Some ([], Ppat_tuple [P1; ...; Pn])]
            - [C (type a b) P]  when [args] is [Some ([a; b], P)]
         *)
  | Ppat_variant of label Ploc.vala * pattern option Ploc.vala
      (** [Ppat_variant(`A, pat)] represents:
            - [`A]   when [pat] is [None],
            - [`A P] when [pat] is [Some P]
         *)
  | Ppat_record of (Longident.t Ploc.vala loc * pattern) list Ploc.vala * closed_flag Ploc.vala
      (** [Ppat_record([(l1, P1) ; ... ; (ln, Pn)], flag)] represents:
            - [{ l1=P1; ...; ln=Pn }]
                 when [flag] is {{!Asttypes.closed_flag.Closed}[Closed]}
            - [{ l1=P1; ...; ln=Pn; _}]
                 when [flag] is {{!Asttypes.closed_flag.Open}[Open]}

           Invariant: [n > 0]
         *)
  | Ppat_array of pattern list Ploc.vala  (** Pattern [[| P1; ...; Pn |]] *)
  | Ppat_or of pattern * pattern  (** Pattern [P1 | P2] *)
  | Ppat_constraint of pattern * core_type  (** Pattern [(P : T)] *)
  | Ppat_type of Longident.t Ploc.vala loc  (** Pattern [#tconst] *)
  | Ppat_lazy of pattern  (** Pattern [lazy P] *)
  | Ppat_unpack of string Ploc.vala option Ploc.vala loc
      (** [Ppat_unpack(s)] represents:
            - [(module P)] when [s] is [Some "P"]
            - [(module _)] when [s] is [None]

           Note: [(module P : S)] is represented as
           [Ppat_constraint(Ppat_unpack(Some "P"), Ptyp_package S)]
         *)
  | Ppat_exception of pattern  (** Pattern [exception P] *)
  | Ppat_extension of extension  (** Pattern [[%id]] *)
  | Ppat_open of Longident.t Ploc.vala loc * pattern  (** Pattern [M.(P)] *)

(** {2 Value expressions} *)

and expression =
    {
     pexp_desc: expression_desc;
     pexp_loc: Location.t;
     pexp_loc_stack: location_stack;
     pexp_attributes: attributes;  (** [... [\@id1] [\@id2]] *)
    }

and expression_desc =
(*-*)  | Pexp_xtr of string loc
  | Pexp_ident of Longident.t Ploc.vala loc
      (** Identifiers such as [x] and [M.x]
         *)
  | Pexp_constant of constant Ploc.vala
      (** Expressions constant such as [1], ['a'], ["true"], [1.0], [1l],
            [1L], [1n] *)
  | Pexp_let of rec_flag Ploc.vala * value_binding list Ploc.vala * expression
      (** [Pexp_let(flag, [(P1,E1) ; ... ; (Pn,En)], E)] represents:
            - [let P1 = E1 and ... and Pn = EN in E]
               when [flag] is {{!Asttypes.rec_flag.Nonrecursive}[Nonrecursive]},
            - [let rec P1 = E1 and ... and Pn = EN in E]
               when [flag] is {{!Asttypes.rec_flag.Recursive}[Recursive]}.
         *)
  | Pexp_function of
      function_param list Ploc.vala * type_constraint Ploc.vala option Ploc.vala * function_body
  (** [Pexp_function ([P1; ...; Pn], C, body)] represents any construct
      involving [fun] or [function], including:
      - [fun P1 ... Pn -> E]
        when [body = Pfunction_body E]
      - [fun P1 ... Pn -> function p1 -> e1 | ... | pm -> em]
        when [body = Pfunction_cases [ p1 -> e1; ...; pm -> em ]]

      [C] represents a type constraint or coercion placed immediately before the
      arrow, e.g. [fun P1 ... Pn : ty -> ...] when [C = Some (Pconstraint ty)].

      A function must have parameters. [Pexp_function (params, _, body)] must
      have non-empty [params] or a [Pfunction_cases _] body.
  *)
  | Pexp_apply of expression * (arg_label * expression) list Ploc.vala
      (** [Pexp_apply(E0, [(l1, E1) ; ... ; (ln, En)])]
            represents [E0 ~l1:E1 ... ~ln:En]

            [li] can be
              {{!Asttypes.arg_label.Nolabel}[Nolabel]}   (non labeled argument),
              {{!Asttypes.arg_label.Labelled}[Labelled]} (labelled arguments) or
              {{!Asttypes.arg_label.Optional}[Optional]} (optional argument).

           Invariant: [n > 0]
         *)
  | Pexp_match of expression * case list Ploc.vala
      (** [match E0 with P1 -> E1 | ... | Pn -> En] *)
  | Pexp_try of expression * case list Ploc.vala
      (** [try E0 with P1 -> E1 | ... | Pn -> En] *)
  | Pexp_tuple of expression list Ploc.vala
      (** Expressions [(E1, ..., En)]

           Invariant: [n >= 2]
        *)
  | Pexp_construct of Longident.t Ploc.vala loc * expression option Ploc.vala
      (** [Pexp_construct(C, exp)] represents:
           - [C]               when [exp] is [None],
           - [C E]             when [exp] is [Some E],
           - [C (E1, ..., En)] when [exp] is [Some (Pexp_tuple[E1;...;En])]
        *)
  | Pexp_variant of label Ploc.vala * expression option Ploc.vala
      (** [Pexp_variant(`A, exp)] represents
            - [`A]   when [exp] is [None]
            - [`A E] when [exp] is [Some E]
         *)
  | Pexp_record of (Longident.t Ploc.vala loc * expression) list Ploc.vala * expression option Ploc.vala
      (** [Pexp_record([(l1,P1) ; ... ; (ln,Pn)], exp0)] represents
            - [{ l1=P1; ...; ln=Pn }]         when [exp0] is [None]
            - [{ E0 with l1=P1; ...; ln=Pn }] when [exp0] is [Some E0]

           Invariant: [n > 0]
         *)
  | Pexp_field of expression * Longident.t Ploc.vala loc  (** [E.l] *)
  | Pexp_setfield of expression * Longident.t Ploc.vala loc * expression
      (** [E1.l <- E2] *)
  | Pexp_array of expression list Ploc.vala  (** [[| E1; ...; En |]] *)
  | Pexp_ifthenelse of expression * expression * expression option Ploc.vala
      (** [if E1 then E2 else E3] *)
  | Pexp_sequence of expression * expression  (** [E1; E2] *)
  | Pexp_while of expression * expression  (** [while E1 do E2 done] *)
  | Pexp_for of pattern * expression * expression * direction_flag Ploc.vala * expression
      (** [Pexp_for(i, E1, E2, direction, E3)] represents:
            - [for i = E1 to E2 do E3 done]
                 when [direction] is {{!Asttypes.direction_flag.Upto}[Upto]}
            - [for i = E1 downto E2 do E3 done]
                 when [direction] is {{!Asttypes.direction_flag.Downto}[Downto]}
         *)
  | Pexp_constraint of expression * core_type  (** [(E : T)] *)
  | Pexp_coerce of expression * core_type option Ploc.vala * core_type
      (** [Pexp_coerce(E, from, T)] represents
            - [(E :> T)]      when [from] is [None],
            - [(E : T0 :> T)] when [from] is [Some T0].
         *)
  | Pexp_send of expression * label Ploc.vala loc  (** [E # m] *)
  | Pexp_new of Longident.t Ploc.vala loc  (** [new M.c] *)
  | Pexp_setinstvar of label Ploc.vala loc * expression  (** [x <- 2] *)
  | Pexp_override of (label Ploc.vala loc * expression) list Ploc.vala
      (** [{< x1 = E1; ...; xn = En >}] *)
  | Pexp_letmodule of string Ploc.vala option Ploc.vala loc * module_expr * expression
      (** [let module M = ME in E] *)
  | Pexp_letexception of extension_constructor Ploc.vala * expression
      (** [let exception C in E] *)
  | Pexp_assert of expression
      (** [assert E].

           Note: [assert false] is treated in a special way by the
           type-checker. *)
  | Pexp_lazy of expression  (** [lazy E] *)
  | Pexp_poly of expression * core_type option
      (** Used for method bodies.

           Can only be used as the expression under
           {{!class_field_kind.Cfk_concrete}[Cfk_concrete]} for methods (not
           values). *)
  | Pexp_object of class_structure  (** [object ... end] *)
  | Pexp_newtype of string Ploc.vala loc * expression  (** [fun (type t) -> E] *)
  | Pexp_pack of module_expr
      (** [(module ME)].

           [(module ME : S)] is represented as
           [Pexp_constraint(Pexp_pack ME, Ptyp_package S)] *)
  | Pexp_open of open_declaration * expression
      (** - [M.(E)]
            - [let open M in E]
            - [let open! M in E] *)
  | Pexp_letop of letop
      (** - [let* P = E0 in E1]
            - [let* P0 = E00 and* P1 = E01 in E1] *)
  | Pexp_extension of extension  (** [[%id]] *)
  | Pexp_unreachable  (** [.] *)

and case =
    {
     pc_lhs: pattern Ploc.vala;
     pc_guard: expression Ploc.vala option Ploc.vala;
     pc_rhs: expression Ploc.vala;
   }
(** Values of type {!case} represents [(P -> E)] or [(P when E0 -> E)] *)

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

and function_param_desc =
  | Pparam_val of arg_label Ploc.vala * expression option Ploc.vala * pattern
  (** [Pparam_val (lbl, exp0, P)] represents the parameter:
      - [P]
        when [lbl] is {{!Asttypes.arg_label.Nolabel}[Nolabel]}
        and [exp0] is [None]
      - [~l:P]
        when [lbl] is {{!Asttypes.arg_label.Labelled}[Labelled l]}
        and [exp0] is [None]
      - [?l:P]
        when [lbl] is {{!Asttypes.arg_label.Optional}[Optional l]}
        and [exp0] is [None]
      - [?l:(P = E0)]
        when [lbl] is {{!Asttypes.arg_label.Optional}[Optional l]}
        and [exp0] is [Some E0]

      Note: If [E0] is provided, only
      {{!Asttypes.arg_label.Optional}[Optional]} is allowed.
  *)
  | Pparam_newtype of string Ploc.vala loc
  (** [Pparam_newtype x] represents the parameter [(type x)].
      [x] carries the location of the identifier, whereas the [pparam_loc]
      on the enclosing [function_param] node is the location of the [(type x)]
      as a whole.

      Multiple parameters [(type a b c)] are represented as multiple
      [Pparam_newtype] nodes, let's say:

      {[ [ { pparam_kind = Pparam_newtype a; pparam_loc = loc1 };
           { pparam_kind = Pparam_newtype b; pparam_loc = loc2 };
           { pparam_kind = Pparam_newtype c; pparam_loc = loc3 };
         ]
      ]}

      Here, the first loc [loc1] is the location of [(type a b c)], and the
      subsequent locs [loc2] and [loc3] are the same as [loc1], except marked as
      ghost locations. The locations on [a], [b], [c], correspond to the
      variables [a], [b], and [c] in the source code.
  *)

and function_param =
  { pparam_loc : Location.t;
    pparam_desc : function_param_desc;
  }

and function_body =
  | Pfunction_body of expression
  | Pfunction_cases of case list Ploc.vala * Location.t * attributes
  (** In [Pfunction_cases (_, loc, attrs)], the location extends from the
      start of the [function] keyword to the end of the last case. The compiler
      will only use typechecking-related attributes from [attrs], e.g. enabling
      or disabling a warning.
  *)
(** See the comment on {{!expression_desc.Pexp_function}[Pexp_function]}. *)

and type_constraint =
  | Pconstraint of core_type
  | Pcoerce of core_type option * core_type
(** See the comment on {{!expression_desc.Pexp_function}[Pexp_function]}. *)

(** {2 Value descriptions} *)

and value_description =
    {
     pval_name: string Ploc.vala loc;
     pval_type: core_type;
     pval_prim: string list Ploc.vala;
     pval_attributes: attributes;  (** [... [\@\@id1] [\@\@id2]] *)
     pval_loc: Location.t;
    }
(** Values of type {!value_description} represents:
    - [val x: T],
            when {{!value_description.pval_prim}[pval_prim]} is [[]]
    - [external x: T = "s1" ... "sn"]
            when {{!value_description.pval_prim}[pval_prim]} is [["s1";..."sn"]]
*)

(** {2 Type declarations} *)

and type_declaration =
    {
     ptype_name: string Ploc.vala loc;
     ptype_params: (core_type * (variance * injectivity)) list Ploc.vala;
      (** [('a1,...'an) t] *)
     ptype_cstrs: (core_type * core_type * Location.t) list Ploc.vala;
      (** [... constraint T1=T1'  ... constraint Tn=Tn'] *)
     ptype_kind: type_kind;
     ptype_private: private_flag Ploc.vala;  (** for [= private ...] *)
     ptype_manifest: core_type Ploc.vala option Ploc.vala;  (** represents [= T] *)
     ptype_attributes: attributes;  (** [... [\@\@id1] [\@\@id2]] *)
     ptype_loc: Location.t;
    }
(**
   Here are type declarations and their representation,
   for various {{!type_declaration.ptype_kind}[ptype_kind]}
           and {{!type_declaration.ptype_manifest}[ptype_manifest]} values:
 - [type t]   when [type_kind] is {{!type_kind.Ptype_abstract}[Ptype_abstract]},
               and [manifest]  is [None],
 - [type t = T0]
              when [type_kind] is {{!type_kind.Ptype_abstract}[Ptype_abstract]},
               and [manifest]  is [Some T0],
 - [type t = C of T | ...]
              when [type_kind] is {{!type_kind.Ptype_variant}[Ptype_variant]},
               and [manifest]  is [None],
 - [type t = T0 = C of T | ...]
              when [type_kind] is {{!type_kind.Ptype_variant}[Ptype_variant]},
               and [manifest]  is [Some T0],
 - [type t = {l: T; ...}]
              when [type_kind] is {{!type_kind.Ptype_record}[Ptype_record]},
               and [manifest]  is [None],
 - [type t = T0 = {l : T; ...}]
              when [type_kind] is {{!type_kind.Ptype_record}[Ptype_record]},
               and [manifest]  is [Some T0],
 - [type t = ..]
              when [type_kind] is {{!type_kind.Ptype_open}[Ptype_open]},
               and [manifest]  is [None].
*)

and type_kind =
  | Ptype_abstract
  | Ptype_variant of constructor_declaration list Ploc.vala
  | Ptype_record of label_declaration list Ploc.vala (** Invariant: non-empty list *)
  | Ptype_open

and label_declaration =
    {
     pld_name: string Ploc.vala loc;
     pld_mutable: mutable_flag Ploc.vala;
     pld_type: core_type Ploc.vala;
     pld_loc: Location.t;
     pld_attributes: attributes;  (** [l : T [\@id1] [\@id2]] *)
    }
(**
   - [{ ...; l: T; ... }]
                           when {{!label_declaration.pld_mutable}[pld_mutable]}
                             is {{!Asttypes.mutable_flag.Immutable}[Immutable]},
   - [{ ...; mutable l: T; ... }]
                           when {{!label_declaration.pld_mutable}[pld_mutable]}
                             is {{!Asttypes.mutable_flag.Mutable}[Mutable]}.

   Note: [T] can be a {{!core_type_desc.Ptyp_poly}[Ptyp_poly]}.
*)

and constructor_declaration =
    {
     pcd_name: string Ploc.vala loc;
     pcd_vars: string Ploc.vala loc list Ploc.vala;
     pcd_args: constructor_arguments;
     pcd_res: core_type option Ploc.vala;
     pcd_loc: Location.t;
     pcd_attributes: attributes;  (** [C of ... [\@id1] [\@id2]] *)
    }

and constructor_arguments =
  | Pcstr_tuple of core_type list Ploc.vala
  | Pcstr_record of label_declaration list Ploc.vala
      (** Values of type {!constructor_declaration}
    represents the constructor arguments of:
  - [C of T1 * ... * Tn]     when [res = None],
                              and [args = Pcstr_tuple [T1; ... ; Tn]],
  - [C: T0]                  when [res = Some T0],
                              and [args = Pcstr_tuple []],
  - [C: T1 * ... * Tn -> T0] when [res = Some T0],
                              and [args = Pcstr_tuple [T1; ... ; Tn]],
  - [C of {...}]             when [res = None],
                              and [args = Pcstr_record [...]],
  - [C: {...} -> T0]         when [res = Some T0],
                              and [args = Pcstr_record [...]].
*)

and type_extension =
    {
     ptyext_path: Longident.t Ploc.vala loc;
     ptyext_params: (core_type * (variance * injectivity)) list Ploc.vala;
     ptyext_constructors: extension_constructor list Ploc.vala;
     ptyext_private: private_flag Ploc.vala;
     ptyext_loc: Location.t;
     ptyext_attributes: attributes;  (** ... [\@\@id1] [\@\@id2] *)
    }
(**
   Definition of new extensions constructors for the extensive sum type [t]
   ([type t += ...]).
*)

and extension_constructor =
    {
     pext_name: string Ploc.vala loc;
     pext_kind: extension_constructor_kind;
     pext_loc: Location.t;
     pext_attributes: attributes;  (** [C of ... [\@id1] [\@id2]] *)
   }

and type_exception =
  {
    ptyexn_constructor : extension_constructor;
    ptyexn_loc : Location.t;
    ptyexn_attributes : attributes;  (** [... [\@\@id1] [\@\@id2]] *)
  }
(** Definition of a new exception ([exception E]). *)

and extension_constructor_kind =
  | Pext_decl of string Ploc.vala loc list Ploc.vala * constructor_arguments * core_type option Ploc.vala
      (** [Pext_decl(existentials, c_args, t_opt)]
          describes a new extension constructor. It can be:
          - [C of T1 * ... * Tn] when:
               {ul {- [existentials] is [[]],}
                   {- [c_args] is [[T1; ...; Tn]],}
                   {- [t_opt] is [None]}.}
          - [C: T0] when
               {ul {- [existentials] is [[]],}
                   {- [c_args] is [[]],}
                   {- [t_opt] is [Some T0].}}
          - [C: T1 * ... * Tn -> T0] when
               {ul {- [existentials] is [[]],}
                   {- [c_args] is [[T1; ...; Tn]],}
                   {- [t_opt] is [Some T0].}}
          - [C: 'a... . T1 * ... * Tn -> T0] when
               {ul {- [existentials] is [['a;...]],}
                   {- [c_args] is [[T1; ... ; Tn]],}
                   {- [t_opt] is [Some T0].}}
       *)
  | Pext_rebind of Longident.t Ploc.vala loc
  (** [Pext_rebind(D)] re-export the constructor [D] with the new name [C] *)

(** {1 Class language} *)
(** {2 Type expressions for the class language} *)

and class_type =
    {
     pcty_desc: class_type_desc;
     pcty_loc: Location.t;
     pcty_attributes: attributes;  (** [... [\@id1] [\@id2]] *)
    }

and class_type_desc =
(*-*)  | Pcty_xtr of string loc
  | Pcty_constr of Longident.t loc * core_type list Ploc.vala
      (** - [c]
            - [['a1, ..., 'an] c] *)
  | Pcty_signature of class_signature  (** [object ... end] *)
  | Pcty_arrow of arg_label Ploc.vala * core_type * class_type
      (** [Pcty_arrow(lbl, T, CT)] represents:
            - [T -> CT]
                     when [lbl] is {{!Asttypes.arg_label.Nolabel}[Nolabel]},
            - [~l:T -> CT]
                     when [lbl] is {{!Asttypes.arg_label.Labelled}[Labelled l]},
            - [?l:T -> CT]
                     when [lbl] is {{!Asttypes.arg_label.Optional}[Optional l]}.
         *)
  | Pcty_extension of extension  (** [%id] *)
  | Pcty_open of open_description * class_type  (** [let open M in CT] *)

and class_signature =
    {
     pcsig_self: core_type;
     pcsig_fields: class_type_field list Ploc.vala;
    }
(** Values of type [class_signature] represents:
    - [object('selfpat) ... end]
    - [object ... end] when {{!class_signature.pcsig_self}[pcsig_self]}
                         is {{!core_type_desc.Ptyp_any}[Ptyp_any]}
*)

and class_type_field =
    {
     pctf_desc: class_type_field_desc;
     pctf_loc: Location.t;
     pctf_attributes: attributes;  (** [... [\@\@id1] [\@\@id2]] *)
    }

and class_type_field_desc =
  | Pctf_inherit of class_type  (** [inherit CT] *)
  | Pctf_val of (label Ploc.vala loc * mutable_flag Ploc.vala * virtual_flag Ploc.vala * core_type)
      (** [val x: T] *)
  | Pctf_method of (label Ploc.vala loc * private_flag Ploc.vala * virtual_flag Ploc.vala * core_type)
      (** [method x: T]

            Note: [T] can be a {{!core_type_desc.Ptyp_poly}[Ptyp_poly]}.
        *)
  | Pctf_constraint of (core_type * core_type)  (** [constraint T1 = T2] *)
  | Pctf_attribute of attribute  (** [[\@\@\@id]] *)
  | Pctf_extension of extension  (** [[%%id]] *)

and 'a class_infos =
    {
     pci_virt: virtual_flag Ploc.vala;
     pci_params: (core_type * (variance * injectivity)) list Ploc.vala;
     pci_name: string Ploc.vala loc;
     pci_expr: 'a;
     pci_loc: Location.t;
     pci_attributes: attributes;  (** [... [\@\@id1] [\@\@id2]] *)
    }
(** Values of type [class_expr class_infos] represents:
    - [class c = ...]
    - [class ['a1,...,'an] c = ...]
    - [class virtual c = ...]

   They are also used for "class type" declaration.
*)

and class_description = class_type class_infos

and class_type_declaration = class_type class_infos

(** {2 Value expressions for the class language} *)

and class_expr =
    {
     pcl_desc: class_expr_desc;
     pcl_loc: Location.t;
     pcl_attributes: attributes;  (** [... [\@id1] [\@id2]] *)
    }

and class_expr_desc =
(*-*)  | Pcl_xtr of string loc
  | Pcl_constr of Longident.t loc * core_type list Ploc.vala
      (** [c] and [['a1, ..., 'an] c] *)
  | Pcl_structure of class_structure  (** [object ... end] *)
  | Pcl_fun of arg_label Ploc.vala * expression option Ploc.vala * pattern * class_expr
      (** [Pcl_fun(lbl, exp0, P, CE)] represents:
            - [fun P -> CE]
                     when [lbl]  is {{!Asttypes.arg_label.Nolabel}[Nolabel]}
                      and [exp0] is [None],
            - [fun ~l:P -> CE]
                     when [lbl]  is {{!Asttypes.arg_label.Labelled}[Labelled l]}
                      and [exp0] is [None],
            - [fun ?l:P -> CE]
                     when [lbl]  is {{!Asttypes.arg_label.Optional}[Optional l]}
                      and [exp0] is [None],
            - [fun ?l:(P = E0) -> CE]
                     when [lbl]  is {{!Asttypes.arg_label.Optional}[Optional l]}
                      and [exp0] is [Some E0].
        *)
  | Pcl_apply of class_expr * (arg_label * expression) list Ploc.vala
      (** [Pcl_apply(CE, [(l1,E1) ; ... ; (ln,En)])]
            represents [CE ~l1:E1 ... ~ln:En].
            [li] can be empty (non labeled argument) or start with [?]
            (optional argument).

            Invariant: [n > 0]
        *)
  | Pcl_let of rec_flag Ploc.vala * value_binding list Ploc.vala * class_expr
      (** [Pcl_let(rec, [(P1, E1); ... ; (Pn, En)], CE)] represents:
            - [let P1 = E1 and ... and Pn = EN in CE]
                when [rec] is {{!Asttypes.rec_flag.Nonrecursive}[Nonrecursive]},
            - [let rec P1 = E1 and ... and Pn = EN in CE]
                when [rec] is {{!Asttypes.rec_flag.Recursive}[Recursive]}.
        *)
  | Pcl_constraint of class_expr * class_type  (** [(CE : CT)] *)
  | Pcl_extension of extension  (** [[%id]] *)
  | Pcl_open of open_description * class_expr  (** [let open M in CE] *)

and class_structure =
    {
     pcstr_self: pattern Ploc.vala;
     pcstr_fields: class_field list Ploc.vala;
    }
(** Values of type {!class_structure} represents:
    - [object(selfpat) ... end]
    - [object ... end] when {{!class_structure.pcstr_self}[pcstr_self]}
                         is {{!pattern_desc.Ppat_any}[Ppat_any]}
*)

and class_field =
    {
     pcf_desc: class_field_desc;
     pcf_loc: Location.t;
     pcf_attributes: attributes;  (** [... [\@\@id1] [\@\@id2]] *)
    }

and class_field_desc =
  | Pcf_inherit of override_flag Ploc.vala * class_expr * string Ploc.vala loc option Ploc.vala
      (** [Pcf_inherit(flag, CE, s)] represents:
            - [inherit CE]
                    when [flag] is {{!Asttypes.override_flag.Fresh}[Fresh]}
                     and [s] is [None],
            - [inherit CE as x]
                   when [flag] is {{!Asttypes.override_flag.Fresh}[Fresh]}
                    and [s] is [Some x],
            - [inherit! CE]
                   when [flag] is {{!Asttypes.override_flag.Override}[Override]}
                    and [s] is [None],
            - [inherit! CE as x]
                   when [flag] is {{!Asttypes.override_flag.Override}[Override]}
                    and [s] is [Some x]
  *)
  | Pcf_val of (label Ploc.vala loc * mutable_flag Ploc.vala * class_field_kind)
      (** [Pcf_val(x,flag, kind)] represents:
            - [val x = E]
       when [flag] is {{!Asttypes.mutable_flag.Immutable}[Immutable]}
        and [kind] is {{!class_field_kind.Cfk_concrete}[Cfk_concrete(Fresh, E)]}
            - [val virtual x: T]
       when [flag] is {{!Asttypes.mutable_flag.Immutable}[Immutable]}
        and [kind] is {{!class_field_kind.Cfk_virtual}[Cfk_virtual(T)]}
            - [val mutable x = E]
       when [flag] is {{!Asttypes.mutable_flag.Mutable}[Mutable]}
        and [kind] is {{!class_field_kind.Cfk_concrete}[Cfk_concrete(Fresh, E)]}
            - [val mutable virtual x: T]
       when [flag] is {{!Asttypes.mutable_flag.Mutable}[Mutable]}
        and [kind] is {{!class_field_kind.Cfk_virtual}[Cfk_virtual(T)]}
  *)
  | Pcf_method of (label Ploc.vala loc * private_flag Ploc.vala * class_field_kind)
      (** - [method x = E]
                        ([E] can be a {{!expression_desc.Pexp_poly}[Pexp_poly]})
            - [method virtual x: T]
                        ([T] can be a {{!core_type_desc.Ptyp_poly}[Ptyp_poly]})
  *)
  | Pcf_constraint of (core_type * core_type)  (** [constraint T1 = T2] *)
  | Pcf_initializer of expression  (** [initializer E] *)
  | Pcf_attribute of attribute  (** [[\@\@\@id]] *)
  | Pcf_extension of extension  (** [[%%id]] *)

and class_field_kind =
  | Cfk_virtual of core_type
  | Cfk_concrete of override_flag Ploc.vala * expression

and class_declaration = class_expr class_infos

(** {1 Module language} *)
(** {2 Type expressions for the module language} *)

and module_type =
    {
     pmty_desc: module_type_desc;
     pmty_loc: Location.t;
     pmty_attributes: attributes;  (** [... [\@id1] [\@id2]] *)
    }

and module_type_desc =
  | Pmty_ident of Longident.t loc  (** [Pmty_ident(S)] represents [S] *)
  | Pmty_signature of signature  (** [sig ... end] *)
  | Pmty_functor of functor_parameter Ploc.vala * module_type
      (** [functor(X : MT1) -> MT2] *)
  | Pmty_with of module_type * with_constraint list Ploc.vala  (** [MT with ...] *)
  | Pmty_typeof of module_expr  (** [module type of ME] *)
  | Pmty_extension of extension  (** [[%id]] *)
  | Pmty_alias of Longident.t Ploc.vala loc  (** [(module M)] *)
(*-*)  | Pmty_xtr of string loc

and functor_parameter =
  | Unit  (** [()] *)
  | Named of string Ploc.vala option Ploc.vala loc * module_type
      (** [Named(name, MT)] represents:
            - [(X : MT)] when [name] is [Some X],
            - [(_ : MT)] when [name] is [None] *)

and signature = signature_item list Ploc.vala

and signature_item =
    {
     psig_desc: signature_item_desc;
     psig_loc: Location.t;
    }

and signature_item_desc =
  | Psig_value of value_description
      (** - [val x: T]
            - [external x: T = "s1" ... "sn"]
         *)
  | Psig_type of rec_flag Ploc.vala * type_declaration list Ploc.vala
      (** [type t1 = ... and ... and tn  = ...] *)
  | Psig_typesubst of type_declaration list Ploc.vala
      (** [type t1 := ... and ... and tn := ...]  *)
  | Psig_typext of type_extension  (** [type t1 += ...] *)
  | Psig_exception of type_exception  (** [exception C of T] *)
  | Psig_module of module_declaration  (** [module X = M] and [module X : MT] *)
  | Psig_modsubst of module_substitution  (** [module X := M] *)
  | Psig_recmodule of module_declaration list Ploc.vala
      (** [module rec X1 : MT1 and ... and Xn : MTn] *)
  | Psig_modtype of module_type_declaration
      (** [module type S = MT] and [module type S] *)
  | Psig_modtypesubst of module_type_declaration
      (** [module type S :=  ...]  *)
  | Psig_open of open_description  (** [open X] *)
  | Psig_include of include_description  (** [include MT] *)
  | Psig_class of class_description list Ploc.vala
      (** [class c1 : ... and ... and cn : ...] *)
  | Psig_class_type of class_type_declaration list Ploc.vala
      (** [class type ct1 = ... and ... and ctn = ...] *)
  | Psig_attribute of attribute  (** [[\@\@\@id]] *)
  | Psig_extension of extension * attributes  (** [[%%id]] *)

and module_declaration =
    {
     pmd_name: string Ploc.vala option Ploc.vala loc;
     pmd_type: module_type;
     pmd_attributes: attributes;  (** [... [\@\@id1] [\@\@id2]] *)
     pmd_loc: Location.t;
    }
(** Values of type [module_declaration] represents [S : MT] *)

and module_substitution =
    {
     pms_name: string Ploc.vala loc;
     pms_manifest: Longident.t Ploc.vala loc;
     pms_attributes: attributes;  (** [... [\@\@id1] [\@\@id2]] *)
     pms_loc: Location.t;
    }
(** Values of type [module_substitution] represents [S := M] *)

and module_type_declaration =
    {
     pmtd_name: string Ploc.vala loc;
     pmtd_type: module_type option Ploc.vala;
     pmtd_attributes: attributes;  (** [... [\@\@id1] [\@\@id2]] *)
     pmtd_loc: Location.t;
    }
(** Values of type [module_type_declaration] represents:
   - [S = MT],
   - [S] for abstract module type declaration,
     when {{!module_type_declaration.pmtd_type}[pmtd_type]} is [None].
*)

and 'a open_infos =
    {
     popen_expr: 'a;
     popen_override: override_flag Ploc.vala;
     popen_loc: Location.t;
     popen_attributes: attributes;
    }
(** Values of type ['a open_infos] represents:
    - [open! X] when {{!open_infos.popen_override}[popen_override]}
                  is {{!Asttypes.override_flag.Override}[Override]}
    (silences the "used identifier shadowing" warning)
    - [open  X] when {{!open_infos.popen_override}[popen_override]}
                  is {{!Asttypes.override_flag.Fresh}[Fresh]}
*)

and open_description = Longident.t Ploc.vala loc open_infos
(** Values of type [open_description] represents:
    - [open M.N]
    - [open M(N).O] *)

and open_declaration = module_expr open_infos
(** Values of type [open_declaration] represents:
    - [open M.N]
    - [open M(N).O]
    - [open struct ... end] *)

and 'a include_infos =
    {
     pincl_mod: 'a;
     pincl_loc: Location.t;
     pincl_attributes: attributes;
    }

and include_description = module_type include_infos
(** Values of type [include_description] represents [include MT] *)

and include_declaration = module_expr include_infos
(** Values of type [include_declaration] represents [include ME] *)

and with_constraint =
  | Pwith_type of Longident.t Ploc.vala loc * type_declaration Ploc.vala
      (** [with type X.t = ...]

            Note: the last component of the longident must match
            the name of the type_declaration. *)
  | Pwith_module of Longident.t Ploc.vala loc * Longident.t Ploc.vala loc
      (** [with module X.Y = Z] *)
  | Pwith_modtype of Longident.t loc * module_type
      (** [with module type X.Y = Z] *)
  | Pwith_modtypesubst of Longident.t loc * module_type
      (** [with module type X.Y := sig end] *)
  | Pwith_typesubst of Longident.t Ploc.vala loc * type_declaration Ploc.vala
      (** [with type X.t := ..., same format as [Pwith_type]] *)
  | Pwith_modsubst of Longident.t Ploc.vala loc * Longident.t Ploc.vala loc
      (** [with module X.Y := Z] *)

(** {2 Value expressions for the module language} *)

and module_expr =
    {
     pmod_desc: module_expr_desc;
     pmod_loc: Location.t;
     pmod_attributes: attributes;  (** [... [\@id1] [\@id2]] *)
    }

and module_expr_desc =
  | Pmod_ident of Longident.t Ploc.vala loc  (** [X] *)
  | Pmod_structure of structure  (** [struct ... end] *)
  | Pmod_functor of functor_parameter Ploc.vala * module_expr
      (** [functor(X : MT1) -> ME] *)
  | Pmod_apply of module_expr * module_expr (** [ME1(ME2)] *)
  | Pmod_apply_unit of module_expr (** [ME1()] *)
  | Pmod_constraint of module_expr * module_type  (** [(ME : MT)] *)
  | Pmod_unpack of expression  (** [(val E)] *)
  | Pmod_extension of extension  (** [[%id]] *)
(*-*)  | Pmod_xtr of string loc

and structure = structure_item list Ploc.vala

and structure_item =
    {
     pstr_desc: structure_item_desc;
     pstr_loc: Location.t;
    }

and structure_item_desc =
  | Pstr_eval of expression Ploc.vala * attributes  (** [E] *)
  | Pstr_value of rec_flag Ploc.vala * value_binding list Ploc.vala
      (** [Pstr_value(rec, [(P1, E1 ; ... ; (Pn, En))])] represents:
            - [let P1 = E1 and ... and Pn = EN]
                when [rec] is {{!Asttypes.rec_flag.Nonrecursive}[Nonrecursive]},
            - [let rec P1 = E1 and ... and Pn = EN ]
                when [rec] is {{!Asttypes.rec_flag.Recursive}[Recursive]}.
        *)
  | Pstr_primitive of value_description
      (** - [val x: T]
            - [external x: T = "s1" ... "sn" ]*)
  | Pstr_type of rec_flag Ploc.vala * type_declaration list Ploc.vala
      (** [type t1 = ... and ... and tn = ...] *)
  | Pstr_typext of type_extension  (** [type t1 += ...] *)
  | Pstr_exception of type_exception
      (** - [exception C of T]
            - [exception C = M.X] *)
  | Pstr_module of module_binding  (** [module X = ME] *)
  | Pstr_recmodule of module_binding list Ploc.vala
      (** [module rec X1 = ME1 and ... and Xn = MEn] *)
  | Pstr_modtype of module_type_declaration  (** [module type S = MT] *)
  | Pstr_open of open_declaration  (** [open X] *)
  | Pstr_class of class_declaration list Ploc.vala
      (** [class c1 = ... and ... and cn = ...] *)
  | Pstr_class_type of class_type_declaration list Ploc.vala
      (** [class type ct1 = ... and ... and ctn = ...] *)
  | Pstr_include of include_declaration  (** [include ME] *)
  | Pstr_attribute of attribute  (** [[\@\@\@id]] *)
  | Pstr_extension of extension * attributes  (** [[%%id]] *)

and value_constraint =
  | Pvc_constraint of {
      locally_abstract_univars:string Ploc.vala loc list Ploc.vala;
      typ:core_type;
    }
  | Pvc_coercion of {ground:core_type option; coercion:core_type }
  (**
     - [Pvc_constraint { locally_abstract_univars=[]; typ}]
         is a simple type constraint on a value binding: [ let x : typ]
     - More generally, in [Pvc_constraint { locally_abstract_univars; typ}]
       [locally_abstract_univars] is the list of locally abstract type
       variables in [ let x: type a ... . typ ]
     - [Pvc_coercion { ground=None; coercion }] represents [let x :> typ]
     - [Pvc_coercion { ground=Some g; coercion }] represents [let x : g :> typ]
  *)

and value_binding =
  {
    pvb_pat: pattern;
    pvb_expr: expression;
    pvb_constraint: value_constraint option;
    pvb_attributes: attributes;
    pvb_loc: Location.t;
  }(** [let pat : type_constraint = exp] *)

and module_binding =
    {
     pmb_name: string Ploc.vala option Ploc.vala loc;
     pmb_expr: module_expr;
     pmb_attributes: attributes;
     pmb_loc: Location.t;
    }
(** Values of type [module_binding] represents [module X = ME] *)

(** {1 Toplevel} *)

(** {2 Toplevel phrases} *)

type toplevel_phrase =
  | Ptop_def of structure
  | Ptop_dir of toplevel_directive  (** [#use], [#load] ... *)

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
