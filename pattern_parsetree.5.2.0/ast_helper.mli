(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helpers to produce Parsetree fragments

  {b Warning} This module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

open Asttypes
open Docstrings
open Parsetree

type 'a with_loc = 'a Location.loc
type loc = Location.t

type lid = Longident.t with_loc
(*-*)type lid_vala = Longident.t Ploc.vala with_loc
type str = string with_loc
(*-*)type str_vala = string Ploc.vala with_loc
type str_opt = string option with_loc
(*-*)type str_vala_opt_vala = string Ploc.vala option Ploc.vala with_loc
type attrs = attribute list Ploc.vala
(*-*)
(*-*)type 'a vala = 'a Ploc.vala =
(*-*)   VaAnt of string
(*-*)  | VaVal of 'a

(*-*)val vaval : 'a -> 'a vala
(*-*)val vaant : string -> 'a vala
(*-*)val unvala : 'a Ploc.vala -> 'a
(*-*)val append_list_vala : 'a list vala -> 'a list vala -> 'a list vala
(*-*)
(** {1 Default locations} *)

val default_loc: loc ref
    (** Default value for all optional location arguments. *)

val with_default_loc: loc -> (unit -> 'a) -> 'a
    (** Set the [default_loc] within the scope of the execution
        of the provided function. *)

(*-*)val loc_map : ('a -> 'b) -> 'a Location.loc -> 'b Location.loc
(*-*)
(** {1 Constants} *)

module Const : sig
  val char : char -> constant
  val string :
    ?quotation_delimiter:string -> ?loc:Location.t -> string -> constant
  val integer : ?suffix:char -> string -> constant
  val int : ?suffix:char -> int -> constant
  val int32 : ?suffix:char -> int32 -> constant
  val int64 : ?suffix:char -> int64 -> constant
  val nativeint : ?suffix:char -> nativeint -> constant
  val float : ?suffix:char -> string -> constant
end

(** {1 Attributes} *)
module Attr : sig
  val mk: ?loc:loc -> str_vala -> payload -> attribute
end

(** {1 Core language} *)

(** Type expressions *)
module Typ :
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> core_type_desc -> core_type
    val attr: core_type -> attribute -> core_type
(*-*)    val attrs: core_type -> attrs -> core_type

    val any: ?loc:loc -> ?attrs:attrs -> unit -> core_type
    val var: ?loc:loc -> ?attrs:attrs -> string Ploc.vala -> core_type
    val arrow: ?loc:loc -> ?attrs:attrs -> arg_label Ploc.vala -> core_type -> core_type
               -> core_type
    val tuple: ?loc:loc -> ?attrs:attrs -> core_type list Ploc.vala -> core_type
    val constr: ?loc:loc -> ?attrs:attrs -> lid_vala -> core_type list Ploc.vala -> core_type
    val object_: ?loc:loc -> ?attrs:attrs -> object_field list Ploc.vala
                   -> closed_flag Ploc.vala -> core_type
    val class_: ?loc:loc -> ?attrs:attrs -> lid -> core_type list Ploc.vala -> core_type
    val alias: ?loc:loc -> ?attrs:attrs -> core_type -> string Ploc.vala with_loc
               -> core_type
    val variant: ?loc:loc -> ?attrs:attrs -> row_field list Ploc.vala -> closed_flag Ploc.vala
                 -> label list Ploc.vala option Ploc.vala -> core_type
    val poly: ?loc:loc -> ?attrs:attrs -> str_vala list Ploc.vala -> core_type -> core_type
    val package: ?loc:loc -> ?attrs:attrs -> lid_vala -> (lid_vala * core_type) list Ploc.vala
                 -> core_type
    val open_ : ?loc:loc -> ?attrs:attrs -> lid_vala -> core_type -> core_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> core_type

    val force_poly: core_type -> core_type

    val varify_constructors: str_vala list -> core_type -> core_type
    (** [varify_constructors newtypes te] is type expression [te], of which
        any of nullary type constructor [tc] is replaced by type variable of
        the same name, if [tc]'s name appears in [newtypes].
        Raise [Syntaxerr.Variable_in_scope] if any type variable inside [te]
        appears in [newtypes].
        @since 4.05
     *)
  end

(** Patterns *)
module Pat:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> pattern_desc -> pattern
    val attr:pattern -> attribute -> pattern
(*-*)    val attrs:pattern -> attributes -> pattern

    val any: ?loc:loc -> ?attrs:attrs -> unit -> pattern
    val var: ?loc:loc -> ?attrs:attrs -> str_vala -> pattern
    val alias: ?loc:loc -> ?attrs:attrs -> pattern -> str_vala -> pattern
    val constant: ?loc:loc -> ?attrs:attrs -> constant Ploc.vala -> pattern
    val interval: ?loc:loc -> ?attrs:attrs -> constant Ploc.vala -> constant Ploc.vala -> pattern
    val tuple: ?loc:loc -> ?attrs:attrs -> pattern list Ploc.vala -> pattern
    val construct: ?loc:loc -> ?attrs:attrs ->
      lid_vala -> (str_vala list Ploc.vala * pattern) option Ploc.vala -> pattern
    val variant: ?loc:loc -> ?attrs:attrs -> label Ploc.vala -> pattern option Ploc.vala -> pattern
    val record: ?loc:loc -> ?attrs:attrs -> (lid_vala * pattern) list Ploc.vala -> closed_flag Ploc.vala
                -> pattern
    val array: ?loc:loc -> ?attrs:attrs -> pattern list Ploc.vala -> pattern
    val or_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern -> pattern
    val constraint_: ?loc:loc -> ?attrs:attrs -> pattern -> core_type -> pattern
    val type_: ?loc:loc -> ?attrs:attrs -> lid_vala -> pattern
    val lazy_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
    val unpack: ?loc:loc -> ?attrs:attrs -> str_vala_opt_vala -> pattern
    val open_: ?loc:loc -> ?attrs:attrs  -> lid_vala -> pattern -> pattern
    val exception_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> pattern
  end

(** Expressions *)
module Exp:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> expression_desc -> expression
    val attr: expression -> attribute -> expression
(*-*)    val attrs: expression -> attrs -> expression

    val ident: ?loc:loc -> ?attrs:attrs -> lid_vala -> expression
    val constant: ?loc:loc -> ?attrs:attrs -> constant Ploc.vala -> expression
    val let_: ?loc:loc -> ?attrs:attrs -> rec_flag Ploc.vala -> value_binding list Ploc.vala
              -> expression -> expression
    val function_ : ?loc:loc -> ?attrs:attrs -> function_param list Ploc.vala
                   -> type_constraint Ploc.vala option Ploc.vala -> function_body
                   -> expression
    val apply: ?loc:loc -> ?attrs:attrs -> expression
               -> (arg_label * expression) list Ploc.vala -> expression
    val match_: ?loc:loc -> ?attrs:attrs -> expression -> case list Ploc.vala
                -> expression
    val try_: ?loc:loc -> ?attrs:attrs -> expression -> case list Ploc.vala -> expression
    val tuple: ?loc:loc -> ?attrs:attrs -> expression list Ploc.vala -> expression
    val construct: ?loc:loc -> ?attrs:attrs -> lid_vala -> expression option Ploc.vala
                   -> expression
    val variant: ?loc:loc -> ?attrs:attrs -> label Ploc.vala -> expression option Ploc.vala
                 -> expression
    val record: ?loc:loc -> ?attrs:attrs -> (lid_vala * expression) list Ploc.vala
                -> expression option Ploc.vala -> expression
    val field: ?loc:loc -> ?attrs:attrs -> expression -> lid_vala -> expression
    val setfield: ?loc:loc -> ?attrs:attrs -> expression -> lid_vala -> expression
                  -> expression
    val array: ?loc:loc -> ?attrs:attrs -> expression list Ploc.vala -> expression
    val ifthenelse: ?loc:loc -> ?attrs:attrs -> expression -> expression
                    -> expression option Ploc.vala -> expression
    val sequence: ?loc:loc -> ?attrs:attrs -> expression -> expression
                  -> expression
    val while_: ?loc:loc -> ?attrs:attrs -> expression -> expression
                -> expression
    val for_: ?loc:loc -> ?attrs:attrs -> pattern -> expression -> expression
              -> direction_flag Ploc.vala -> expression -> expression
    val coerce: ?loc:loc -> ?attrs:attrs -> expression -> core_type option Ploc.vala
                -> core_type -> expression
    val constraint_: ?loc:loc -> ?attrs:attrs -> expression -> core_type
                     -> expression
    val send: ?loc:loc -> ?attrs:attrs -> expression -> str_vala -> expression
    val new_: ?loc:loc -> ?attrs:attrs -> lid_vala -> expression
    val setinstvar: ?loc:loc -> ?attrs:attrs -> str_vala -> expression -> expression
    val override: ?loc:loc -> ?attrs:attrs -> (str_vala * expression) list Ploc.vala
                  -> expression
    val letmodule: ?loc:loc -> ?attrs:attrs -> str_vala_opt_vala -> module_expr
                   -> expression -> expression
    val letexception:
      ?loc:loc -> ?attrs:attrs -> extension_constructor Ploc.vala -> expression
      -> expression
    val assert_: ?loc:loc -> ?attrs:attrs -> expression -> expression
    val lazy_: ?loc:loc -> ?attrs:attrs -> expression -> expression
    val poly: ?loc:loc -> ?attrs:attrs -> expression -> core_type option
              -> expression
    val object_: ?loc:loc -> ?attrs:attrs -> class_structure -> expression
    val newtype: ?loc:loc -> ?attrs:attrs -> str_vala -> expression -> expression
    val pack: ?loc:loc -> ?attrs:attrs -> module_expr -> expression
    val open_: ?loc:loc -> ?attrs:attrs -> open_declaration -> expression
               -> expression
    val letop: ?loc:loc -> ?attrs:attrs -> binding_op Ploc.vala
               -> binding_op list Ploc.vala -> expression -> expression
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> expression
    val unreachable: ?loc:loc -> ?attrs:attrs -> unit -> expression

    val case: pattern Ploc.vala -> guard:expression Ploc.vala option Ploc.vala -> expression Ploc.vala -> case
    val binding_op: str_vala -> pattern -> expression -> loc -> binding_op
  end

(** Value declarations *)
module Val:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      ?prim:string list Ploc.vala -> str_vala -> core_type -> value_description
  end

(** Type declarations *)
module Type:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?params:(core_type * (variance * injectivity)) list Ploc.vala ->
      ?cstrs:(core_type * core_type * loc) list Ploc.vala ->
      ?kind:type_kind -> ?priv:private_flag Ploc.vala -> manifest:core_type Ploc.vala option Ploc.vala -> str_vala ->
      type_declaration

    val constructor: ?loc:loc -> ?attrs:attrs -> ?info:info ->
      ?vars:str_vala list Ploc.vala -> ?args:constructor_arguments -> res:core_type option Ploc.vala ->
      str_vala ->
      constructor_declaration
    val field: ?loc:loc -> ?attrs:attrs -> ?info:info ->
      ?mut:mutable_flag Ploc.vala -> str_vala -> core_type Ploc.vala -> label_declaration
  end

(** Type extensions *)
module Te:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      ?params:(core_type * (variance * injectivity)) list Ploc.vala ->
      ?priv:private_flag Ploc.vala -> lid_vala -> extension_constructor list Ploc.vala -> type_extension

    val mk_exception: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      extension_constructor -> type_exception

    val constructor: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      str_vala -> extension_constructor_kind -> extension_constructor

    val decl: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      ?vars:str_vala list Ploc.vala -> ?args:constructor_arguments -> res:core_type option Ploc.vala ->
      str_vala ->
      extension_constructor
    val rebind: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      str_vala -> lid_vala -> extension_constructor
  end

(** {1 Module language} *)

(** Module type expressions *)
module Mty:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> module_type_desc -> module_type
    val attr: module_type -> attribute -> module_type
(*-*)    val attrs: module_type -> attrs -> module_type

    val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_type
    val alias: ?loc:loc -> ?attrs:attrs -> lid_vala -> module_type
    val signature: ?loc:loc -> ?attrs:attrs -> signature -> module_type
    val functor_: ?loc:loc -> ?attrs:attrs ->
      functor_parameter Ploc.vala -> module_type -> module_type
    val with_: ?loc:loc -> ?attrs:attrs -> module_type ->
      with_constraint list Ploc.vala -> module_type
    val typeof_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_type
  end

(** Module expressions *)
module Mod:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> module_expr_desc -> module_expr
    val attr: module_expr -> attribute -> module_expr
(*-*)    val attrs: module_expr -> attrs -> module_expr

    val ident: ?loc:loc -> ?attrs:attrs -> lid_vala -> module_expr
    val structure: ?loc:loc -> ?attrs:attrs -> structure -> module_expr
    val functor_: ?loc:loc -> ?attrs:attrs ->
      functor_parameter Ploc.vala -> module_expr -> module_expr
    val apply: ?loc:loc -> ?attrs:attrs -> module_expr -> module_expr ->
      module_expr
    val apply_unit: ?loc:loc -> ?attrs:attrs -> module_expr -> module_expr
    val constraint_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type ->
      module_expr
    val unpack: ?loc:loc -> ?attrs:attrs -> expression -> module_expr
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_expr
  end

(** Signature items *)
module Sig:
  sig
    val mk: ?loc:loc -> signature_item_desc -> signature_item

    val value: ?loc:loc -> value_description -> signature_item
    val type_: ?loc:loc -> rec_flag Ploc.vala -> type_declaration list Ploc.vala -> signature_item
    val type_subst: ?loc:loc -> type_declaration list Ploc.vala -> signature_item
    val type_extension: ?loc:loc -> type_extension -> signature_item
    val exception_: ?loc:loc -> type_exception -> signature_item
    val module_: ?loc:loc -> module_declaration -> signature_item
    val mod_subst: ?loc:loc -> module_substitution -> signature_item
    val rec_module: ?loc:loc -> module_declaration list Ploc.vala -> signature_item
    val modtype: ?loc:loc -> module_type_declaration -> signature_item
    val modtype_subst: ?loc:loc -> module_type_declaration -> signature_item
    val open_: ?loc:loc -> open_description -> signature_item
    val include_: ?loc:loc -> include_description -> signature_item
    val class_: ?loc:loc -> class_description list Ploc.vala -> signature_item
    val class_type: ?loc:loc -> class_type_declaration list Ploc.vala -> signature_item
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> signature_item
    val attribute: ?loc:loc -> attribute -> signature_item
    val text: text -> signature_item list
  end

(** Structure items *)
module Str:
  sig
    val mk: ?loc:loc -> structure_item_desc -> structure_item

    val eval: ?loc:loc -> ?attrs:attributes -> expression Ploc.vala -> structure_item
    val value: ?loc:loc -> rec_flag Ploc.vala -> value_binding list Ploc.vala -> structure_item
    val primitive: ?loc:loc -> value_description -> structure_item
    val type_: ?loc:loc -> rec_flag Ploc.vala -> type_declaration list Ploc.vala -> structure_item
    val type_extension: ?loc:loc -> type_extension -> structure_item
    val exception_: ?loc:loc -> type_exception -> structure_item
    val module_: ?loc:loc -> module_binding -> structure_item
    val rec_module: ?loc:loc -> module_binding list Ploc.vala -> structure_item
    val modtype: ?loc:loc -> module_type_declaration -> structure_item
    val open_: ?loc:loc -> open_declaration -> structure_item
    val class_: ?loc:loc -> class_declaration list Ploc.vala -> structure_item
    val class_type: ?loc:loc -> class_type_declaration list Ploc.vala -> structure_item
    val include_: ?loc:loc -> include_declaration -> structure_item
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> structure_item
    val attribute: ?loc:loc -> attribute -> structure_item
    val text: text -> structure_item list
  end

(** Module declarations *)
module Md:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      str_vala_opt_vala -> module_type -> module_declaration
  end

(** Module substitutions *)
module Ms:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      str_vala -> lid_vala -> module_substitution
  end

(** Module type declarations *)
module Mtd:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      typ:module_type option Ploc.vala -> str_vala -> module_type_declaration
  end

(** Module bindings *)
module Mb:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      str_vala_opt_vala -> module_expr -> module_binding
  end

(** Opens *)
module Opn:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs ->
      ?override:override_flag Ploc.vala -> 'a -> 'a open_infos
  end

(** Includes *)
module Incl:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> 'a -> 'a include_infos
  end

(** Value bindings *)
module Vb:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?value_constraint:value_constraint -> pattern -> expression ->
      value_binding
  end


(** {1 Class language} *)

(** Class type expressions *)
module Cty:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> class_type_desc -> class_type
    val attr: class_type -> attribute -> class_type
(*-*)    val attrs: class_type -> attrs -> class_type

    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list Ploc.vala -> class_type
    val signature: ?loc:loc -> ?attrs:attrs -> class_signature -> class_type
    val arrow: ?loc:loc -> ?attrs:attrs -> arg_label Ploc.vala -> core_type ->
      class_type -> class_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type
    val open_: ?loc:loc -> ?attrs:attrs -> open_description -> class_type
               -> class_type
  end

(** Class type fields *)
module Ctf:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      class_type_field_desc -> class_type_field
    val attr: class_type_field -> attribute -> class_type_field

    val inherit_: ?loc:loc -> ?attrs:attrs -> class_type -> class_type_field
    val val_: ?loc:loc -> ?attrs:attrs -> str_vala -> mutable_flag Ploc.vala ->
      virtual_flag Ploc.vala -> core_type -> class_type_field
    val method_: ?loc:loc -> ?attrs:attrs -> str_vala -> private_flag Ploc.vala ->
      virtual_flag Ploc.vala -> core_type -> class_type_field
    val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type ->
      class_type_field
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type_field
    val attribute: ?loc:loc -> attribute -> class_type_field
    val text: text -> class_type_field list
  end

(** Class expressions *)
module Cl:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> class_expr_desc -> class_expr
    val attr: class_expr -> attribute -> class_expr
(*-*)    val attrs: class_expr -> attrs -> class_expr

    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list Ploc.vala -> class_expr
    val structure: ?loc:loc -> ?attrs:attrs -> class_structure -> class_expr
    val fun_: ?loc:loc -> ?attrs:attrs -> arg_label Ploc.vala -> expression option Ploc.vala ->
      pattern -> class_expr -> class_expr
    val apply: ?loc:loc -> ?attrs:attrs -> class_expr ->
      (arg_label * expression) list Ploc.vala -> class_expr
    val let_: ?loc:loc -> ?attrs:attrs -> rec_flag Ploc.vala -> value_binding list Ploc.vala ->
      class_expr -> class_expr
    val constraint_: ?loc:loc -> ?attrs:attrs -> class_expr -> class_type ->
      class_expr
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_expr
    val open_: ?loc:loc -> ?attrs:attrs -> open_description -> class_expr
               -> class_expr
  end

(** Class fields *)
module Cf:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> class_field_desc ->
      class_field
    val attr: class_field -> attribute -> class_field

    val inherit_: ?loc:loc -> ?attrs:attrs -> override_flag Ploc.vala -> class_expr ->
      str_vala option Ploc.vala -> class_field
    val val_: ?loc:loc -> ?attrs:attrs -> str_vala -> mutable_flag Ploc.vala ->
      class_field_kind -> class_field
    val method_: ?loc:loc -> ?attrs:attrs -> str_vala -> private_flag Ploc.vala ->
      class_field_kind -> class_field
    val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type ->
      class_field
    val initializer_: ?loc:loc -> ?attrs:attrs -> expression -> class_field
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_field
    val attribute: ?loc:loc -> attribute -> class_field
    val text: text -> class_field list

    val virtual_: core_type -> class_field_kind
    val concrete: override_flag Ploc.vala -> expression -> class_field_kind

  end

(** Classes *)
module Ci:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?virt:virtual_flag Ploc.vala ->
      ?params:(core_type * (variance * injectivity)) list Ploc.vala ->
      str_vala -> 'a -> 'a class_infos
  end

(** Class signatures *)
module Csig:
  sig
    val mk: core_type -> class_type_field list Ploc.vala -> class_signature
  end

(** Class structures *)
module Cstr:
  sig
    val mk: pattern Ploc.vala -> class_field list Ploc.vala -> class_structure
  end

(** Row fields *)
module Rf:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> row_field_desc -> row_field
    val tag: ?loc:loc -> ?attrs:attrs ->
      label Ploc.vala with_loc -> bool Ploc.vala -> core_type list Ploc.vala -> row_field
    val inherit_: ?loc:loc -> core_type -> row_field
  end

(** Object fields *)
module Of:
  sig
    val mk: ?loc:loc -> ?attrs:attrs ->
      object_field_desc -> object_field
    val tag: ?loc:loc -> ?attrs:attrs ->
      label Ploc.vala with_loc -> core_type -> object_field
    val inherit_: ?loc:loc -> core_type -> object_field
  end
