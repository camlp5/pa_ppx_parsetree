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

(** Support for the builtin attributes:

    - ocaml.alert
    - ocaml.boxed
    - ocaml.deprecated
    - ocaml.deprecated_mutable
    - ocaml.explicit_arity
    - ocaml.immediate
    - ocaml.immediate64
    - ocaml.inline
    - ocaml.inlined
    - ocaml.noalloc
    - ocaml.poll
    - ocaml.ppwarning
    - ocaml.specialise
    - ocaml.specialised
    - ocaml.tailcall
    - ocaml.tail_mod_cons
    - ocaml.unboxed
    - ocaml.untagged
    - ocaml.unrolled
    - ocaml.warnerror
    - ocaml.warning
    - ocaml.warn_on_literal_pattern

    {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

(** {2 Attribute tracking for warning 53} *)

(** [register_attr] must be called on the locations of all attributes that
    should be tracked for the purpose of misplaced attribute warnings.  In
    particular, it should be called on all attributes that are present in the
    source program except those that are contained in the payload of another
    attribute (because these may be left behind by a ppx and intentionally
    ignored by the compiler).

    The [current_phase] argument indicates when this function is being called
    - either when an attribute is created in the parser or when we see an
    attribute while running the check in the [Ast_invariants] module.  This is
    used to ensure that we track only attributes from the final version of the
    parse tree: we skip adding attributes seen at parse time if we can see that
    a ppx will be run later, because the [Ast_invariants] check is always run on
    the result of a ppx.

    Note that the [Ast_invariants] check is also run on parse trees created from
    marshalled ast files if no ppx is being used, ensuring we don't miss
    attributes in that case.
*)
type current_phase = Parser | Invariant_check

(** Marks the attributes hiding in the payload of another attribute used, for
    the purposes of misplaced attribute warnings (see comment on
    [current_phase] above).  In the parser, it's simplest to add these to
    the table and remove them later, rather than threading through state
    tracking whether we're in an attribute payload. *)
val mark_payload_attrs_used : Parsetree.payload -> unit

(** {3 Warning 53 helpers for environment attributes}

    Some attributes, like deprecation markers, do not affect the compilation of
    the definition on which they appear, but rather result in warnings on future
    uses of that definition.  This is implemented by moving the raw attributes
    into the environment, where they will be noticed on future accesses.

    To make misplaced attribute warnings work appropriately for these
    attributes, we mark them "used" when they are moved into the environment.
    This is done with the helper functions in this section.
*)

(** {2 Helpers for alert and warning attributes} *)

val error_of_extension: Parsetree.extension -> Location.error

