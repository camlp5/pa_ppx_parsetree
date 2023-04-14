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

(** Long identifiers, used in parsetree.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

type t =
    Lident of string Ploc.vala
  | Ldot of t Ploc.vala * string Ploc.vala
  | Lapply of t Ploc.vala * t Ploc.vala

val flatten: t -> string list
val unflatten: string list -> t option
val last: t -> string
(*-*)val last_vala: t -> string Ploc.vala
val parse: string -> t

(** To print a longident, see {!Pprintast.longident}, using
    {!Format.asprintf} to convert to a string. *)
