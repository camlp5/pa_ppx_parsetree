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

(** Source code locations (ranges of positions), used in parsetree.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

open Format

type t = Warnings.loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
  }
(** [t] represents a range of characters in the source code.

    loc_ghost=false whenever the AST described by the location can be parsed
    from the location. In all other cases, loc_ghost must be true. Most
    locations produced by the parser have loc_ghost=false.
    When loc_ghost=true, the location is usually a best effort approximation.

    This info is used by tools like merlin that want to relate source code with
    parsetrees or later asts. ocamlprof skips instrumentation of ghost nodes.

    Example: in `let f x = x`, we have:
    - a structure item at location "let f x = x"
    - a pattern "f" at location "f"
    - an expression "fun x -> x" at location "x = x" with loc_ghost=true
    - a pattern "x" at location "x"
    - an expression "x" at location "x"
    In this case, every node has loc_ghost=false, except the node "fun x -> x",
    since [Parser.expression (Lexing.from_string "x = x")] would fail to parse.
    By contrast, in `let f = fun x -> x`, every node has loc_ghost=false.

    Line directives can modify the filenames and line numbers arbitrarily,
    which is orthogonal to loc_ghost, which describes the range of characters
    from loc_start.pos_cnum to loc_end.pos_cnum in the parsed string.
 *)

(** Note on the use of Lexing.position in this module.
   If [pos_fname = ""], then use [!input_name] instead.
   If [pos_lnum = -1], then [pos_bol = 0]. Use [pos_cnum] and
     re-parse the file to get the line and character numbers.
   Else all fields are correct.
*)

val none : t
(** An arbitrary value of type [t]; describes an empty ghost range. *)

val is_none : t -> bool
(** True for [Location.none], false any other location *)

val in_file : string -> t
(** Return an empty ghost range located in a given file. *)

val init : Lexing.lexbuf -> string -> unit
(** Set the file name and line number of the [lexbuf] to be the start
    of the named file. *)

val curr : Lexing.lexbuf -> t
(** Get the location of the current token from the [lexbuf]. *)

val symbol_rloc: unit -> t
val symbol_gloc: unit -> t

(** [rhs_loc n] returns the location of the symbol at position [n], starting
  at 1, in the current parser rule. *)
val rhs_loc: int -> t

val rhs_interval: int -> int -> t

val get_pos_info: Lexing.position -> string * int * int
(** file, line, char *)

type 'a loc = {
  txt : 'a;
  loc : t;
}

val mknoloc : 'a -> 'a loc
val mkloc : 'a -> t -> 'a loc
val map : ('a -> 'b) -> 'a loc -> 'b loc


(** {1 Input info} *)

val input_name: string ref
val input_lexbuf: Lexing.lexbuf option ref

(* This is used for reporting errors coming from the toplevel.

   When running a toplevel session (i.e. when [!input_name] is "//toplevel//"),
   [!input_phrase_buffer] should be [Some buf] where [buf] contains the last
   toplevel phrase. *)
val input_phrase_buffer: Buffer.t option ref


(** {1 Toplevel-specific functions} *)

val echo_eof: unit -> unit
val reset: unit -> unit


(** {1 Rewriting path } *)

val absolute_path: string -> string
 (** [absolute_path path] first makes an absolute path, [s] from [path],
     prepending the current working directory if [path] was relative.
     Then [s] is rewritten using [rewrite_absolute_path].
     Finally the result is normalized by eliminating instances of
     ['.'] or ['..']. *)

(** {1 Printing locations} *)

val show_filename: string -> string
    (** In -absname mode, return the absolute path for this filename.
        Otherwise, returns the filename unchanged. *)

val print_filename: formatter -> string -> unit
val print_loc: formatter -> t -> unit
val print_locs: formatter -> t list -> unit
val separate_new_message: formatter -> unit

module Doc: sig
  val separate_new_message: unit Format_doc.printer
  val filename: string Format_doc.printer
  val quoted_filename: string Format_doc.printer
  val loc: t Format_doc.printer
  val locs: t list Format_doc.printer
end

(** {1 Toplevel-specific location highlighting} *)

val highlight_terminfo:
  Lexing.lexbuf -> formatter -> t list -> unit


(** {1 Reporting errors and warnings} *)

(** {2 The type of reports and report printers} *)

type msg = Format_doc.t loc

val msg: ?loc:t -> ('a, Format_doc.formatter, unit, msg) format4 -> 'a

type report_kind =
  | Report_error
  | Report_warning of string
  | Report_warning_as_error of string
  | Report_alert of string
  | Report_alert_as_error of string

type report = {
  kind : report_kind;
  main : msg;
  sub : msg list;
  footnote: Format_doc.t option
}

type report_printer = {
  (* The entry point *)
  pp : report_printer ->
    Format.formatter -> report -> unit;

  pp_report_kind : report_printer -> report ->
    Format.formatter -> report_kind -> unit;
  pp_main_loc : report_printer -> report ->
    Format.formatter -> t -> unit;
  pp_main_txt : report_printer -> report ->
    Format.formatter -> Format_doc.t -> unit;
  pp_submsgs : report_printer -> report ->
    Format.formatter -> msg list -> unit;
  pp_submsg : report_printer -> report ->
    Format.formatter -> msg -> unit;
  pp_submsg_loc : report_printer -> report ->
    Format.formatter -> t -> unit;
  pp_submsg_txt : report_printer -> report ->
    Format.formatter -> Format_doc.t -> unit;
}
(** A printer for [report]s, defined using open-recursion.
    The goal is to make it easy to define new printers by reusing code from
    existing ones.
*)

(** {2 Report printers used in the compiler} *)

(** {2 Printing a [report]} *)

(** {1 Reporting warnings} *)

(** {2 Converting a [Warnings.t] into a [report]} *)

(** {1 Reporting alerts} *)

(** {1 Reporting errors} *)

type error = report
(** An [error] is a [report] which [report_kind] must be [Report_error]. *)

type delayed_msg = unit -> Format_doc.t option

val error: ?loc:t -> ?sub:msg list -> ?footnote:delayed_msg -> string -> error

val errorf: ?loc:t -> ?sub:msg list -> ?footnote:delayed_msg ->
  ('a, Format_doc.formatter, unit, error) format4 -> 'a

val aligned_error_hint:
  ?loc:t -> ?sub:msg list -> ?footnote:delayed_msg ->
  ('a, Format_doc.formatter, unit, Format_doc.t option ->  error) format4 -> 'a
(** [aligned_error_hint ?loc ?sub ?footnote fmt ... aligned_hint] produces an
    error report where the potential [aligned_hint] message has been aligned
    with the main error message before being added to the list of submessages.*)

val error_of_printer: ?loc:t -> ?sub:msg list -> ?footnote:delayed_msg ->
  (Format_doc.formatter -> 'a -> unit) -> 'a -> error

val error_of_printer_file: (Format_doc.formatter -> 'a -> unit) -> 'a -> error


(** {1 Automatically reporting errors for raised exceptions} *)

val register_error_of_exn: (exn -> error option) -> unit
(** Each compiler module which defines a custom type of exception
    which can surface as a user-visible error should register
    a "printer" for this exception using [register_error_of_exn].
    The result of the printer is an [error] value containing
    a location, a message, and optionally sub-messages (each of them
    being located as well). *)

val error_of_exn: exn -> [ `Ok of error | `Already_displayed ] option

exception Error of error
(** Raising [Error e] signals an error [e]; the exception will be caught and the
   error will be printed. *)

exception Already_displayed_error
(** Raising [Already_displayed_error] signals an error which has already been
   printed. The exception will be caught, but nothing will be printed *)

val raise_errorf: ?loc:t -> ?sub:msg list -> ?footnote:delayed_msg ->
  ('a, Format_doc.formatter, unit, 'b) format4 -> 'a

