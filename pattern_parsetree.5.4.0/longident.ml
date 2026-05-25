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
open Location

(*-*)let unvala = Pcaml.unvala
(*-*)let vaval x = Ploc.VaVal x

type t =
    Lident of string Ploc.vala
  | Ldot of t Ploc.vala loc * string Ploc.vala loc
  | Lapply of t Ploc.vala loc * t Ploc.vala loc


let rec same t t' =
  t == t'
  || match t, t' with
  | Lident s, Lident s' ->
      String.equal (unvala s) (unvala s')
  | Ldot ({ txt = t; _ }, { txt = s; _ }),
    Ldot ({ txt = t'; _ }, { txt = s'; _ }) ->
      if String.equal (unvala s) (unvala s') then
        same (unvala t) (unvala t')
      else
        false
  | Lapply ({ txt = tl; _ }, { txt = tr; _ }),
    Lapply ({ txt = tl'; _ }, { txt = tr'; _ }) ->
      same (unvala tl) (unvala tl') && same (unvala tr) (unvala tr')
  | _, _ -> false


let rec flat accu = function
    Lident s -> (unvala s) :: accu
  | Ldot({ txt = lid; _ }, { txt = s; _ }) -> flat ((unvala s) :: accu) (unvala lid)
  | Lapply(_, _) -> Misc.fatal_error "Longident.flat"

let flatten lid = flat [] lid

let last = function
    Lident s -> (unvala s)
  | Ldot(_, s) -> (unvala s.txt)
  | Lapply(_, _) -> Misc.fatal_error "Longident.last"

(*-*)let last_vala = function
(*-*)    Lident s -> s
(*-*)  | Ldot(_, s) -> s.txt
(*-*)  | Lapply(_, _) -> Misc.fatal_error "Longident.last_vala"

let rec split_at_dots s pos =
  try
    let dot = String.index_from s pos '.' in
    String.sub s pos (dot - pos) :: split_at_dots s (dot + 1)
  with Not_found ->
    [String.sub s pos (String.length s - pos)]

let unflatten l =
  match l with
  | [] -> None
  | hd :: tl ->
    Some (List.fold_left (fun p s -> Ldot(mknoloc (vaval p), mknoloc (vaval s)))
                         (Lident (vaval hd)) tl)

let parse s =
  match unflatten (split_at_dots s 0) with
  | None -> Lident (vaval "")  (* should not happen, but don't put assert false
                          so as not to crash the toplevel (see Genprintval) *)
  | Some v -> v
