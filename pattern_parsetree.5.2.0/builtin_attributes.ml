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

open Asttypes
open Parsetree
open Ast_helper


module Attribute_table = Hashtbl.Make (struct
  type t = string with_loc

  let hash : t -> int = Hashtbl.hash
  let equal : t -> t -> bool = (=)
end)
let unused_attrs = Attribute_table.create 128
let mark_used t = Attribute_table.remove unused_attrs t

(* [attr_order] is used to issue unused attribute warnings in the order the
   attributes occur in the file rather than the random order of the hash table
*)
let attr_order a1 a2 =
  match String.compare a1.loc.loc_start.pos_fname a2.loc.loc_start.pos_fname
  with
  | 0 -> Int.compare a1.loc.loc_start.pos_cnum a2.loc.loc_start.pos_cnum
  | n -> n

(* These are the attributes that are tracked in the builtin_attrs table for
   misplaced attribute warnings. *)
let builtin_attrs =
  [ "alert"
  ; "boxed"
  ; "deprecated"
  ; "deprecated_mutable"
  ; "explicit_arity"
  ; "immediate"
  ; "immediate64"
  ; "inline"
  ; "inlined"
  ; "noalloc"
  ; "poll"
  ; "ppwarning"
  ; "specialise"
  ; "specialised"
  ; "tailcall"
  ; "tail_mod_cons"
  ; "unboxed"
  ; "untagged"
  ; "unrolled"
  ; "warnerror"
  ; "warning"
  ; "warn_on_literal_pattern"
  ]

let builtin_attrs =
  let tbl = Hashtbl.create 128 in
  List.iter (fun attr -> Hashtbl.add tbl attr ()) builtin_attrs;
  tbl

type current_phase = Parser | Invariant_check

let string_of_cst = function
  | Pconst_string(s, _, _) -> Some (unvala s)
  | _ -> None

let string_of_payload = function
  | PStr(Ploc.VaVal [{pstr_desc=Pstr_eval(Ploc.VaVal {pexp_desc=Pexp_constant c},_)}]) ->
      string_of_cst (unvala c)
  | _ -> None

let string_of_opt_payload p =
  match string_of_payload p with
  | Some s -> s
  | None -> ""

let error_of_extension ext =
  let submessage_from main_loc main_txt = function
    | {pstr_desc=Pstr_extension
           (({txt = (Ploc.VaVal ("ocaml.error"|"error")); loc}, p), _)} ->
        begin match p with
        | PStr(Ploc.VaVal [{pstr_desc=Pstr_eval
                     (Ploc.VaVal {pexp_desc=Pexp_constant(Ploc.VaVal (Pconst_string(msg,_,_)))}, _)}
               ]) ->
            { Location.loc; txt = fun ppf -> Format.pp_print_text ppf (unvala msg) }
        | _ ->
            { Location.loc; txt = fun ppf ->
                Format.fprintf ppf
                  "Invalid syntax for sub-message of extension '%s'." main_txt }
        end
    | {pstr_desc=Pstr_extension (({txt; loc}, _), _)} ->
        { Location.loc; txt = fun ppf ->
            Format.fprintf ppf "Uninterpreted extension '%s'." (unvala txt) }
    | _ ->
        { Location.loc = main_loc; txt = fun ppf ->
            Format.fprintf ppf
              "Invalid syntax for sub-message of extension '%s'." main_txt }
  in
  match ext with
  | ({txt = Ploc.VaVal (("ocaml.error"|"error") as txt); loc}, p) ->
      begin match p with
      | PStr (Ploc.VaVal []) -> raise Location.Already_displayed_error
      | PStr(Ploc.VaVal ({pstr_desc=Pstr_eval
                  (Ploc.VaVal {pexp_desc=Pexp_constant(Ploc.VaVal (Pconst_string(msg,_,_)))}, _)}::
             inner)) ->
          let sub = List.map (submessage_from loc txt) inner in
          Location.error_of_printer ~loc ~sub Format.pp_print_text (unvala msg)
      | _ ->
          Location.errorf ~loc "Invalid syntax for extension '%s'." txt
      end
  | ({txt; loc}, _) ->
      Location.errorf ~loc "Uninterpreted extension '%s'." (unvala txt)

let mark_payload_attrs_used payload =
  let iter =
    { Ast_iterator.default_iterator
      with attribute = fun self a ->
        mark_used (loc_map unvala a.attr_name);
        Ast_iterator.default_iterator.attribute self a
    }
  in
  iter.payload iter payload

let kind_and_message = function
  | PStr(Ploc.VaVal [
      {pstr_desc=
         Pstr_eval
           (Ploc.VaVal {pexp_desc=Pexp_apply
                 ({pexp_desc=Pexp_ident{txt=Ploc.VaVal (Longident.Lident id)}},
                  (Ploc.VaVal [Nolabel,{pexp_desc=Pexp_constant (Ploc.VaVal (Pconst_string(s,_,_)))}]))
            },_)}]) ->
      Some (id, s)
  | PStr(Ploc.VaVal [
      {pstr_desc=
         Pstr_eval
           (Ploc.VaVal {pexp_desc=Pexp_ident{txt=Ploc.VaVal (Longident.Lident id)}},_)}]) ->
      Some (id, Ploc.VaVal "")
  | _ -> None

let cat s1 s2 =
  if s2 = "" then s1 else s1 ^ "\n" ^ s2

let rec attrs_of_sig = function
  | {psig_desc = Psig_attribute a} :: tl ->
      a :: attrs_of_sig tl
  | _ ->
      []

let rec attrs_of_str = function
  | {pstr_desc = Pstr_attribute a} :: tl ->
      a :: attrs_of_str tl
  | _ ->
      []
