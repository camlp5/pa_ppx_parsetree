(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Weis && Damien Doligez, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* When you change this, you need to update:
   - the list 'description' at the bottom of this file
   - man/ocamlc.m
*)

type loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

type field_usage_warning =
  | Unused
  | Not_read
  | Not_mutated

type constructor_usage_warning =
  | Unused
  | Not_constructed
  | Only_exported_private

type t =
  | Comment_start                           (*  1 *)
  | Comment_not_end                         (*  2 *)
(*| Deprecated --> alert "deprecated" *)    (*  3 *)
  | Fragile_match of string                 (*  4 *)
  | Ignored_partial_application             (*  5 *)
  | Labels_omitted of string list           (*  6 *)
  | Method_override of string list          (*  7 *)
  | Partial_match of string                 (*  8 *)
  | Missing_record_field_pattern of string  (*  9 *)
  | Non_unit_statement                      (* 10 *)
  | Redundant_case                          (* 11 *)
  | Redundant_subpat                        (* 12 *)
  | Instance_variable_override of string list (* 13 *)
  | Illegal_backslash                       (* 14 *)
  | Implicit_public_methods of string list  (* 15 *)
  | Unerasable_optional_argument            (* 16 *)
  | Undeclared_virtual_method of string     (* 17 *)
  | Not_principal of string                 (* 18 *)
  | Non_principal_labels of string          (* 19 *)
  | Ignored_extra_argument                  (* 20 *)
  | Nonreturning_statement                  (* 21 *)
  | Preprocessor of string                  (* 22 *)
  | Useless_record_with                     (* 23 *)
  | Bad_module_name of string               (* 24 *)
  | All_clauses_guarded                     (* 8, used to be 25 *)
  | Unused_var of string                    (* 26 *)
  | Unused_var_strict of string             (* 27 *)
  | Wildcard_arg_to_constant_constr         (* 28 *)
  | Eol_in_string                           (* 29 *)
  | Duplicate_definitions of string * string * string * string (*30 *)
  (* [Module_linked_twice of string * string * string] (* 31 *)
     was turned into a hard error *)
  | Unused_value_declaration of string      (* 32 *)
  | Unused_open of string                   (* 33 *)
  | Unused_type_declaration of string       (* 34 *)
  | Unused_for_index of string              (* 35 *)
  | Unused_ancestor of string               (* 36 *)
  | Unused_constructor of string * constructor_usage_warning (* 37 *)
  | Unused_extension of string * bool * constructor_usage_warning (* 38 *)
  | Unused_rec_flag                         (* 39 *)
  | Name_out_of_scope of string * string list * bool (* 40 *)
  | Ambiguous_name of string list * string list *  bool * string (* 41 *)
  | Disambiguated_name of string            (* 42 *)
  | Nonoptional_label of string             (* 43 *)
  | Open_shadow_identifier of string * string (* 44 *)
  | Open_shadow_label_constructor of string * string (* 45 *)
  | Bad_env_variable of string * string     (* 46 *)
  | Attribute_payload of string * string    (* 47 *)
  | Eliminated_optional_arguments of string list (* 48 *)
  | No_cmi_file of string * string option   (* 49 *)
  | Unexpected_docstring of bool            (* 50 *)
  | Wrong_tailcall_expectation of bool      (* 51 *)
  | Fragile_literal_pattern                 (* 52 *)
  | Misplaced_attribute of string           (* 53 *)
  | Duplicated_attribute of string          (* 54 *)
  | Inlining_impossible of string           (* 55 *)
  | Unreachable_case                        (* 56 *)
  | Ambiguous_var_in_pattern_guard of string list (* 57 *)
  | No_cmx_file of string                   (* 58 *)
  | Flambda_assignment_to_non_mutable_value (* 59 *)
  | Unused_module of string                 (* 60 *)
  | Unboxable_type_in_prim_decl of string   (* 61 *)
  | Constraint_on_gadt                      (* 62 *)
  | Erroneous_printed_signature of string   (* 63 *)
  | Unsafe_array_syntax_without_parsing     (* 64 *)
  | Redefining_unit of string               (* 65 *)
  | Unused_open_bang of string              (* 66 *)
  | Unused_functor_parameter of string      (* 67 *)
  | Match_on_mutable_state_prevent_uncurry  (* 68 *)
  | Unused_field of string * field_usage_warning (* 69 *)
  | Missing_mli                             (* 70 *)
  | Unused_tmc_attribute                    (* 71 *)
  | Tmc_breaks_tailcall                     (* 72 *)
  | Generative_application_expects_unit     (* 73 *)

(* If you remove a warning, leave a hole in the numbering.  NEVER change
   the numbers of existing warnings.
   If you add a new warning, add it at the end with a new number;
   do NOT reuse one of the holes.
*)

type alert = {kind:string; message:string; def:loc; use:loc}

let number = function
  | Comment_start -> 1
  | Comment_not_end -> 2
  | Fragile_match _ -> 4
  | Ignored_partial_application -> 5
  | Labels_omitted _ -> 6
  | Method_override _ -> 7
  | Partial_match _ -> 8
  | Missing_record_field_pattern _ -> 9
  | Non_unit_statement -> 10
  | Redundant_case -> 11
  | Redundant_subpat -> 12
  | Instance_variable_override _ -> 13
  | Illegal_backslash -> 14
  | Implicit_public_methods _ -> 15
  | Unerasable_optional_argument -> 16
  | Undeclared_virtual_method _ -> 17
  | Not_principal _ -> 18
  | Non_principal_labels _ -> 19
  | Ignored_extra_argument -> 20
  | Nonreturning_statement -> 21
  | Preprocessor _ -> 22
  | Useless_record_with -> 23
  | Bad_module_name _ -> 24
  | All_clauses_guarded -> 8 (* used to be 25 *)
  | Unused_var _ -> 26
  | Unused_var_strict _ -> 27
  | Wildcard_arg_to_constant_constr -> 28
  | Eol_in_string -> 29
  | Duplicate_definitions _ -> 30
  | Unused_value_declaration _ -> 32
  | Unused_open _ -> 33
  | Unused_type_declaration _ -> 34
  | Unused_for_index _ -> 35
  | Unused_ancestor _ -> 36
  | Unused_constructor _ -> 37
  | Unused_extension _ -> 38
  | Unused_rec_flag -> 39
  | Name_out_of_scope _ -> 40
  | Ambiguous_name _ -> 41
  | Disambiguated_name _ -> 42
  | Nonoptional_label _ -> 43
  | Open_shadow_identifier _ -> 44
  | Open_shadow_label_constructor _ -> 45
  | Bad_env_variable _ -> 46
  | Attribute_payload _ -> 47
  | Eliminated_optional_arguments _ -> 48
  | No_cmi_file _ -> 49
  | Unexpected_docstring _ -> 50
  | Wrong_tailcall_expectation _ -> 51
  | Fragile_literal_pattern -> 52
  | Misplaced_attribute _ -> 53
  | Duplicated_attribute _ -> 54
  | Inlining_impossible _ -> 55
  | Unreachable_case -> 56
  | Ambiguous_var_in_pattern_guard _ -> 57
  | No_cmx_file _ -> 58
  | Flambda_assignment_to_non_mutable_value -> 59
  | Unused_module _ -> 60
  | Unboxable_type_in_prim_decl _ -> 61
  | Constraint_on_gadt -> 62
  | Erroneous_printed_signature _ -> 63
  | Unsafe_array_syntax_without_parsing -> 64
  | Redefining_unit _ -> 65
  | Unused_open_bang _ -> 66
  | Unused_functor_parameter _ -> 67
  | Match_on_mutable_state_prevent_uncurry -> 68
  | Unused_field _ -> 69
  | Missing_mli -> 70
  | Unused_tmc_attribute -> 71
  | Tmc_breaks_tailcall -> 72
  | Generative_application_expects_unit -> 73
;;
(* DO NOT REMOVE the ;; above: it is used by
   the testsuite/ests/warnings/mnemonics.mll test to determine where
   the  definition of the number function above ends *)

let last_warning_number = 73

(* Must be the max number returned by the [number] function. *)

let letter = function
  | 'a' ->
     let rec loop i = if i = 0 then [] else i :: loop (i - 1) in
     loop last_warning_number
  | 'b' -> []
  | 'c' -> [1; 2]
  | 'd' -> [3]
  | 'e' -> [4]
  | 'f' -> [5]
  | 'g' -> []
  | 'h' -> []
  | 'i' -> []
  | 'j' -> []
  | 'k' -> [32; 33; 34; 35; 36; 37; 38; 39]
  | 'l' -> [6]
  | 'm' -> [7]
  | 'n' -> []
  | 'o' -> []
  | 'p' -> [8]
  | 'q' -> []
  | 'r' -> [9]
  | 's' -> [10]
  | 't' -> []
  | 'u' -> [11; 12]
  | 'v' -> [13]
  | 'w' -> []
  | 'x' -> [14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24; 30]
  | 'y' -> [26]
  | 'z' -> [27]
  | _ -> assert false

type state =
  {
    active: bool array;
    error: bool array;
    alerts: (Misc.Stdlib.String.Set.t * bool); (* false:set complement *)
    alert_errors: (Misc.Stdlib.String.Set.t * bool); (* false:set complement *)
  }

let current =
  ref
    {
      active = Array.make (last_warning_number + 1) true;
      error = Array.make (last_warning_number + 1) false;
      alerts = (Misc.Stdlib.String.Set.empty, false);
      alert_errors = (Misc.Stdlib.String.Set.empty, true); (* all soft *)
    }

let disabled = ref false

let without_warnings f =
  Misc.protect_refs [Misc.R(disabled, true)] f

let backup () = !current

let restore x = current := x

let is_active x =
  not !disabled && (!current).active.(number x)

let is_error x =
  not !disabled && (!current).error.(number x)

let alert_is_active {kind; _} =
  not !disabled &&
  let (set, pos) = (!current).alerts in
  Misc.Stdlib.String.Set.mem kind set = pos

let alert_is_error {kind; _} =
  not !disabled &&
  let (set, pos) = (!current).alert_errors in
  Misc.Stdlib.String.Set.mem kind set = pos

let with_state state f =
  let prev = backup () in
  restore state;
  try
    let r = f () in
    restore prev;
    r
  with exn ->
    restore prev;
    raise exn

let mk_lazy f =
  let state = backup () in
  lazy (with_state state f)

let set_alert ~error ~enable s =
  let upd =
    match s with
    | "all" ->
        (Misc.Stdlib.String.Set.empty, not enable)
    | s ->
        let (set, pos) =
          if error then (!current).alert_errors else (!current).alerts
        in
        let f =
          if enable = pos
          then Misc.Stdlib.String.Set.add
          else Misc.Stdlib.String.Set.remove
        in
        (f s set, pos)
  in
  if error then
    current := {(!current) with alert_errors=upd}
  else
    current := {(!current) with alerts=upd}

let parse_alert_option s =
  let n = String.length s in
  let id_char = function
    | 'a'..'z' | 'A'..'Z' | '_' | '\'' | '0'..'9' -> true
    | _ -> false
  in
  let rec parse_id i =
    if i < n && id_char s.[i] then parse_id (i + 1) else i
  in
  let rec scan i =
    if i = n then ()
    else if i + 1 = n then raise (Arg.Bad "Ill-formed list of alert settings")
    else match s.[i], s.[i+1] with
      | '+', '+' -> id (set_alert ~error:true ~enable:true) (i + 2)
      | '+', _ -> id (set_alert ~error:false ~enable:true) (i + 1)
      | '-', '-' -> id (set_alert ~error:true ~enable:false) (i + 2)
      | '-', _ -> id (set_alert ~error:false ~enable:false) (i + 1)
      | '@', _ ->
          id (fun s ->
              set_alert ~error:true ~enable:true s;
              set_alert ~error:false ~enable:true s)
            (i + 1)
      | _ -> raise (Arg.Bad "Ill-formed list of alert settings")
  and id f i =
    let j = parse_id i in
    if j = i then raise (Arg.Bad "Ill-formed list of alert settings");
    let id = String.sub s i (j - i) in
    f id;
    scan j
  in
  scan 0

type modifier =
  | Set (** +a *)
  | Clear (** -a *)
  | Set_all (** @a *)

type token =
  | Letter of char * modifier option
  | Num of int * int * modifier

let ghost_loc_in_file name =
  let pos = { Lexing.dummy_pos with pos_fname = name } in
  { loc_start = pos; loc_end = pos; loc_ghost = true }

let parse_warnings s =
  let error () = raise (Arg.Bad "Ill-formed list of warnings") in
  let rec get_num n i =
    if i >= String.length s then i, n
    else match s.[i] with
    | '0'..'9' -> get_num (10 * n + Char.code s.[i] - Char.code '0') (i + 1)
    | _ -> i, n
  in
  let get_range i =
    let i, n1 = get_num 0 i in
    if i + 2 < String.length s && s.[i] = '.' && s.[i + 1] = '.' then
      let i, n2 = get_num 0 (i + 2) in
      if n2 < n1 then error ();
      i, n1, n2
    else
      i, n1, n1
  in
  let rec loop tokens i =
    if i >= String.length s then List.rev tokens else
    match s.[i] with
    | 'A' .. 'Z' | 'a' .. 'z' ->
        loop (Letter(s.[i],None)::tokens) (i+1)
    | '+' -> loop_letter_num tokens Set (i+1)
    | '-' -> loop_letter_num tokens Clear (i+1)
    | '@' -> loop_letter_num tokens Set_all (i+1)
    | _ -> error ()
  and loop_letter_num tokens modifier i =
    if i >= String.length s then error () else
    match s.[i] with
    | '0' .. '9' ->
        let i, n1, n2 = get_range i in
        loop (Num(n1,n2,modifier)::tokens) i
    | 'A' .. 'Z' | 'a' .. 'z' ->
       loop (Letter(s.[i],Some modifier)::tokens) (i+1)
    | _ -> error ()
  in
  loop [] 0

(* If you change these, don't forget to change them in man/ocamlc.m *)
let defaults_w = "+a-4-7-9-27-29-30-32..42-44-45-48-50-60-66..70"
let defaults_warn_error = "-a"
let default_disabled_alerts = [ "unstable"; "unsynchronized_access" ]

let message = function
  | Comment_start ->
      "this `(*' is the start of a comment.\n\
       Hint: Did you forget spaces when writing the infix operator `( * )'?"
  | Comment_not_end -> "this is not the end of a comment."
  | Fragile_match "" ->
      "this pattern-matching is fragile."
  | Fragile_match s ->
      "this pattern-matching is fragile.\n\
       It will remain exhaustive when constructors are added to type " ^ s ^ "."
  | Ignored_partial_application ->
      "this function application is partial,\n\
       maybe some arguments are missing."
  | Labels_omitted [] -> assert false
  | Labels_omitted [l] ->
     "label " ^ l ^ " was omitted in the application of this function."
  | Labels_omitted ls ->
     "labels " ^ String.concat ", " ls ^
       " were omitted in the application of this function."
  | Method_override [lab] ->
      "the method " ^ lab ^ " is overridden."
  | Method_override (cname :: slist) ->
      String.concat " "
        ("the following methods are overridden by the class"
         :: cname  :: ":\n " :: slist)
  | Method_override [] -> assert false
  | Partial_match "" -> "this pattern-matching is not exhaustive."
  | Partial_match s ->
      "this pattern-matching is not exhaustive.\n\
       Here is an example of a case that is not matched:\n" ^ s
  | Missing_record_field_pattern s ->
      "the following labels are not bound in this record pattern:\n" ^ s ^
      "\nEither bind these labels explicitly or add '; _' to the pattern."
  | Non_unit_statement ->
      "this expression should have type unit."
  | Redundant_case -> "this match case is unused."
  | Redundant_subpat -> "this sub-pattern is unused."
  | Instance_variable_override [lab] ->
      "the instance variable " ^ lab ^ " is overridden."
  | Instance_variable_override (cname :: slist) ->
      String.concat " "
        ("the following instance variables are overridden by the class"
         :: cname  :: ":\n " :: slist)
  | Instance_variable_override [] -> assert false
  | Illegal_backslash ->
    "illegal backslash escape in string.\n\
    Hint: Single backslashes \\ are reserved for escape sequences\n\
    (\\n, \\r, ...). Did you check the list of OCaml escape sequences?\n\
    To get a backslash character, escape it with a second backslash: \\\\."
  | Implicit_public_methods l ->
      "the following private methods were made public implicitly:\n "
      ^ String.concat " " l ^ "."
  | Unerasable_optional_argument -> "this optional argument cannot be erased."
  | Undeclared_virtual_method m -> "the virtual method "^m^" is not declared."
  | Not_principal s -> s^" is not principal."
  | Non_principal_labels s -> s^" without principality."
  | Ignored_extra_argument -> "this argument will not be used by the function."
  | Nonreturning_statement ->
      "this statement never returns (or has an unsound type.)"
  | Preprocessor s -> s
  | Useless_record_with ->
      "all the fields are explicitly listed in this record:\n\
       the 'with' clause is useless."
  | Bad_module_name (modname) ->
      "bad source file name: \"" ^ modname ^ "\" is not a valid module name."
  | All_clauses_guarded ->
      "this pattern-matching is not exhaustive.\n\
       All clauses in this pattern-matching are guarded."
  | Unused_var v | Unused_var_strict v -> "unused variable " ^ v ^ "."
  | Wildcard_arg_to_constant_constr ->
     "wildcard pattern given as argument to a constant constructor"
  | Eol_in_string ->
     "unescaped end-of-line in a string constant\n\
      (non-portable behavior before OCaml 5.2)"
  | Duplicate_definitions (kind, cname, tc1, tc2) ->
      Printf.sprintf "the %s %s is defined in both types %s and %s."
        kind cname tc1 tc2
  | Unused_value_declaration v -> "unused value " ^ v ^ "."
  | Unused_open s -> "unused open " ^ s ^ "."
  | Unused_open_bang s -> "unused open! " ^ s ^ "."
  | Unused_type_declaration s -> "unused type " ^ s ^ "."
  | Unused_for_index s -> "unused for-loop index " ^ s ^ "."
  | Unused_ancestor s -> "unused ancestor variable " ^ s ^ "."
  | Unused_constructor (s, Unused) -> "unused constructor " ^ s ^ "."
  | Unused_constructor (s, Not_constructed) ->
      "constructor " ^ s ^
      " is never used to build values.\n\
        (However, this constructor appears in patterns.)"
  | Unused_constructor (s, Only_exported_private) ->
      "constructor " ^ s ^
      " is never used to build values.\n\
        Its type is exported as a private type."
  | Unused_extension (s, is_exception, complaint) ->
     let kind =
       if is_exception then "exception" else "extension constructor" in
     let name = kind ^ " " ^ s in
     begin match complaint with
       | Unused -> "unused " ^ name
       | Not_constructed ->
          name ^
          " is never used to build values.\n\
           (However, this constructor appears in patterns.)"
       | Only_exported_private ->
          name ^
          " is never used to build values.\n\
            It is exported or rebound as a private extension."
     end
  | Unused_rec_flag ->
      "unused rec flag."
  | Name_out_of_scope (ty, [nm], false) ->
      nm ^ " was selected from type " ^ ty ^
      ".\nIt is not visible in the current scope, and will not \n\
       be selected if the type becomes unknown."
  | Name_out_of_scope (_, _, false) -> assert false
  | Name_out_of_scope (ty, slist, true) ->
      "this record of type "^ ty ^" contains fields that are \n\
       not visible in the current scope: "
      ^ String.concat " " slist ^ ".\n\
       They will not be selected if the type becomes unknown."
  | Ambiguous_name ([s], tl, false, expansion) ->
      s ^ " belongs to several types: " ^ String.concat " " tl ^
      "\nThe first one was selected. Please disambiguate if this is wrong."
      ^ expansion
  | Ambiguous_name (_, _, false, _ ) -> assert false
  | Ambiguous_name (_slist, tl, true, expansion) ->
      "these field labels belong to several types: " ^
      String.concat " " tl ^
      "\nThe first one was selected. Please disambiguate if this is wrong."
      ^ expansion
  | Disambiguated_name s ->
      "this use of " ^ s ^ " relies on type-directed disambiguation,\n\
       it will not compile with OCaml 4.00 or earlier."
  | Nonoptional_label s ->
      "the label " ^ s ^ " is not optional."
  | Open_shadow_identifier (kind, s) ->
      Printf.sprintf
        "this open statement shadows the %s identifier %s (which is later used)"
        kind s
  | Open_shadow_label_constructor (kind, s) ->
      Printf.sprintf
        "this open statement shadows the %s %s (which is later used)"
        kind s
  | Bad_env_variable (var, s) ->
      Printf.sprintf "illegal environment variable %s : %s" var s
  | Attribute_payload (a, s) ->
      Printf.sprintf "illegal payload for attribute '%s'.\n%s" a s
  | Eliminated_optional_arguments sl ->
      Printf.sprintf "implicit elimination of optional argument%s %s"
        (if List.length sl = 1 then "" else "s")
        (String.concat ", " sl)
  | No_cmi_file(name, None) ->
      "no cmi file was found in path for module " ^ name
  | No_cmi_file(name, Some msg) ->
      Printf.sprintf
        "no valid cmi file was found in path for module %s. %s"
        name msg
  | Unexpected_docstring unattached ->
      if unattached then "unattached documentation comment (ignored)"
      else "ambiguous documentation comment"
  | Wrong_tailcall_expectation b ->
      Printf.sprintf "expected %s"
        (if b then "tailcall" else "non-tailcall")
  | Fragile_literal_pattern ->
      let[@manual.ref "ss:warn52"] ref_manual = [ 13; 5; 3 ] in
      Format.asprintf
        "Code should not depend on the actual values of\n\
         this constructor's arguments. They are only for information\n\
         and may change in future versions. %a"
        Misc.print_see_manual ref_manual
  | Unreachable_case ->
      "this match case is unreachable.\n\
       Consider replacing it with a refutation case '<pat> -> .'"
  | Misplaced_attribute attr_name ->
      Printf.sprintf "the %S attribute cannot appear in this context" attr_name
  | Duplicated_attribute attr_name ->
      Printf.sprintf "the %S attribute is used more than once on this \
          expression"
        attr_name
  | Inlining_impossible reason ->
      Printf.sprintf "Cannot inline: %s" reason
  | Ambiguous_var_in_pattern_guard vars ->
      let[@manual.ref "ss:warn57"] ref_manual = [ 13; 5; 4 ] in
      let vars = List.sort String.compare vars in
      let vars_explanation =
        let in_different_places =
          "in different places in different or-pattern alternatives"
        in
        match vars with
        | [] -> assert false
        | [x] -> "variable " ^ x ^ " appears " ^ in_different_places
        | _::_ ->
            let vars = String.concat ", " vars in
            "variables " ^ vars ^ " appear " ^ in_different_places
      in
      Format.asprintf
        "Ambiguous or-pattern variables under guard;\n\
         %s.\n\
         Only the first match will be used to evaluate the guard expression.\n\
         %a"
        vars_explanation Misc.print_see_manual ref_manual
  | No_cmx_file name ->
      Printf.sprintf
        "no cmx file was found in path for module %s, \
         and its interface was not compiled with -opaque" name
  | Flambda_assignment_to_non_mutable_value ->
      "A potential assignment to a non-mutable value was detected \n\
        in this source file.  Such assignments may generate incorrect code \n\
        when using Flambda."
  | Unused_module s -> "unused module " ^ s ^ "."
  | Unboxable_type_in_prim_decl t ->
      Printf.sprintf
        "This primitive declaration uses type %s, whose representation\n\
         may be either boxed or unboxed. Without an annotation to indicate\n\
         which representation is intended, the boxed representation has been\n\
         selected by default. This default choice may change in future\n\
         versions of the compiler, breaking the primitive implementation.\n\
         You should explicitly annotate the declaration of %s\n\
         with [@@boxed] or [@@unboxed], so that its external interface\n\
         remains stable in the future." t t
  | Constraint_on_gadt ->
      "Type constraints do not apply to GADT cases of variant types."
  | Erroneous_printed_signature s ->
      "The printed interface differs from the inferred interface.\n\
       The inferred interface contained items which could not be printed\n\
       properly due to name collisions between identifiers."
     ^ s
     ^ "\nBeware that this warning is purely informational and will not catch\n\
        all instances of erroneous printed interface."
  | Unsafe_array_syntax_without_parsing ->
     "option -unsafe used with a preprocessor returning a syntax tree"
  | Redefining_unit name ->
      Printf.sprintf
        "This type declaration is defining a new '()' constructor\n\
         which shadows the existing one.\n\
         Hint: Did you mean 'type %s = unit'?" name
  | Unused_functor_parameter s -> "unused functor parameter " ^ s ^ "."
  | Match_on_mutable_state_prevent_uncurry ->
    "This pattern depends on mutable state.\n\
     It prevents the remaining arguments from being uncurried, which will \
     cause additional closure allocations."
  | Unused_field (s, Unused) -> "unused record field " ^ s ^ "."
  | Unused_field (s, Not_read) ->
      "record field " ^ s ^
      " is never read.\n\
        (However, this field is used to build or mutate values.)"
  | Unused_field (s, Not_mutated) ->
      "mutable record field " ^ s ^
      " is never mutated."
  | Missing_mli ->
    "Cannot find interface file."
  | Unused_tmc_attribute ->
      "This function is marked @tail_mod_cons\n\
       but is never applied in TMC position."
  | Tmc_breaks_tailcall ->
      "This call\n\
       is in tail-modulo-cons position in a TMC function,\n\
       but the function called is not itself specialized for TMC,\n\
       so the call will not be transformed into a tail call.\n\
       Please either mark the called function with the [@tail_mod_cons]\n\
       attribute, or mark this call with the [@tailcall false] attribute\n\
       to make its non-tailness explicit."
  | Generative_application_expects_unit ->
      "A generative functor\n\
       should be applied to '()'; using '(struct end)' is deprecated."
;;

let nerrors = ref 0

type reporting_information =
  { id : string
  ; message : string
  ; is_error : bool
  ; sub_locs : (loc * string) list;
  }

let report_alert (alert : alert) =
  match alert_is_active alert with
  | false -> `Inactive
  | true ->
      let is_error = alert_is_error alert in
      if is_error then incr nerrors;
      let message = Misc.normalise_eol alert.message in
       (* Reduce \r\n to \n:
           - Prevents any \r characters being printed on Unix when processing
             Windows sources
           - Prevents \r\r\n being generated on Windows, which affects the
             testsuite
       *)
      let sub_locs =
        if not alert.def.loc_ghost && not alert.use.loc_ghost then
          [
            alert.def, "Definition";
            alert.use, "Expected signature";
          ]
        else
          []
      in
      `Active
        {
          id = alert.kind;
          message;
          is_error;
          sub_locs;
        }

exception Errors

let reset_fatal () =
  nerrors := 0

let check_fatal () =
  if !nerrors > 0 then begin
    nerrors := 0;
    raise Errors;
  end
