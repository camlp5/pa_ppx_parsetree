(**pp -package pa_ppx.deriving_plugins.std,pa_ppx_parsetree_via_parsetree,pa_ppx_quotation2extension -syntax camlp5o *)

open Asttypes

let si0txt = {|
type t1 = int * bool * t2 option
and t2 = { f : string ; g : bool }
           |}

let impl0 = si0txt |> Lexing.from_string |> Parse.implementation


let si1txt = {|
type t1 = A of int | B of string list | C of bool t2 | D of int t2
and 'a t2 = {it : 'a ; other : string }
           |}

let impl1 = si1txt |> Lexing.from_string |> Parse.implementation

let __loc__ = Location.none

let expr_applist0 e l =
  let rec exrec e = function
      [] -> e
    | h::t -> exrec [%expression {| $e$ $h$ |}] t
  in exrec e l

let expr_unapplist e =
  let rec exrec e acc = match e with
      [%expression {| $e$ $list:l$ |}] -> exrec e (l@acc)
    | e ->  (e,acc)
  in exrec e []

let expr_applist e l =
  let l = l |>  List.map (fun e -> (Nolabel,e)) in
  let (e,l0) = expr_unapplist e in
  [%expression {| $e$ $list:l0@l$ |}]

let rec core_type pfx = function
    [%core_type.loc {| int |}] -> [%expression {| int |}]
  | [%core_type.loc {| bool |}] -> [%expression {| bool |}]
  | [%core_type.loc {| string |}] -> [%expression {| (quote Dump.string) |}]
  | [%core_type.loc {| $longid:li$ . $lid:tname$ |}] ->
     let pp_name = Fmt.(str "pp_%s" tname) in
     [%expression {| $longid:li$ . $lid:pp_name$ |}]

  | [%core_type.loc {| $lid:tname$ |}] ->
     let pp_name = Fmt.(str "pp_%s" tname) in
     [%expression {| $lid:pp_name$ |}]

  | [%core_type.loc {| $t$ option |}] ->
     let f = core_type pfx t in
     [%expression {| option ~none:(const string "None") ((const string "Some ") ++ $f$) |}]
  | [%core_type.loc {| $tuplelist:l$ |}] ->
     let prefixes_types = l |> List.mapi (fun i t -> (Fmt.(str "%s_%d" pfx i), t)) in
     let fmtstring = l |> List.map (fun _ -> "%a") |> String.concat "," in
     let varspat =
       let varpat_list = prefixes_types |> List.map (fun (id, _) -> [%pattern {| $lid:id$ |}]) in
       [%pattern {| ( $tuplelist:varpat_list$ ) |}] in
     let pplist =
       prefixes_types
       |> List.concat_map (fun (id, t) -> [ core_type id t ; [%expression {| $lid:id$ |}] ]) in
     let body = expr_applist [%expression {| pf pps $string:fmtstring$ |}] pplist in
     [%expression {| parens (fun pps $varspat$ -> $body$) |}]

let type_decl = function
    [%type_decl {| $lid:tname$ = $ty$ |}] ->
    let pp_name = Fmt.(str "pp_%s" tname) in
    let f = core_type "_" ty in
    [%value_binding {| $lid:pp_name$ = fun pps x -> Fmt.(pf pps "%a" $f$ x) |}]

  | [%type_decl {| $lid:tname$ = { $list:fields$ } |}] ->
     let pp_name = Fmt.(str "pp_%s" tname) in
     let ids_types =
       fields
       |>  List.map (function [%field {| $mutable:_$ $lid:l$ : $typ:t$ $algattrs:_$ |}] ->
                       (l, t))  in
     let recpat =
       let binding_list =
         ids_types |> List.map (fun (id,_) ->
                          let li = [%longident_t {| $lid:id$ |}] in
                          (Location.mkloc li __loc__,
                           [%pattern {| $lid:id$ |}])) in
       [%pattern {| { $list:binding_list$ } |}] in
     let fmtstring = fields |> List.map (fun _ -> "%a") |> String.concat "; " in
     let pplist =
       ids_types
       |> List.concat_map (fun (id, t) ->
              let ppt = core_type "_" t in
              [ [%expression {| (const string $string:id$) ++ (const string " = ") ++ $ppt$ |}]
              ; [%expression {| $lid:id$ |}] ]) in
     
     let body = expr_applist [%expression {| (pf pps $string:fmtstring$) |}] pplist in
     [%value_binding {| $lid:pp_name$ = Fmt.(braces (fun pps $recpat$ -> $body$)) |}]

let top_si si = match si with
    [%structure_item {| type $nonrecflag:rf$ $list:tdl$ |}] ->
    let bindings = List.map type_decl tdl in
    [si ; [%structure_item {| let $recflag:rf$ $list:bindings$ |}]]

let top l = List.concat_map top_si l 

let _ = Fmt.(pf stdout "%a\n%!" Pprintast.structure (top impl0))
