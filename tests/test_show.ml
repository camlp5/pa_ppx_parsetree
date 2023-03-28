(**pp -package pa_ppx.deriving_plugins.std,pa_ppx_parsetree_via_parsetree,pa_ppx_quotation2extension -syntax camlp5o *)

open Asttypes

let si0txt = {|
type t1 = int * bool * string option
           |}

let si0 = si0txt |> Lexing.from_string |> Parse.implementation |> List.hd


let si1txt = {|
type t1 = A of int | B of string list | C of bool t2 | D of int t2
and 'a t2 = {it : 'a ; other : string }
           |}

let si1 = si1txt |> Lexing.from_string |> Parse.implementation |> List.hd

let tdl = match si0 with
    [%structure_item {| type $list:l$ |}] -> l

let __loc__ = Location.none
let t1 = [%core_type {| int |}]
let t3 = [%core_type {| int * bool |}]
let t3 = [%core_type {| int * bool * string option |}]

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
     [%expression {| $longid:li$ . $lid:tname$ |}]

  | [%core_type.loc {| $lid:tname$ |}] ->
     let pp_name = Fmt.(str "pp_%s" tname) in
     [%expression {| $lid:tname$ |}]

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

let top = function
    [%structure_item {| type $lid:tname$ = $ty$ |}] ->
    let f = core_type "_" ty in
    let pp_name = Fmt.(str "pp_%s" tname) in
    [%structure_item {| let $lid:pp_name$ pps x = Fmt.(pf pps "%a" $f$ x) |}]

;;
Fmt.(pf stdout "%a\n%!" Pprintast.structure [top si0])
