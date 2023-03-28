(**pp -package pa_ppx.deriving_plugins.std,pa_ppx_parsetree_via_parsetree,pa_ppx_quotation2extension -syntax camlp5o *)

open Asttypes

let si0txt = {|
type tint = int
type tstring = string
type ('a, 'b) choice = Left of 'a | Right of 'b
type ('a, 'b) pair = 'a * 'b
type t1 = int * bool * t2 option
and t2 = { f : string ; g : bool }
and t3 = C | D of int * int | E of { h : int ; j : string }
and t4 = string
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
    [%core_type.loc {| ' $lid:tv$ |}] ->
     let pp_name = Fmt.(str "pp_param_%s" tv) in
     [%expression {| $lid:pp_name$ |}]

  | [%core_type.loc {| int |}] -> [%expression {| int |}]
  | [%core_type.loc {| bool |}] -> [%expression {| bool |}]
  | [%core_type.loc {| string |}] -> [%expression {| Dump.string |}]
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
     let (varpat_list,_, body) = core_type_tuple __loc__ pfx l in
     [%expression {| parens (fun pps ( $tuplelist:varpat_list$ ) -> $body$) |}]

and core_type_tuple __loc__ pfx l =
  let prefixes_types = l |> List.mapi (fun i t -> (Fmt.(str "%s_%d" pfx i), t)) in
  let fmtstring = l |> List.map (fun _ -> "%a") |> String.concat "," in
  let varpat_list = prefixes_types |> List.map (fun (id, _) -> [%pattern {| $lid:id$ |}]) in
  let varexp_list = prefixes_types |> List.map (fun (id, _) -> [%expression {| $lid:id$ |}]) in
  let pplist =
    prefixes_types
    |> List.concat_map (fun (id, t) -> [ core_type id t ; [%expression {| $lid:id$ |}] ]) in
  let body = expr_applist [%expression {| pf pps $string:fmtstring$ |}] pplist in
  (varpat_list, varexp_list, body)

and constructor_decl = function
    [%constructor_declaration.loc {| $uid:cid$ |}] ->
     [%case {| $uid:cid$ -> const string $string:cid$ pps () |}]

  | [%constructor_declaration.loc {| $uid:cid$ of $list:tyl$ |}] ->
     let (varpat_list, varexp_list, body) = core_type_tuple __loc__ "_" tyl in
     [%case {| $uid:cid$ ( $tuplelist:varpat_list$ ) ->
             ((const string $string:cid$) ++ (const string " ") ++ (parens (fun pps ( $tuplelist:varpat_list$ ) -> $body$))) pps ( $tuplelist:varexp_list$ ) |}]

  | [%constructor_declaration.loc {| $uid:cid$ of { $list:fields$ } |}] ->
     let (patbinding_list, (varpat_list, varexp_list), body) = record_type __loc__ fields in
     [%case {| $uid:cid$ { $list:patbinding_list$ } ->
             ((const string $string:cid$) ++ (const string " ") ++ (braces (fun pps ( $tuplelist:varpat_list$ ) -> $body$)))
             pps ( $tuplelist:varexp_list$ ) |}]

and record_type __loc__ fields =
  let ids_types =
    fields
    |>  List.map (function [%field {| $mutable:_$ $lid:l$ : $typ:t$ $algattrs:_$ |}] ->
                    (l, t))  in
  let patbinding_list =
    ids_types |> List.map (fun (id,_) ->
                     let li = [%longident_t {| $lid:id$ |}] in
                     (Location.mkloc li __loc__,
                      [%pattern {| $lid:id$ |}])) in

  let varpat_list = ids_types |> List.map (fun (id, _) -> [%pattern {| $lid:id$ |}]) in
  let varexp_list = ids_types |> List.map (fun (id, _) -> [%expression {| $lid:id$ |}]) in

  let fmtstring = fields |> List.map (fun _ -> "%a") |> String.concat "; " in
  let pplist =
    ids_types
    |> List.concat_map (fun (id, t) ->
           let ppt = core_type "_" t in
           [ [%expression {| (const string $string:id$) ++ (const string " = ") ++ $ppt$ |}]
           ; [%expression {| $lid:id$ |}] ]) in
  
  let body = expr_applist [%expression {| (pf pps $string:fmtstring$) |}] pplist in
  (patbinding_list, (varpat_list, varexp_list), body)

let type_decl = function
    [%type_decl.loc {| $list:tvl$ $lid:tname$ = $ty$ |}] ->
     let pp_name = Fmt.(str "pp_%s" tname) in
     let params = List.map (function ([%core_type {| ' $lid:v$ |}], _) -> Fmt.(str "pp_param_%s" v)) tvl in

     let f = core_type "_" ty in
     let rhs = [%expression {| fun pps x -> Fmt.(pf pps "%a" $f$ x) |}] in
     let rhs = List.fold_right (fun v rhs -> [%expression {| fun $lid:v$ -> $rhs$ |}]) params rhs in
     [%value_binding {| $lid:pp_name$ = $rhs$ |}]

  | [%type_decl.loc {| $list:tvl$ $lid:tname$ = $constructorlist:cl$ |}] ->
     let pp_name = Fmt.(str "pp_%s" tname) in
     let params = List.map (function ([%core_type {| ' $lid:v$ |}], _) -> Fmt.(str "pp_param_%s" v)) tvl in

     let branches = List.map constructor_decl cl in
     let rhs = [%expression {| fun pps ->  Fmt.(function $list:branches$) |}] in
     let rhs = List.fold_right (fun v rhs -> [%expression {| fun $lid:v$ -> $rhs$ |}]) params rhs in

     [%value_binding {| $lid:pp_name$ = $rhs$ |}]

  | [%type_decl.loc {| $list:tvl$ $lid:tname$ = { $list:fields$ } |}] ->
     let pp_name = Fmt.(str "pp_%s" tname) in
     let params = List.map (function ([%core_type {| ' $lid:v$ |}], _) -> Fmt.(str "pp_param_%s" v)) tvl in

     let (patbinding_list, _, body) = record_type __loc__ fields in
     let rhs = [%expression {| Fmt.(braces (fun pps { $list:patbinding_list$ } -> $body$)) |}] in
     let rhs = List.fold_right (fun v rhs -> [%expression {| fun $lid:v$ -> $rhs$ |}]) params rhs in

     [%value_binding {| $lid:pp_name$ = $rhs$ |}]

let top_si si = match si with
    [%structure_item.loc {| type $nonrecflag:rf$ $list:tdl$ |}] ->
    let bindings = List.map type_decl tdl in
    [si ; [%structure_item {| let $recflag:rf$ $list:bindings$ |}]]

let top l = List.concat_map top_si l 

let _ = Fmt.(pf stdout "%a\n%!" Pprintast.structure (top impl0))
