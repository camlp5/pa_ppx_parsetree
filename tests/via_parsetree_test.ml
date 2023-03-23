(**pp -package pa_ppx_parsetree_via_parsetree,pa_ppx_quotation2extension -syntax camlp5o *)

open OUnit2

let x = 1

module Fixtures = struct

let __loc__ = Location.none

let l = "x" 
let m = "M" 
let n = "N" 
let li1 = [%longident_t {| $uid:m$ |}] 
let li2 = [%longident_t {| B.C |}] 

let e0 = [%expression {| $lid:l$ |}]
let e1 = [%expression {| a * b |}]
let e2 = [%expression {| a / b |}]

let p1 = [%pattern {| C(a, b) |}]
let p2 = [%pattern {| [a :: b] |}]

let v1 = "a"
let v2 = "b"
let t1 = [%core_type {| 'a |}]
let t2 = [%core_type {| 'b |}]

let cd1 = [%constructor_declaration {| C of t1 * t2 |}]
let cd2 = [%constructor_declaration {| D |}]

let attr1 = [%attribute {| [@foo] |}]
let attr2 = [%attribute {| [@bar] |}]
let attrs = [attr1;attr2]

let fld1 = [%field {| x : int |}]
let fld2 = [%field {| mutable y : int |}]
let fields = [fld1; fld2]

let case1 = [%case {| C x -> f x |}]
let case2 = [%case {| D y when p y -> g y |}]
let cases = [case1; case2]

end

module Helpers = struct
  include Pa_ppx_parsetree_official_parsetree.Derive_parsetree
end

module LI = struct
open Fixtures

let test ctxt = 
  assert_equal () (match [%longident_t {| $lid:l$ |}] with
                     [%longident_t {| $lid:_$ |}] -> ())
  ; assert_equal l (match [%longident_t {| $lid:l$ |}] with
                      [%longident_t {| $lid:l2$ |}] -> l2)

  ; assert_equal (m, l) (match [%longident_t {| $uid:m$ . $lid:l$ |}] with
                           [%longident_t {| $uid:m2$ . $lid:l2$ |}] -> (m2,l2))
  ; assert_equal (li1,l) (match [%longident_t {| $li1$. $lid:l$ |}] with
                            [%longident_t {| $li2$. $lid:l2$ |}] -> (li2,l2))

end

module XM = struct

open Fixtures

let test ctxt =
  assert_equal () (match [%extended_module_path {| $uid:m$ |}] with
                     [%extended_module_path {| $uid:_$ |}] -> ())
; assert_equal m (match [%extended_module_path {| $uid:m$ |}] with
                     [%extended_module_path {| $uid:e1$ |}] -> e1)

; assert_equal (m, l) (match [%extended_module_path {| $uid:m$ . $uid:l$ |}] with
                         [%extended_module_path {| $uid:m2$ . $uid:l2$ |}] -> (m2,l2))
; assert_equal (m, n) (match [%extended_module_path {| $uid:m$. $uid:n$ |}] with
                          [%extended_module_path {| $uid:m2$. $uid:l2$ |}] -> (m2,l2))

; assert_equal (li1, m) (match [%extended_module_path {| $li1$. $uid:m$ |}] with
                           [%extended_module_path {| $li2$. $uid:m2$ |}] -> (li2,m2))

; assert_equal (li1, li2) (match [%extended_module_path {| $li1$ ($li2$) |}] with
                             [%extended_module_path {| $l$ ($m$) |}] -> (l,m))

end

module EX = struct

open Fixtures

let test0 ctxt = 
  assert_equal Location.none (
      let __loc__ = 1 in
      match e1 with
        [%expression.loc {| $_$ * $_$ |}] -> __loc__)

let test1 ctxt = 
  let open Asttypes in
  assert_equal (e0,[(Nolabel,e1);(Nolabel,e2)]) (
      let el = [(Nolabel,e1);(Nolabel,e2)] in
      match [%expression {| $e0$ $list:el$ |}] with
        [%expression {| $e0'$ $list:el'$ |}] -> (e0',el'))
; assert_equal (e1, e2) (match [%expression {| $e1$ + $e2$ |}] with
                           [%expression {| $e1'$ + $e2'$ |}] -> (e1',e2'))
; assert_equal [e1;e2] (match [%expression {| $tuplelist:[e1;e2]$ |}] with
                          [%expression {| $tuplelist:l2$ |}] ->  l2)

; assert_equal (e1,e2) (match [%expression {| ($e1$, $e2$) |}] with
                          [%expression {| ($e1'$, $e2'$) |}] ->  (e1', e2'))

; assert_equal (m,l) (match [%expression {| $uid:m$. $lid:l$ |}] with
                         [%expression {| $uid:m'$. $lid:l'$ |}] -> (m',l'))
; assert_equal l (match [%expression {| let $lid:l$ = 1 in () |}] with
                    [%expression {| let $lid:l'$ = 1 in () |}] -> l')
; assert_equal  l (match [%expression {| let* $lid:l$ = 1 in () |}] with
                     [%expression {| let* $lid:x$ = 1 in () |}] -> x)

; assert_equal (e1,cases) (match [%expression {| match $e1$ with $list:cases$ |}] with
                              [%expression {| match $e'$ with $list:cases'$ |}] -> (e',cases'))

let e1 = [%expression {| { x = 1 } |}]
let e2 =  {| { x = 1 } |} |> Lexing.from_string |> Parse.expression

let test2 ctxt =
  assert_bool "builtin equality fails on expressions" (not (e1 = e2))
  ; assert_equal ~cmp:Helpers.equal_expression e1 e2

let test3 ctxt =
  assert_equal ("x", [%expression {| 1 |}])
    (match e1 with
       [%expression {| { $lid:x$ = $e$ } |}] -> (x,e))
  ; assert_equal None
      (match e1 with
         [%expression {| { $withe:e$ $list:_$ } |}] -> e)
  ; assert_equal ~cmp:Helpers.equal_expression
      ({| { e with y = 2 } |} |> Lexing.from_string |> Parse.expression)
      (let e = Some [%expression {| e |}] in
       [%expression {| { $withe:e$ y = 2 } |}])
  ; assert_equal [(Location.mknoloc [%longident_t "x"], [%expression {| 1 |}])]
      (match [%expression {| { x = 1 } |}] with
         [%expression {| { $list:l$  } |}] -> l)

let test4 ctxt =
  let open Asttypes in
  assert_equal Recursive (
      let rf = Recursive in
      match [%expression {| let $recflag:rf$ f x = f x in 1 |}] with
                            [%expression {| let $recflag:rf'$ f x = f x in 1 |}] -> rf)

let test = "expression" >::: [
      "0"   >:: test0
    ; "1"   >:: test1
    ; "2"   >:: test2
    ; "3"   >:: test3
    ; "4"   >:: test4
    ]

end

module Case = struct

open Fixtures

let test ctxt = 

  assert_equal (p1, e1, e2) (match [%case {| $p1$ when $e1$ -> $e2$ |}] with
                           [%case {| $p1'$ when $e1'$ -> $e2'$ |}] -> (p1', e1',e2'))
; assert_equal (p1, Some e1, e2) (match [%case {| $p1$ when $e1$ -> $e2$ |}] with
                           [%case {| $p1'$ $wheno:e1opt$ -> $e2'$ |}] -> (p1', e1opt,e2'))
; assert_equal (p1, None, e2) (match [%case {| $p1$ -> $e2$ |}] with
                           [%case {| $p1'$ $wheno:e1opt$ -> $e2'$ |}] -> (p1', e1opt,e2'))
end

module PA = struct

open Fixtures

let test ctxt =

  assert_equal [p1;p2] (match  [%pattern {| $tuplelist:[p1;p2]$ |}] with
                          [%pattern {| $tuplelist:l'$ |}] ->  l')

; assert_equal (p1,p2) (match [%pattern {| ($p1$, $p2$) |}] with
                          [%pattern {| ($e1$, $e2$) |}] ->  (e1, e2))

; assert_equal l (match  [%pattern {| $lid:l$ |}] with
                    [%pattern {| $lid:m'$ |}] -> m')

; assert_equal m (match [%pattern {| $uid:m$ |}] with
                     [%pattern {| $uid:m'$ |}] -> m')

end

module TY = struct

open Fixtures

let test ctxt =
  assert_equal [t1;t2] (match [%core_type {| $list:[t1;t2]$ t |}] with
                          [%core_type {| $list:l'$ t |}] ->  l')

  ; assert_equal (t1,t2) (match  [%core_type {| ($t1$, $t2$) t |}] with
                            [%core_type {| ($e1$, $e2$) t |}] ->  (e1, e2))

  ; assert_equal (v1,v2) (match  [%core_type {| (' $lid:v1$, ' $lid:v2$) t |}] with
                            [%core_type {| (' $lid:e1$, ' $lid:e2$) t |}] ->  (e1, e2))

  ; assert_equal (t1,m,l) (match [%core_type {| $t1$ $uid:m$ . $lid:l$ |}] with
                             [%core_type {| $c'$ $uid:m'$ . $lid:t'$ |}] -> (c',m',t'))

  ; assert_equal [t1;t2] (match  [%core_type {| $tuplelist:[t1;t2]$ |}] with
                            [%core_type {| $tuplelist:l'$ |}] -> l')

  ; assert_equal [%core_type {| $t1$ * $t2$ |}]
      [%core_type {| $tuplelist:[t1;t2]$ |}]

  ; assert_equal l (match  [%core_type {| $lid:l$ |}] with
                      [%core_type {| $lid:s'$ |}] -> s')

  ; assert_equal (t1,t2) (match [%core_type {| $t1$ * $t2$ |}] with
                            [%core_type {| $t1'$ * $t2'$ |}] -> (t1', t2'))

end

module CD = struct

  open Fixtures

  let test ctxt =
    assert_equal (m, [t1;t2]) (match  [%constructor_declaration {| $uid:m$ of $list:[t1;t2]$ |}] with
                                 [%constructor_declaration {| $uid:cid'$ of $list:tl'$ |}] -> (cid', tl'))

end

module FLD = struct

  open Fixtures
  open Asttypes

  let test ctxt =
    assert_equal (Mutable, l, t1, attrs)
      (match  [%field {| $mutable:Mutable$ $lid:l$ : $typ:t1$ $algattrs:attrs$ |}] with
         [%field {| $mutable:f'$ $lid:l'$ : $typ:t'$ $algattrs:attrs'$ |}] -> (f', l', t', attrs'))

end

module STRI = struct

  open Fixtures
  open Asttypes

  let test ctxt =

    assert_equal [cd1;cd2] (match [%structure_item {| type t = $constructorlist:[cd1;cd2]$ |}] with
                              [%structure_item {| type t = $constructorlist:l'$ |}] -> l')

    ; assert_equal (Private, [cd1;cd2])
        (match [%structure_item {| type t = $priv:Private$ $constructorlist:[cd1;cd2]$ |}] with
           [%structure_item {| type t = $priv:p'$ $constructorlist:l'$ |}] -> (p',l'))

    ; assert_equal (Private,  t1)
        (match  [%structure_item {| type t = $priv:Private$ $typ:t1$ |}] with
           [%structure_item {| type t = $priv:p'$ $typ:t'$ |}] -> (p', t'))

    ; assert_equal (m,attrs) (match  [%structure_item {| type t = $uid:m$ of int $algattrs:attrs$ |}] with
                                [%structure_item {| type t = $uid:cid$ of int $algattrs:l$ |}] -> (cid,l))

    ; assert_equal (m, [t1;t2], attrs)
        (match [%structure_item {| exception $uid:m$ of $list:[t1;t2]$ $algattrs:attrs$ |}] with
           [%structure_item {| exception $uid:cid$ of $list:tl'$ $algattrs:l$ |}] -> (cid, tl', l))

    ; assert_equal (m, fields, attrs) (match [%structure_item {| exception $uid:m$ of { $list:fields$ } $algattrs:attrs$ |}] with
                                         [%structure_item {| exception $uid:cid$ of { $list:fl$ } $algattrs:l$ |}] -> (cid, fl, l))

    ; assert_equal (Mutable, l, t1)
        (match [%structure_item {| type t = { $mutable:Mutable$ $lid:l$ : $typ:t1$ } |}] with
           [%structure_item {| type t = { $mutable:f'$ $lid:name$ : $typ:t$ } |}] -> (f',  name, t))

end


let suite = "Test pa_ppx_parsetree_via_parsetree" >::: [
      "longident"   >:: LI.test
    ; "extended_module_path"   >:: XM.test
    ; EX.test
    ; "core_type"   >:: TY.test
    ; "constructor_declaration"   >:: CD.test
    ; "field"   >:: FLD.test
    ; "structure_item"   >:: STRI.test
    ; "case"   >:: Case.test
    ]


let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
