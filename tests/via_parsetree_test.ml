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

let e1 = [%expression {| a * b |}]
let e2 = [%expression {| a / b |}]

let p1 = [%pattern {| C(a, b) |}]
let p2 = [%pattern {| [a :: b] |}]

let v1 = "a"
let v2 = "b"
let t1 = [%core_type {| 'a |}]
let t2 = [%core_type {| 'b |}]

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
        <:expression:< $_$ * $_$ >> -> __loc__)

let test1 ctxt = 

  assert_equal (e1, e2) (match [%expression {| $e1$ + $e2$ |}] with
                           [%expression {| $e1'$ + $e2'$ |}] -> (e1',e2'))
; assert_equal [e1;e2] (
      let l = [e1;e2] in
      match [%expression {| $tuplelist:l$ |}] with
        [%expression {| $tuplelist:l2$ |}] ->  l2)

; assert_equal (e1,e2) (match [%expression {| ($e1$, $e2$) |}] with
                          [%expression {| ($e1'$, $e2'$) |}] ->  (e1', e2'))

; assert_equal (m,l) (match [%expression {| $uid:m$. $lid:l$ |}] with
                         [%expression {| $uid:m'$. $lid:l'$ |}] -> (m',l'))
; assert_equal l (match [%expression {| let $lid:l$ = 1 in () |}] with
                    [%expression {| let $lid:l'$ = 1 in () |}] -> l')
; assert_equal  l (match [%expression {| let* $lid:l$ = 1 in () |}] with
                     [%expression {| let* $lid:x$ = 1 in () |}] -> x)

end

module PA = struct

open Fixtures

let test ctxt =

  assert_equal [p1;p2] (
      let l = [p1;p2] in
      match  [%pattern {| $tuplelist:l$ |}] with
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
  assert_equal [t1;t2] (
      let l = [t1;t2] in
      match [%core_type {| $list:l$ t |}] with
        [%core_type {| $list:l'$ t |}] ->  l')

  ; assert_equal (t1,t2) (match  [%core_type {| ($t1$, $t2$) t |}] with
                            [%core_type {| ($e1$, $e2$) t |}] ->  (e1, e2))

  ; assert_equal (v1,v2) (match  [%core_type {| (' $lid:v1$, ' $lid:v2$) t |}] with
                            [%core_type {| (' $lid:e1$, ' $lid:e2$) t |}] ->  (e1, e2))

  ; assert_equal (t1,m,l) (match [%core_type {| $t1$ $uid:m$ . $lid:l$ |}] with
                             [%core_type {| $c'$ $uid:m'$ . $lid:t'$ |}] -> (c',m',t'))

  ; assert_equal [t1;t2] (
        let l = [t1;t2] in
        match  [%core_type {| $tuplelist:l$ |}] with
          [%core_type {| $tuplelist:l'$ |}] -> l')

; assert_equal l (match  [%core_type {| $lid:l$ |}] with
                    [%core_type {| $lid:s'$ |}] -> s')

; assert_equal (t1,t2) (match [%core_type {| $t1$ * $t2$ |}] with
                          [%core_type {| $t1'$ * $t2'$ |}] -> (t1', t2'))

end

module STRI = struct

let f1 : Parsetree.structure_item -> Parsetree.constructor_declaration list =
  function [%structure_item {| type t = $constructorlist:l$ |}] -> l

let f3 : Parsetree.structure_item -> Asttypes.private_flag * Parsetree.constructor_declaration list =
  function [%structure_item {| type t = $priv:p$ $constructorlist:l$ |}] -> (p,l)

let f3 : Parsetree.structure_item -> Parsetree.constructor_declaration list =
  function [%structure_item {| type t = $priv:p$ $constructorlist:l$ |}] -> l

let f4 : Parsetree.structure_item -> Asttypes.private_flag =
  function [%structure_item {| type t = $priv:p$ $typ:t$ |}] -> p

let f5 : Parsetree.structure_item -> string * Parsetree.attribute list =
  function [%structure_item {| type t = $uid:cid$ of int $algattrs:l$ |}] -> (cid,l)

let f6 : Parsetree.structure_item -> string * Parsetree.core_type list * Parsetree.attribute list =
  function [%structure_item {| exception $uid:cid$ of $list:tl$ $algattrs:l$ |}] -> (cid, tl, l)

let f7 : Parsetree.structure_item -> string * Parsetree.label_declaration list * Parsetree.attribute list =
  function [%structure_item {| exception $uid:cid$ of { $list:fl$ } $algattrs:l$ |}] -> (cid, fl, l)


let f8 : Parsetree.structure_item -> Asttypes.mutable_flag * string * Parsetree.core_type =
  function [%structure_item {| type t = { $mutable:f$ $lid:name$ : $typ:t$ } |}] -> (f,  name, t)

end


let suite = "Test pa_ppx_parsetree_via_parsetree" >::: [
      "longident"   >:: LI.test
    ; "extended_module_path"   >:: XM.test
    ; "expression-0"   >:: EX.test0
    ; "expression-1"   >:: EX.test1
    ]


let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
