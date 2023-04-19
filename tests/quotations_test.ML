(**pp -package pa_ppx.deriving_plugins.std,pa_ppx_parsetree_quotations,pa_ppx_quotation2extension -syntax camlp5o *)

open OUnit2

let x = 1

module Fixtures = struct

let __loc__ = Location.none

let l = "x" 
let l' = "y"

let l_loc = [%lident_loc {| $lid:l$ |}]

let m = "M" 
let n = "N" 
let li1 = [%longident_t {| $uid:m$ |}] 
let li2 = [%longident_t {| B.C |}] 

let e0 = [%expression {| $lid:l$ |}]
let e1 = [%expression {| a * b |}]
let e2 = [%expression {| a / b |}]
let e3 = [%expression {| a - b |}]

let me1 = [%module_expr {| M |}]
let me2 = [%module_expr {| N |}]

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

let fld1 = [%label_declaration {| x : int |}]
let fld2 = [%label_declaration {| mutable y : int |}]
let fields = [fld1; fld2]

let case1 = [%case {| C x -> f x |}]
let case2 = [%case {| D y when p y -> g y |}]
let cases = [case1; case2]

let vb1 = [%value_binding {| x = 1 |}]
let vb2 = [%value_binding {| y = 2 |}]

let excon1 = [%extension_constructor {| C of int |}]

end

module Helpers = struct

#if OCAML_VERSION >= (4,11,0)
let parse_longident s = s |> Lexing.from_string |> Parse.longident
#else
let parse_longident = Longident.parse
#endif

  include Pa_ppx_parsetree_helpers.Std_derivers
end

module SV = struct
  open Fixtures
  let test ctxt =
    assert_equal [%lident_loc {| x |}] [%lident_loc {| x |}]
    ; assert_equal l (match [%lident_loc {| $lid:l$ |}] with
                        [%lident_loc {| $lid:l'$ |}] -> l')
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
  ; assert_equal (li1,l) (match [%longident_t {| $longid:li1$. $lid:l$ |}] with
                            [%longident_t {| $longid:li2$. $lid:l2$ |}] -> (li2,l2))
  ; assert_equal ({|M.x|} |> Helpers.parse_longident) [%longident_t {| $uid:m$ . $lid:l$ |}]
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

; assert_equal (li1, m) (match [%extended_module_path {| $longid:li1$. $uid:m$ |}] with
                           [%extended_module_path {| $longid:li2$. $uid:m2$ |}] -> (li2,m2))

; assert_equal (li1, li2) (match [%extended_module_path {| $longid:li1$ ($longid:li2$) |}] with
                             [%extended_module_path {| $longid:l$ ($longid:m$) |}] -> (l,m))

end

module VB = struct

open Fixtures

let test ctxt = 
  assert_equal [%value_binding {| x = 1 |}] [%value_binding {| x = 1 |}]

end

module AL = struct

open Fixtures

let test ctxt = 
  assert_equal [%arg_label {| |}] Asttypes.Nolabel 
  ; assert_equal [%arg_label {| foo: |}] (Asttypes.Labelled "foo")
  ; assert_equal [%arg_label {| ?foo: |}] (Asttypes.Optional "foo")
  ; assert_equal "x" (match [%arg_label {| $lid:l$: |}] with
                        [%arg_label {| $lid:l'$: |}] -> l')
  ; assert_equal "x" (match [%arg_label {| ? $lid:l$: |}] with
                        [%arg_label {| ?  $lid:l'$: |}] -> l')
end

module BOP = struct

open Fixtures

let test ctxt =
  assert_equal [%binding_op {| let* x = y |}] [%binding_op {| let* x = y |}]
  ; assert_equal [%binding_op {| and* x = y |}] [%binding_op {| and* x = y |}]
  ; assert_equal ("let*", p1, e1) (
        let s = "let*" in
        match [%binding_op {| $lid:s$ $p1$ = $e1$ |}]
        with [%binding_op {| $lid:s'$ $p1'$ = $e1'$ |}] -> (s', p1', e1'))
end

module EX = struct

open Fixtures

let test_equality ctxt =
  assert_equal ~cmp:Helpers.equal_expression ({| x |} |>  Lexing.from_string |> Parse.expression) [%expression {| $lid:l$ |}]
  ; assert_bool "builtin equality should fail here (b/c locations)"
      (not (({| x |} |>  Lexing.from_string |> Parse.expression) = [%expression {| $lid:l$ |}]))

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
; assert_equal ~cmp:[%eq: Helpers.expression list]
    [e1;e2] (match ({| (a*b, a/b) |} |> Lexing.from_string |> Parse.expression) with
                          [%expression {| $tuplelist:l2$ |}] ->  l2)


let e1 = [%expression {| { x = 1 } |}]
let e2 =  {| { x = 1 } |} |> Lexing.from_string |> Parse.expression

let test2 ctxt =
  assert_bool "builtin equality fails on expressions" (not (e1 = e2))
  ; assert_equal ~cmp:Helpers.equal_expression e1 e2

let test3 ctxt =
  assert_equal ("x", [%expression {| 1 |}])
    (match e1 with
       [%expression {| { $lid:x$ = $e$ } |}] -> (x,e))
  ; assert_equal (li1, "x", [%expression {| 1 |}])
    (match [%expression {| { M.x = 1 } |}] with
       [%expression {| { $longid:m'$ . $lid:x$ = $e$ } |}] -> (m', x,e))
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

let test_let ctxt =
  let open Asttypes in
  assert_equal (Recursive, [vb1; vb2],e1) (
      let rf = Recursive in
      let vbl = [vb1; vb2] in
      match [%expression {| let $recflag:rf$ $list:vbl$ in $e1$ |}] with
        [%expression {| let $recflag:rf'$ $list:vbl'$ in $e1'$ |}] -> (rf',vbl',e1'))

let test_function ctxt =
  let open Asttypes in
  assert_equal cases (
      match [%expression {| function $list:cases$ |}] with
        [%expression {| function $list:cases'$ |}] -> cases')

let test_try ctxt =
  let open Asttypes in
  assert_equal (e1,cases) (
      match [%expression {| try $e1$ with $list:cases$ |}] with
        [%expression {| try $e1'$ with $list:cases'$ |}] -> (e1',cases'))

let test_construct ctxt =
  let open Asttypes in
  assert_equal () (
      match [%expression {| C |}] with
        [%expression {| C |}] -> ())
  ; begin
      let eopt = Some [%expression {| ($e1$, $e2$)|}] in
      assert_equal (li1, eopt) (
          match [%expression {| $longid:li1$ $expropt:eopt$ |}] with
            [%expression {| $longid:l'$ $expropt:eopt'$ |}] -> (l',eopt'))
    end

let test_field ctxt =
  let open Asttypes in
  assert_equal () (
      match [%expression {| x.f |}] with
        [%expression {| x.f |}] -> ())
  ; assert_equal () (
        match [%expression {| x.M.f |}] with
          [%expression {| x.M.f |}] -> ())
  ; assert_equal () (
        match [%expression {| x. $longid:li2$ . $lid:l$ |}] with
          [%expression {| x.B.C.x |}] -> ())
  ; assert_equal (li2, l) (
        match [%expression {| x. $longid:li2$ . $lid:l$ |}] with
          [%expression {| x. $longid:li2'$ . $lid:l'$ |}] -> (li2', l'))

let test_setfield ctxt =
  let open Asttypes in
  assert_equal () (
      match [%expression {| x.f <- 1 |}] with
        [%expression {| x.f <- 1 |}] -> ())
  ; assert_equal () (
        match [%expression {| x.M.f <- 1 |}] with
          [%expression {| x.M.f <- 1 |}] -> ())
  ; assert_equal () (
        match [%expression {| x. $longid:li2$ . $lid:l$ <- 1 |}] with
          [%expression {| x.B.C.x <- 1 |}] -> ())
  ; assert_equal (e1, li2, l, e2) (
        match [%expression {| $e1$ . $longid:li2$ . $lid:l$ <- $e2$ |}] with
          [%expression {| $e1'$ . $longid:li2'$ . $lid:l'$ <- $e2'$ |}] -> (e1', li2', l', e2'))

let test_fun ctxt =
  let open Asttypes in
  let lab = Nolabel in
  let eopt = None in
  assert_equal lab (
      match [%expression {| fun $label:lab$ ( x ) -> 1 |}] with
        [%expression {| fun $label:lab'$ ( x ) -> 1 |}] -> lab')
  ; assert_equal lab (
        match [%expression {| fun $label:lab$ ( x = 2 ) -> 1 |}] with
          [%expression {| fun $label:lab'$ ( x = 2 ) -> 1 |}] -> lab')
  ; assert_equal (lab, None) (
        match [%expression {| fun $label:lab$ ( x $expropt:eopt$ ) -> 1 |}] with
          [%expression {| fun $label:lab'$ ( x $expropt:eopt'$ ) -> 1 |}] -> (lab',eopt'))
  ; assert_equal (lab, p1, None) (
        match [%expression {| fun $label:lab$ ( $p1$ $expropt:eopt$ ) -> 1 |}] with
          [%expression {| fun $label:lab'$ ( $p1'$ $expropt:eopt'$ ) -> 1 |}] -> (lab',p1', eopt'))
  ; assert_equal (lab, p1, None, e2) (
        match [%expression {| fun $label:lab$ ( $p1$ $expropt:eopt$ ) -> $e2$ |}] with
          [%expression {| fun $label:lab'$ ( $p1'$ $expropt:eopt'$ ) -> $e2'$ |}] -> (lab',p1', eopt', e2'))

let test_variant ctxt =
  let open Asttypes in
  assert_equal () (
      match [%expression {| `C |}] with
        [%expression {| `C |}] -> ())
  ; assert_equal m (
      match [%expression {| ` $id:m$ |}] with
        [%expression {| ` $id:m'$ |}] -> m')
  ; begin
      let eopt = Some [%expression {| ($e1$, $e2$)|}] in
      assert_equal (m, eopt) (
          match [%expression {| ` $id:m$ $expropt:eopt$ |}] with
            [%expression {| ` $id:m'$ $expropt:eopt'$ |}] -> (m',eopt'))
    end

let test_array ctxt =
  let open Asttypes in
  assert_equal () (
      match [%expression {| [|1;2|] |}] with
        [%expression {| [|1;2|] |}] -> ())
  ; assert_equal [e1;e2] (
        let l = [e1;e2] in
        match [%expression {| [| $list:l$ |] |}] with
          [%expression {| [| $list:l'$ |] |}] -> l')

let test_ifthenelse ctxt =
  let open Asttypes in
  assert_equal () (
      match [%expression {| if e1 then e2 else e3 |}] with
        [%expression {| if e1 then e2 else e3 |}] -> ())
 ; assert_equal (e1,e2,e3) (
       match [%expression {| if $e1$ then $e2$ else $e3$ |}] with
         [%expression {| if $e1'$ then $e2'$ else $e3'$ |}] -> (e1',e2',e3'))
 ; assert_equal (e1,e2,Some e3) (
       let e3opt = Some e3 in
       match [%expression {| if $e1$ then $e2$ $expropt:e3opt$ |}] with
         [%expression {| if $e1'$ then $e2'$ $expropt:e3opt'$ |}] -> (e1',e2',e3opt'))

let test_for ctxt =
  let open Asttypes in
  assert_equal () (
      match [%expression {| for i = e1 to e2 do e3 done |}] with
        [%expression {| for i = e1 to e2 do e3 done |}] -> ())
  ; assert_equal (p1, e1, e2, e3) (
      match [%expression {| for $p1$ = $e1$ to $e2$ do $e3$ done |}] with
        [%expression {| for $p1'$ = $e1'$ to $e2'$ do $e3'$ done |}] -> (p1', e1', e2', e3'))
  ; assert_equal (p1, e1, Upto, e2, e3) (
        let f = Upto in
        match [%expression {| for $p1$ = $e1$ $dirflag:f$ $e2$ do $e3$ done |}] with
          [%expression {| for $p1'$ = $e1'$ $dirflag:f'$ $e2'$ do $e3'$ done |}] -> (p1', e1', f', e2', e3'))

let test_coerce ctxt =
  let open Asttypes in
  assert_equal () (
      match [%expression {| ( e1 : t1 :> t2 ) |}] with
        [%expression {| ( e1 : t1 :> t2 ) |}] -> ())
  ; assert_equal (e1, t1, t2) (
        match [%expression {| ( $e1$ : $t1$ :> $t2$ ) |}] with
          [%expression {| ( $e1'$ : $t1'$ :> $t2'$ ) |}] -> (e1', t1', t2'))
  ; assert_equal (e1, Some t1, t2) (
        let t1opt = Some t1 in
        match [%expression {| ( $e1$ $ctypopt:t1opt$ :> $t2$ ) |}] with
          [%expression {| ( $e1'$ $ctypopt:t1opt'$ :> $t2'$ ) |}] -> (e1', t1opt', t2'))

let test_send ctxt =
  let open Asttypes in
  assert_equal () (
      match [%expression {| e1#m |}] with
        [%expression {| e1#m |}] -> ())
  ; assert_equal (e1, m) (
        match [%expression {| $e1$ # $lid:m$ |}] with
          [%expression {| $e1'$ # $lid:m'$ |}] -> (e1', m'))

let test_new ctxt =
  let open Asttypes in
  assert_equal () (
      match [%expression {| new c |}] with
        [%expression {| new c |}] -> ())
  ; assert_equal () (
        match [%expression {| new M.N.c |}] with
          [%expression {| new M.N.c |}] -> ())
  ; assert_equal (li1, m) (
        match [%expression {| new $longid:li1$ . $lid:m$ |}] with
          [%expression {| new $longid:li1'$ . $lid:m'$ |}] -> (li1', m'))

let test_setinstvar ctxt =
  let open Asttypes in
  assert_equal () (
      match [%expression {| x <- 1 |}] with
        [%expression {| x <- 1 |}] -> ())
  ; assert_equal (l, e2) (
        match [%expression {| $lid:l$ <- $e2$ |}] with
          [%expression {| $lid:l'$ <- $e2'$ |}] -> (l', e2'))

let test_override ctxt =
  let open Asttypes in
  assert_equal () (
      match [%expression {| {< x1 = E1; xn = En >} |}] with
        [%expression {| {< x1 = E1; xn = En >} |}] -> ())
    ; assert_equal (l, e1) (
          match [%expression {| {< $lid:l$ = $e1$; xn = En >} |}] with
            [%expression {| {< $lid:l'$ = $e1'$; xn = En >} |}] -> (l', e1'))
    ; assert_equal ([(l_loc, e1)]) (
          let ll = [(l_loc, e1)] in
          match [%expression {| {< $list:ll$ >} |}] with
            [%expression {| {< $list:ll'$ >} |}] -> ll')

let test_letmodule ctxt =
  assert_equal () (
      match [%expression {| let module M = ME in e |}] with
        [%expression {| let module M = ME in e |}] -> ())
  ; assert_equal (m, me1, e1) (
      match [%expression {| let module $uid:m$ = $me1$ in $e1$ |}] with
        [%expression {| let module $uid:m'$ = $me1'$ in $e1'$ |}] -> (m', me1', e1'))
  ; assert_equal (None, me1, e1) (
        let uopt = None in
      match [%expression {| let module $opt:uopt$ = $me1$ in $e1$ |}] with
        [%expression {| let module $opt:uopt'$ = $me1'$ in $e1'$ |}] -> (uopt', me1', e1'))

let test_letexception ctxt =
  assert_equal () (
      match [%expression {| let exception C of int in e |}] with
        [%expression {| let exception C of int in e |}] -> ())
  ; assert_equal (excon1, e1) (
        match [%expression {| let $excon:excon1$ in $e1$ |}] with
          [%expression {| let $excon:excon1'$ in $e1'$ |}] -> (excon1', e1'))

let test_open ctxt =
  let open Asttypes in
  assert_equal () (
      match [%expression {| let open M in e |}] with
        [%expression {| let open M in e |}] -> ())
  ; assert_equal () (
        match [%expression {| let open! M in e |}] with
          [%expression {| let open! M in e |}] -> ())
  ; assert_equal () (
        match [%expression {| M.(e) |}] with
          [%expression {| M.(e) |}] -> ())
  ; assert_equal (me1, e1) (
        match [%expression {| let open $me1$ in $e1$ |}] with
          [%expression {| let open $me1'$ in $e1'$ |}] -> (me1', e1'))
  ; assert_equal (Fresh, me1, e1) (
        let ovf = Fresh in
        match [%expression {| let open $overrideflag:ovf$ $me1$ in $e1$ |}] with
          [%expression {| let open $overrideflag:ovf'$ $me1'$ in $e1'$ |}] -> (ovf', me1', e1'))

let test_letop ctxt =
  let open Asttypes in
  assert_equal () (
      match [%expression {| let* x = y in e |}] with
        [%expression {| let* x = y in e |}] -> ())
  ; assert_equal () (
        match [%expression {| let* x = y and* u = v in e |}] with
          [%expression {| let* x = y and* u = v in e |}] -> ()) ;
  let bop = [%binding_op {| let* x = y |}] in
  let ands = [] in
  assert_equal (bop, [], e1) (
      match [%expression {| $letop:bop$ $list:ands$ in $e1$ |}] with
        [%expression {| $letop:bop'$ $list:ands'$ in $e1'$ |}] -> (bop', ands', e1'))

let test_extension ctxt =
  let open Asttypes in
  assert_equal () (match [%expression {| [%foo] |}] with
                     [%expression {| [%foo] |}] -> ())
  ; assert_equal () (match [%expression {| [%foo: int] |}] with
                     [%expression {| [%foo: int] |}] -> ())
  ; assert_equal l (match [%expression {| [% $attrid:l$] |}] with
                       [%expression {| [% $attrid:l'$] |}] -> l')
  ; assert_equal (l, t1) (match [%expression {| [% $attrid:l$ : $t1$] |}] with
                       [%expression {| [% $attrid:l'$ : $t1'$] |}] -> (l', t1'))
  ; assert_equal (l, []) (
        let sil = [] in
        match [%expression {| [% $attrid:l$ $list:sil$] |}] with
          [%expression {| [% $attrid:l'$ $list:sil'$] |}] -> (l', sil'))

let test = "expression" >::: [
      "equality"   >:: test_equality
    ; "0"   >:: test0
    ; "1"   >:: test1
    ; "2"   >:: test2
    ; "3"   >:: test3
    ; "let"   >:: test_let
    ; "function"   >:: test_function
    ; "try"   >:: test_try
    ; "construct"   >:: test_construct
    ; "fun"   >:: test_fun
    ; "variant"   >:: test_variant
    ; "field"   >:: test_field
    ; "setfield"   >:: test_setfield
    ; "array"   >:: test_array
    ; "ifthenelse"   >:: test_ifthenelse
    ; "for"   >:: test_for
    ; "coerce"   >:: test_coerce
    ; "send"   >:: test_send
    ; "new"   >:: test_new
    ; "setinstvar"   >:: test_setinstvar
    ; "override"   >:: test_override
    ; "letmodule"   >:: test_letmodule
    ; "letexception"   >:: test_letexception
    ; "open"   >:: test_open
    ; "letop"   >:: test_letop
    ; "extension"   >:: test_extension
    ]

end

module ME = struct

open Fixtures

let test_ident ctxt = 
  assert_equal () (match [%module_expr {| M |}] with
                           [%module_expr {| M |}] -> ())

let test = "module_expr" >::: [
      "ident"   >:: test_ident
    ]

end

module XC = struct

open Fixtures

let test_basic ctxt = 
  assert_equal () (match [%extension_constructor {| C of int |}] with
                     [%extension_constructor {| C of int |}] -> ())

let test = "module_expr" >::: [
      "basic"   >:: test_basic
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

; assert_equal (l,p2) (match  [%pattern {| ( $lid:l$, $p2$ ) |}] with
                    [%pattern {| ( $lid:l'$, $p2'$ ) |}] -> (l', p2'))

; assert_equal m (match [%pattern {| $uid:m$ |}] with
                     [%pattern {| $uid:m'$ |}] -> m')


; let pats = [ [%pattern {| $lid:l$ |}] ; [%pattern {| $lid:l'$ |}] ] in
  assert_equal (m, pats) (match [%pattern {| $uid:m$ ( $tuplelist:pats$ ) |}] with
                            [%pattern {| $uid:m'$ ( $tuplelist:pats'$ ) |}] -> (m', pats'))

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
  ; assert_equal (li1, l) (match  [%core_type {| $longid:li1$ . $lid:l$ |}] with
                      [%core_type {| $longid:li1'$ . $lid:s'$ |}] -> (li1', s'))

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
    assert_equal (Mutable, l, t1)
      (match  [%label_declaration {| $mutable:Mutable$ $lid:l$ : $typ:t1$ |}] with
         [%label_declaration {| $mutable:f'$ $lid:l'$ : $typ:t'$ |}] -> (f', l', t'))

end

module TD = struct

  open Fixtures
  open Asttypes

  let test ctxt =
    assert_equal [%type_decl  {| t = int |}] [%type_decl  {| t = int |}]
    ; assert_equal ("t", "int") (match [%type_decl  {| t = int |}] with
                            [%type_decl  {| $lid:t$ = $lid:m$ |}] -> (t,m))
    ; assert_equal [ [%constructor_declaration {| C of int |}] ]
        (match [%type_decl  {| t = C of int |}] with
           [%type_decl  {| t = $constructorlist:cl$ |}] -> cl)
    ; assert_equal [ [%label_declaration {| mutable f : int |}] ]
        (match [%type_decl  {| t = { mutable f : int } |}] with
           [%type_decl  {| t = { $list:fl$ } |}] -> fl)
    ; assert_equal ("C", [ [%label_declaration {| mutable f : int |}] ])
        (match [%type_decl  {| t = C of { mutable f : int } |}] with
           [%type_decl  {| t = $uid:cid$ of { $list:fl$ } |}] -> (cid, fl))

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
        (match  [%structure_item {| type t = $priv:Private$ $t1$ |}] with
           [%structure_item {| type t = $priv:p'$ $t'$ |}] -> (p', t'))

    ; assert_equal (Recursive,m) (
          let nr = Recursive in
          match  [%structure_item {| type $nonrecflag:nr$ t = $uid:m$ of int |}] with
            [%structure_item {| type $nonrecflag:nr'$ t = $uid:cid$ of int |}] -> (nr', cid))

    ; assert_equal (m, [t1;t2])
        (match [%structure_item {| exception $uid:m$ of $list:[t1;t2]$ |}] with
           [%structure_item {| exception $uid:cid$ of $list:tl'$ |}] -> (cid, tl'))

    ; assert_equal (m, fields) (match [%structure_item {| exception $uid:m$ of { $list:fields$ } |}] with
                                         [%structure_item {| exception $uid:cid$ of { $list:fl$ } |}] -> (cid, fl))

    ; assert_equal (Mutable, l, t1)
        (match [%structure_item {| type t = { $mutable:Mutable$ $lid:l$ : $typ:t1$ } |}] with
           [%structure_item {| type t = { $mutable:f'$ $lid:name$ : $typ:t$ } |}] -> (f',  name, t))

  
    ; assert_equal [ [%type_decl {| t = int |} ] ]
        (match [%structure_item {| type t = int |}] with
           [%structure_item {| type $list:l$ |}] -> l)

    ; assert_equal e1
        (match [%structure_item {| $expr:e1$ |}] with
           [%structure_item {| $expr:e1'$ |}] -> e1')

end

let suite = "Test pa_ppx_parsetree_quotations" >::: [
      "str_vala"   >:: SV.test
    ; "longident"   >:: LI.test
    ; "extended_module_path"   >:: XM.test
    ; "value_binding"   >:: VB.test
    ; "arg_label"   >:: AL.test
    ; "binding_op"   >:: BOP.test
    ; "pattern"   >:: PA.test
    ; EX.test
    ; ME.test
    ; XC.test
    ; "core_type"   >:: TY.test
    ; "constructor_declaration"   >:: CD.test
    ; "field"   >:: FLD.test
    ; "type_decl"   >:: TD.test
    ; "structure_item"   >:: STRI.test
    ; "case"   >:: Case.test
    ]


let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
