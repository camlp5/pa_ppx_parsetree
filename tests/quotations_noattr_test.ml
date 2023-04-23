(**pp -package pa_ppx.deriving_plugins.std,pa_ppx_parsetree_quotations,pa_ppx_quotation2extension -syntax camlp5o *)

open OUnit2

let x = 1

module Fixtures = struct

let loc = Location.none

let l = "x" 
let l' = "y"

let l_loc = [%lident_loc {| $lid:l$ |}]

let m = "M" 
let n = "N" 
let li1 = [%longident_t {| $uid:m$ |}] 
let li2 = [%longident_t {| B.C |}] 

let lili1 = [%longlident {| M.x |}]

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

module TY = struct

open Fixtures

(* If we get a error "-8" (unmatched case) here, it means that the "noattr" quotation is broken *)
let test ctxt =
  assert_equal [] (match [%core_type {| int |}] with
                       [%core_type {| $_$ $algattrs:l$ |}] -> l)
  ; assert_equal Location.none
      (match [%core_type {| int |}] with
         [%core_type.noattr.loc {| $_$ |}] -> loc)[@@ocaml.warnerror "+8"]

end


let suite = "Test pa_ppx_parsetree_quotations (noattr)" >::: [
      "core_type"   >:: TY.test
    ]


let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
