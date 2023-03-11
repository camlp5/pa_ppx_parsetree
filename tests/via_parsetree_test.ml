(**pp -package pa_ppx_parsetree_via_parsetree -syntax camlp5o *)

open OUnit2

let x = 1

module LI = struct

let f1 : Longident.t -> string = function
<:longident_t< $lid:e1$ >> -> e1

let f2 : Longident.t -> string * string = function
<:longident_t< $uid:m$ . $lid:e1$ >> -> (m,e1)

end

module EX = struct

let f1 : Parsetree.expression -> Parsetree.expression * Parsetree.expression = function
<:expression< $e1$ + $e2$ >> -> (e1,e2)

let f2 : Parsetree.expression -> Parsetree.expression list =
 function <:expression< $tuplelist:l$ >> ->  l

let f3 : Parsetree.expression -> Parsetree.expression * Parsetree.expression =
 function <:expression< ($e1$, $e2$) >> ->  (e1, e2)

end

module PA = struct
let f1 : Parsetree.pattern -> Parsetree.pattern list =
 function <:pattern< $tuplelist:l$ >> ->  l

let f2 : Parsetree.pattern -> Parsetree.pattern * Parsetree.pattern =
 function <:pattern< ($e1$, $e2$) >> ->  (e1, e2)

end

module TY = struct
let f1 : Parsetree.core_type -> Parsetree.core_type list =
 function <:core_type< $list:l$ t >> ->  l

let f1 : Parsetree.core_type -> Parsetree.core_type * Parsetree.core_type =
 function <:core_type< ($e1$, $e2$) t >> ->  (e1, e2)

end

(*
let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

 *)
