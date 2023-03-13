(**pp -package pa_ppx_parsetree_via_parsetree -syntax camlp5o *)

open OUnit2

let x = 1

module LI = struct

let f0 : Longident.t -> unit = function
<:longident_t< $lid:_$ >> -> ()

let f1 : Longident.t -> string = function
<:longident_t< $lid:e1$ >> -> e1

let f2 : Longident.t -> string * string = function
<:longident_t< $uid:m$ . $lid:e1$ >> -> (m,e1)

let f3 : Longident.t -> string * string =
 function <:longident_t< $uid:m$. $lid:e1$ >> -> (m,e1)

let f4 : Longident.t -> Longident.t * string =
 function <:longident_t< $l$. $lid:e1$ >> -> (l,e1)

end

module XM = struct

let f0 : Longident.t -> unit = function
<:extended_module_path< $uid:_$ >> -> ()

let f1 : Longident.t -> string = function
<:extended_module_path< $uid:e1$ >> -> e1

let f2 : Longident.t -> string * string = function
<:extended_module_path< $uid:m$ . $uid:e1$ >> -> (m,e1)

let f3 : Longident.t -> string * string =
 function <:extended_module_path< $uid:m$. $uid:e1$ >> -> (m,e1)

let f4 : Longident.t -> Longident.t * string =
 function <:extended_module_path< $l$. $uid:e1$ >> -> (l,e1)

let f5 : Longident.t -> Longident.t * Longident.t =
 function <:extended_module_path< $l$ ($m$) >> -> (l,m)

end

module EX = struct

let f1 : Parsetree.expression -> Parsetree.expression * Parsetree.expression = function
<:expression< $e1$ + $e2$ >> -> (e1,e2)

let f2 : Parsetree.expression -> Parsetree.expression list =
 function <:expression< $tuplelist:l$ >> ->  l

let f3 : Parsetree.expression -> Parsetree.expression * Parsetree.expression =
 function <:expression< ($e1$, $e2$) >> ->  (e1, e2)

let f4 : Parsetree.expression -> string * string =
 function <:expression< $uid:m$. $lid:e1$ >> -> (m,e1)

let f5 : Parsetree.expression -> string =
  function <:expression< let $lid:x$ = 1 in () >> -> x

let f6 : Parsetree.expression -> string =
  function <:expression< let* $lid:x$ = 1 in () >> -> x

end

module PA = struct
let f1 : Parsetree.pattern -> Parsetree.pattern list =
 function <:pattern< $tuplelist:l$ >> ->  l

let f2 : Parsetree.pattern -> Parsetree.pattern * Parsetree.pattern =
 function <:pattern< ($e1$, $e2$) >> ->  (e1, e2)

let f3 : Parsetree.pattern -> string =
 function <:pattern< $lid:m$ >> -> m

let f4 : Parsetree.pattern -> string =
 function <:pattern< $uid:m$ >> -> m

end

module TY = struct
let f1 : Parsetree.core_type -> Parsetree.core_type list =
 function <:core_type< $list:l$ t >> ->  l

let f2 : Parsetree.core_type -> Parsetree.core_type * Parsetree.core_type =
 function <:core_type< ($e1$, $e2$) t >> ->  (e1, e2)

let f2' : Parsetree.core_type -> string * string =
 function <:core_type< (' $lid:e1$, ' $lid:e2$) t >> ->  (e1, e2)

let f3 : Parsetree.core_type -> Parsetree.core_type * string * string =
function <:core_type< $c$ $uid:m$ . $lid:t$ >> -> (c,m,t)

let f4 : Parsetree.core_type -> Parsetree.core_type list =
function <:core_type< $tuplelist:l$ >> -> l

let f5 : Parsetree.core_type -> string =
function <:core_type< $lid:s$ >> -> s

let f6 : Parsetree.core_type -> Parsetree.core_type * Parsetree.core_type =
function <:core_type< $t1$ * $t2$ >> -> (t1, t2)

end

module STRI = struct

let f1 : Parsetree.structure_item -> Parsetree.constructor_declaration list =
  function <:structure_item< type t = $constructorlist:l$ >> -> l

let f2 : Parsetree.structure_item -> Asttypes.private_flag =
  function <:structure_item< type t = $priv:p$ t >> -> p

end


(*
let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

 *)
