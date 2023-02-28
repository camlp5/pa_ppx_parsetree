(** -syntax camlp5o *)
open OUnit2

let f = function
  <:expression:< 1 >> ->  1
  | <:expression:< 1 + 2 >> ->  2
  | <:expression:< $e1$ + $e2$ >> ->  3

let test_simple ctxt =
  ()

let suite = "Test pa_ppx_parsetree" >::: [
      "simple"   >:: test_simple
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

