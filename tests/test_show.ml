(**pp -package pa_ppx.deriving_plugins.std,pa_ppx_parsetree_via_parsetree,pa_ppx_quotation2extension -syntax camlp5o *)

let tds = {|
type t1 = A of int | B of string list | C of bool t2 | D of int t2
and 'a t2 = {it : 'a ; other : string }
           |}
let si = tds |> Lexing.from_string |> Parse.implementation
