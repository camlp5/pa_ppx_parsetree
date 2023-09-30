open Pa_ppx_base
open Ppxutil

let progname = "extract-major-minor-ocaml-version"
let overs = ref ""

let _ =
  Arg.
  (parse
    []
    (fun s -> overs := s)
    Fmt.(str "%s <arg>" progname))

if !overs = "" then
  Fmt.(pf stderr "%s: must supply a non-trivial first argument" progname)
else () ;;
  
let overs = !overs
let overs = [%subst {|\s+$|} / {||} ] overs ;;
let l = [%split {|\.|}/re_perl] overs
let (maj, min) = match l with
    a::b::_ -> (int_of_string a,int_of_string b)
  | a::_ -> (int_of_string a,0)
  | _ -> Fmt.(failwithf "%s: bad argument" progname)
;;
Fmt.(pf stdout "%d%02d" maj min) ;;
