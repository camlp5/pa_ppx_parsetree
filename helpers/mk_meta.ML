#include "version.inc"

let top = try Sys.getenv "TOP" with Not_found -> "" ;;
let destdir = ref (top ^ "/local-install") ;;

Arg.(parse [
         "-destdir", (Arg.String (fun s -> destdir := s)),
         ("<path>    Set the destination directory (default: " ^
	    !destdir ^ ")")
       ]
       (fun _ -> failwith "mk_meta: no anonymous args supported")
     "mk_meta -destdir <dir>")
;;

Fmt.(pf stdout {|
# Specifications for the "pa_ppx_parsetree_helpers" preprocessor:
requires = "fmt"
version = "%s"
description = "pa_ppx_parsetree_helpers: OCaml AST for officials"

archive(byte) = "pa_ppx_parsetree_helpers.cma"
archive(native) = "pa_ppx_parsetree_helpers.cmxa"


# For the toploop:
archive(byte,toploop) = "pa_ppx_parsetree_helpers.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "fmt"
  archive(syntax,preprocessor,-native) = "pa_ppx_parsetree_helpers.cma"
  archive(syntax,preprocessor,native) = "pa_ppx_parsetree_helpers.cmxa"

|} version)
