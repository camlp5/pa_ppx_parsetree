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
# Specifications for the "pa_ppx_parsetree_pattern_parsetree_411" preprocessor:
requires = "camlp5,fmt,re,menhirLib"
version = "%s"
description = "pa_ppx_parsetree_pattern_parsetree_411: OCaml AST for patterns"

archive(byte) = "pa_ppx_parsetree_pattern_parsetree_411.cma"
archive(native) = "pa_ppx_parsetree_pattern_parsetree_411.cmxa"


# For the toploop:
archive(byte,toploop) = "pa_ppx_parsetree_pattern_parsetree_411.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,re,pa_ppx.base,menhirLib"
  archive(syntax,preprocessor,-native) = "pa_ppx_parsetree_pattern_parsetree_411.cma"
  archive(syntax,preprocessor,native) = "pa_ppx_parsetree_pattern_parsetree_411.cmxa"

|} version)
