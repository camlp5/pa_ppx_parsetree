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
# Specifications for the "pa_ppx_parsetree_pattern_parsetree" preprocessor:
requires = "pa_ppx_parsetree_pattern_parsetree_VERSION"
version = "%s"
description = "pa_ppx_parsetree_pattern_parsetree: OCaml AST for patterns"

archive(byte) = "pa_ppx_parsetree_pattern_parsetree.cmo"
archive(native) = "pa_ppx_parsetree_pattern_parsetree.cmx"


# For the toploop:
archive(byte,toploop) = "pa_ppx_parsetree_pattern_parsetree.cmo"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "pa_ppx_parsetree_pattern_parsetree_VERSION"
  archive(syntax,preprocessor,-native) = "pa_ppx_parsetree_pattern_parsetree.cmo"
  archive(syntax,preprocessor,native) = "pa_ppx_parsetree_pattern_parsetree.cmx"

|} version)
