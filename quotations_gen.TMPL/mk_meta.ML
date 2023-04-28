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
# Specifications for the "pa_ppx_parsetree_quotations" preprocessor:
requires = "pa_ppx_parsetree_quotations_VERSION"
version = "%s"
description = "pa_ppx_parsetree_quotations: OCaml AST for patterns"

archive(byte) = "pa_ppx_parsetree_quotations.cmo"
archive(native) = "pa_ppx_parsetree_quotations.cmx"


# For the toploop:
archive(byte,toploop) = "pa_ppx_parsetree_quotations.cmo"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "pa_ppx_parsetree_quotations_VERSION"
  archive(syntax,preprocessor,-native) = "pa_ppx_parsetree_quotations.cmo"
  archive(syntax,preprocessor,native) = "pa_ppx_parsetree_quotations.cmx"

|} version)
