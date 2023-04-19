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
# Specifications for the "pa_ppx_parsetree" preprocessor:
requires = "camlp5,fmt,pa_ppx_parsetree_pattern_parsetree,pa_ppx.base,camlp5.parser_quotations_base,pa_ppx_q_ast.runtime"
version = "%s"
description = "pa_ppx_parsetree_quotations: pa_ppx_parsetree_quotations rewriter"

# For linking
package "link" (
requires = "camlp5,fmt,pa_ppx_parsetree_pattern_parsetree,pa_ppx.base.link,camlp5.parser_quotations_base.link,,pa_ppx_q_ast.runtime"
archive(byte) = "pa_ppx_parsetree_quotations.cma"
archive(native) = "pa_ppx_parsetree_quotations.cmxa"
)

# For the toploop:
archive(byte,toploop) = "pa_ppx_parsetree_quotations.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,pa_ppx_parsetree_pattern_parsetree,pa_ppx.base,camlp5.parser_quotations_base,pa_ppx_q_ast.runtime"
  archive(syntax,preprocessor,-native) = "pa_ppx_parsetree_quotations.cma"
  archive(syntax,preprocessor,native) = "pa_ppx_parsetree_quotations.cmxa"

|} version)
