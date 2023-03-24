(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Pa_ppx_parsetree_via_parsetree.Reorg_parsetree.attribute]
[@@deriving quotation_test {
        target_is_pattern_ast = false
      ; location_type = [%typ: location]
      ; loc_varname = __loc__
      ; superfluous_constructors = [
          Pexp_xtr
        ]
      ; test_types = [
        	expression
        ]
      ; default_expression = {
          location_stack = []
        ; attributes = []
        }
      ; expand_types = [
          expression_desc
        ; located
        ]
      ; expand_types_per_constructor = [
          (Pexp_ident, [longident_t])
        ]
      ; type_module_map = {
          expression = Parsetree
        ; expression_desc = Parsetree
        ; located = Location
        ; longident_t = Longident
        }
      }
  ]
