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
      ; expand_types = {
          expression_desc = Auto
        ; located = Auto
        ; constant = Auto
        }
      ; expand_types_per_constructor = [
          (Pexp_ident,
           { longident_t = Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
          ]})
        ; (Pexp_field,
           { longident_t = Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
          ]})
        ; (Pexp_setfield,
           { longident_t = Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
          ]})
        ; (Pexp_setfield,
           { longident_t = Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
          ]})
        ; (Pexp_for, { direction_flag = Auto })
        ; (Pexp_new,
           { longident_t = Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
          ]})
        ; (Pexp_object, { class_structure = Auto })
        ]
      ; per_constructor_exprs = [
          (Pconst_integer, [
             (Parsetree.Pconst_integer (s, None))
           ; (Parsetree.Pconst_integer (s, Some 'l'))
           ; (Parsetree.Pconst_integer (s, Some 'L'))
           ; (Parsetree.Pconst_integer (s, Some 'n'))
           ])
        ; (Pconst_float, [
             Parsetree.Pconst_float (sxf1, None)
          ])

        ]
      ; type_module_map = {
          constant = Parsetree
        ; expression = Parsetree
        ; expression_desc = Parsetree
        ; located = Location
        ; longident_t = Longident
        ; direction_flag = Asttypes
        ; class_structure = Parsetree
        }
      }
  ]
