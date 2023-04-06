(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Pa_ppx_parsetree_via_parsetree.Reorg_parsetree.attribute]
[@@deriving quotation_test {
        target_is_pattern_ast = false
      ; location_type = [%typ: location]
      ; loc_varname = __loc__
      ; superfluous_constructors = [
          Pexp_xtr
        ; Ppat_xtr
        ; Ptyp_xtr
        ]
      ; test_types = [
          expression
        ; pattern
        ; constant
        ; core_type
        ; attribute
        ; extension
        ; case
        ]
      ; default_expression = {
          location_stack = []
        ; attributes = []
        }
      ; expand_types = {
          expression_desc = Auto
        ; pattern_desc = Auto
        ; core_type_desc = Auto
        ; located = Auto
        ; open_declaration = Auto
        ; open_infos = Auto
        ; letop = Auto
        ; extension = Auto
        ; payload = Auto
        ; package_type = Auto
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
        ; (Pexp_constant, { constant = Auto })
        ; (Ppat_constant, { constant = Auto })
        ]
      ; per_constructor_expansion = [
          (Pconst_integer, Explicit [
             (Parsetree.Pconst_integer (s, None))
           ; (Parsetree.Pconst_integer (s, Some 'l'))
           ; (Parsetree.Pconst_integer (s, Some 'L'))
           ; (Parsetree.Pconst_integer (s, Some 'n'))
           ])
        ; (Pconst_float, Explicit [
             Parsetree.Pconst_float (sxf1, None)
          ])
        ; (Ptyp_variant, AddDel (
                             [Parsetree.Ptyp_variant(lx1, Asttypes.Closed, Some [])],
                             [Parsetree.Ptyp_variant(lx1, x, Some [])]
          ))
        ]
      ; type_module_map = {
          constant = Parsetree
        ; expression = Parsetree
        ; expression_desc = Parsetree
        ; pattern = Parsetree
        ; pattern_desc = Parsetree
        ; core_type = Parsetree
        ; core_type_desc = Parsetree
        ; located = Location
        ; longident_t = Longident
        ; direction_flag = Asttypes
        ; class_structure = Parsetree
        ; open_infos = Parsetree
        ; letop = Parsetree
        ; payload = Parsetree
        ; attribute = Parsetree
        ; extension = Parsetree
        ; case = Parsetree
        }
      }
  ]
