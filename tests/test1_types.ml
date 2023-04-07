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
        ; Pmod_xtr
        ; Pmty_xtr
        ; Psig_xtr
        ; Pstr_xtr
        ]
      ; test_types = [
          expression
        ; pattern
        ; constant
        ; core_type
        ; attribute
        ; extension
        ; case
        ; module_type
        ; module_expr
        ; signature_item
        ; structure_item
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
        ; module_type_desc = Auto
        ; module_expr_desc = Auto
        ; signature_item_desc = Auto
        ; structure_item_desc = Auto
        ; value_description = Auto
        ; type_extension = Auto
        ; type_exception = Auto
        ; module_declaration = Auto
        ; module_type_declaration = Auto
        ; module_substitution = Auto
        ; open_infos = Auto
        ; open_description = Auto
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
        ; (Psig_typext, { longident_t = Explicit [
                                            Longident.Lident s
                                          ; Longident.Ldot (x, s)
          ]})
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
        ; (Psig_value, AddDel (
                           [Parsetree.Psig_value {Parsetree.pval_name = {Location.txt = s; Location.loc = __loc__}; Parsetree.pval_type = x2; Parsetree.pval_prim = []; Parsetree.pval_attributes = []; Parsetree.pval_loc = __loc__}],
                           []
          ))
        ; (Psig_modtypesubst, AddDel (
                                  [],
                                  [
                                    Parsetree.Psig_modtypesubst {Parsetree.pmtd_name = {Location.txt = s; Location.loc = __loc__}; Parsetree.pmtd_type = None; Parsetree.pmtd_attributes = []; Parsetree.pmtd_loc = __loc__}
                                  ; Parsetree.Psig_modtypesubst {Parsetree.pmtd_name = {Location.txt = s; Location.loc = __loc__}; Parsetree.pmtd_type = omt; Parsetree.pmtd_attributes = []; Parsetree.pmtd_loc = __loc__}
                                  ]
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
        ; module_type = Parsetree
        ; module_type_desc = Parsetree
        ; module_expr = Parsetree
        ; module_expr_desc = Parsetree
        ; signature_item = Parsetree
        ; signature_item_desc = Parsetree
        ; structure_item = Parsetree
        ; structure_item_desc = Parsetree
        ; value_description = Parsetree
        ; type_extension = Parsetree
        ; type_exception = Parsetree
        ; extension_constructor = Parsetree
        ; extension_constructor_kind = Parsetree
        ; module_declaration = Parsetree
        ; module_type_declaration = Parsetree
        ; module_substitution = Parsetree
        ; open_infos = Parsetree
        }
      }
  ]
