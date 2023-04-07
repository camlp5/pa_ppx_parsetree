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
        ]
      ; test_types = [
          arg_label
        ; attribute
        ; binding_op
        ; case
        ; class_description
        ; class_field
        ; class_type_declaration
        ; constant
        ; constructor_declaration
        ; core_type
        ; expression
        ; extension
        ; extension_constructor
        ; functor_parameter
        ; label_declaration
        ; module_declaration
        ; module_expr
        ; module_type
        ; object_field
        ; pattern
        ; row_field
        ; signature_item
        ; structure_item
        ; type_declaration
        ; value_binding
        ; with_constraint
        ]
      ; default_expression = {
          attributes = []
        ; location_stack = []
        }
      ; expand_types = {
          attribute = Auto
        ; core_type_desc = Auto
        ; expression_desc = Auto
        ; extension = Auto
        ; extension_constructor_kind = Auto
        ; include_declaration = Auto
        ; include_description = Auto
        ; include_infos = Auto
        ; letop = Auto
        ; located = Auto
        ; module_binding = Auto
        ; module_declaration = Auto
        ; module_expr_desc = Auto
        ; module_substitution = Auto
        ; module_type_declaration = Auto
        ; module_type_desc = Auto
        ; object_field_desc = Auto
        ; open_declaration = Auto
        ; open_description = Auto
        ; open_infos = Auto
        ; open_infos = Auto
        ; package_type = Auto
        ; pattern_desc = Auto
        ; payload = Auto
        ; row_field_desc = Auto
        ; signature_item_desc = Auto
        ; structure_item_desc = Auto
        ; type_exception = Auto
        ; type_extension = Auto
        ; value_description = Auto
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
        ; (Pstr_typext, { longident_t = Explicit [
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
          arg_label = Parsetree
        ; attribute = Parsetree
        ; binding_op = Parsetree
        ; case = Parsetree
        ; class_field = Parsetree
        ; class_field_desc = Parsetree
        ; class_structure = Parsetree
        ; constant = Parsetree
        ; constructor_declaration = Parsetree
        ; core_type_desc = Parsetree
        ; core_type = Parsetree
        ; direction_flag = Asttypes
        ; expression_desc = Parsetree
        ; expression = Parsetree
        ; extension_constructor_kind = Parsetree
        ; extension_constructor = Parsetree
        ; extension = Parsetree
        ; functor_parameter = Parsetree
        ; include_infos = Parsetree
        ; label_declaration = Parsetree
        ; letop = Parsetree
        ; located = Location
        ; longident_t = Longident
        ; module_binding = Parsetree
        ; module_declaration = Parsetree
        ; module_expr_desc = Parsetree
        ; module_expr = Parsetree
        ; module_substitution = Parsetree
        ; module_type_declaration = Parsetree
        ; module_type_desc = Parsetree
        ; module_type = Parsetree
        ; object_field = Parsetree
        ; object_field_desc = Parsetree
        ; open_infos = Parsetree
        ; open_infos = Parsetree
        ; pattern_desc = Parsetree
        ; pattern = Parsetree
        ; payload = Parsetree
        ; row_field = Parsetree
        ; row_field_desc = Parsetree
        ; signature_item_desc = Parsetree
        ; signature_item = Parsetree
        ; structure_item_desc = Parsetree
        ; structure_item = Parsetree
        ; type_declaration = Parsetree
        ; type_exception = Parsetree
        ; type_extension = Parsetree
        ; value_binding = Parsetree
        ; value_description = Parsetree
        ; with_constraint = Parsetree
        }
      }
  ]
