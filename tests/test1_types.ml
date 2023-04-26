(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Pa_ppx_parsetree_quotations.Reorg_parsetree.attribute]
[@@deriving quotation_test {
        target_is_pattern_ast = false
      ; location_type = [%typ: location]
      ; loc_varname = loc
      ; minimal_record_module_labels = true
      ; superfluous_constructors = [
          Pexp_xtr
        ; Ppat_xtr
        ; Ptyp_xtr
        ; Pmod_xtr
        ; Pmty_xtr
        ; Pcl_xtr
        ; Pcty_xtr
        ]
      ; test_types = [
          arg_label
        ; attribute
        ; binding_op
        ; case
        ; class_description
        ; class_expr
        ; class_field
        ; class_type
        ; class_type_declaration
        ; class_type_field
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
          location_stack = []
        }
      ; expand_types = {
          attribute = Auto
        ; attributes = Explicit [
                           attrs ;
                           []
                         ]
        ; constructor_arguments = Auto
        ; class_expr_desc = Auto
        ; class_field_desc = Auto
        ; class_field_kind = Auto
        ; class_infos = Auto
        ; class_signature = Auto
        ; class_structure = Auto
        ; class_type_desc = Auto
        ; class_type_field_desc = Auto
        ; core_type_desc = Auto
        ; expression_desc = Auto
        ; extension = Auto
        ; extension_constructor = Auto
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
        ; structure_item_desc = AddDel (
                                    [],
                                    [
                                      Parsetree.Pstr_eval(x1, attrs);
                                      Parsetree.Pstr_eval(x1, [])
                                    ]
                                  )
        ; type_exception = Auto
        ; type_extension = Auto
        ; type_kind = Auto
        ; value_description = Auto
        }
      ; expand_types_per_type = {
          type_declaration = {
            private_flag = Auto
          ; type_declaration = AddDel(
                                   [],
                                   [
                                     {Parsetree.ptype_name = {Location.txt = s; loc = loc}; ptype_params = lxxx; ptype_cstrs = lxxl; ptype_kind = Parsetree.Ptype_abstract; ptype_private = Asttypes.Private; ptype_manifest = None; ptype_attributes = attrs; ptype_loc = loc};
                                     {Parsetree.ptype_name = {Location.txt = s; loc = loc}; ptype_params = lxxx; ptype_cstrs = lxxl; ptype_kind = Parsetree.Ptype_abstract; ptype_private = Asttypes.Private; ptype_manifest = None; ptype_attributes = []; ptype_loc = loc};
                                     {Parsetree.ptype_name = {Location.txt = s; loc = loc}; ptype_params = lxxx; ptype_cstrs = lxxl; ptype_kind = Parsetree.Ptype_abstract; ptype_private = Asttypes.Private; ptype_manifest = ox4; ptype_attributes = attrs; ptype_loc = loc};
                                     {Parsetree.ptype_name = {Location.txt = s; loc = loc}; ptype_params = lxxx; ptype_cstrs = lxxl; ptype_kind = Parsetree.Ptype_abstract; ptype_private = Asttypes.Private; ptype_manifest = ox4; ptype_attributes = []; ptype_loc = loc};
                                     {Parsetree.ptype_name = {Location.txt = s; loc = loc}; ptype_params = lxxx; ptype_cstrs = lxxl; ptype_kind = Parsetree.Ptype_abstract; ptype_private = Asttypes.Public; ptype_manifest = ox4; ptype_attributes = attrs; ptype_loc = loc};
                                     {Parsetree.ptype_name = {Location.txt = s; loc = loc}; ptype_params = lxxx; ptype_cstrs = lxxl; ptype_kind = Parsetree.Ptype_abstract; ptype_private = Asttypes.Public; ptype_manifest = ox4; ptype_attributes = []; ptype_loc = loc};
                                     {Parsetree.ptype_name = {Location.txt = s; loc = loc}; ptype_params = lxxx; ptype_cstrs = lxxl; ptype_kind = Parsetree.Ptype_abstract; ptype_private = x3; ptype_manifest = None; ptype_attributes = attrs; ptype_loc = loc};
                                     {Parsetree.ptype_name = {Location.txt = s; loc = loc}; ptype_params = lxxx; ptype_cstrs = lxxl; ptype_kind = Parsetree.Ptype_abstract; ptype_private = x3; ptype_manifest = None; ptype_attributes = []; ptype_loc = loc};
                                     {Parsetree.ptype_name = {Location.txt = s; loc = loc}; ptype_params = lxxx; ptype_cstrs = lxxl; ptype_kind = Parsetree.Ptype_abstract; ptype_private = x3; ptype_manifest = ox4; ptype_attributes = attrs; ptype_loc = loc};
                                     {Parsetree.ptype_name = {Location.txt = s; loc = loc}; ptype_params = lxxx; ptype_cstrs = lxxl; ptype_kind = Parsetree.Ptype_abstract; ptype_private = x3; ptype_manifest = ox4; ptype_attributes = []; ptype_loc = loc};
                                   ]
                                 )
          }
        }
      ; expand_types_per_constructor = [
          (Pexp_ident,
           { longident_t = Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
          ]})
        ; (Ptyp_package,
           { longident_t = Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
          ]})
        ; (Ptyp_class,
           { longident_t = Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
          ]})
        ; (Ptyp_constr,
           { longident_t = Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
          ]})
        ; (Ppat_type,
           { longident_t = Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
          ]})
        ; (Pmty_ident,
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
        ; (Pcl_constr,
           { longident_t = Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
          ]})
        ; (Pcty_constr,
           { longident_t = Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
          ]})
        ; (Pwith_type,
           { longident_t = Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
          ]})
        ; (Pwith_typesubst,
           { longident_t = Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
          ]})
        ; (Pwith_modtype,
           { longident_t = Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
          ]})
        ; (Pwith_modtypesubst,
           { longident_t = Explicit [
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
                           [Parsetree.Psig_value {Parsetree.pval_name = {Location.txt = s; loc = loc}; pval_type = x2; pval_prim = []; pval_attributes = []; pval_loc = loc}],
                           []
          ))
        ; (Psig_modtypesubst, AddDel (
                                  [],
                                  [
                                    Parsetree.Psig_modtypesubst {Parsetree.pmtd_name = {Location.txt = s; loc = loc}; pmtd_type = None; pmtd_attributes = []; pmtd_loc = loc}
                                  ; Parsetree.Psig_modtypesubst {Parsetree.pmtd_name = {Location.txt = s; loc = loc}; pmtd_type = omt; pmtd_attributes = []; pmtd_loc = loc}
                                  ]
          ))
        ; (Psig_exception, AddDel (
                                  [],
                                  [
                                    Parsetree.Psig_exception {Parsetree.ptyexn_constructor = {Parsetree.pext_name = {Location.txt = s; loc = loc}; pext_kind = Parsetree.Pext_rebind {Location.txt = x; loc = loc}; pext_loc = loc; pext_attributes = attrs}; ptyexn_loc = loc; ptyexn_attributes = attrs};
                                    Parsetree.Psig_exception {Parsetree.ptyexn_constructor = {Parsetree.pext_name = {Location.txt = s; loc = loc}; pext_kind = Parsetree.Pext_rebind {Location.txt = x; loc = loc}; pext_loc = loc; pext_attributes = attrs}; ptyexn_loc = loc; ptyexn_attributes = []};
                                    Parsetree.Psig_exception {Parsetree.ptyexn_constructor = {Parsetree.pext_name = {Location.txt = s; loc = loc}; pext_kind = Parsetree.Pext_rebind {Location.txt = x; loc = loc}; pext_loc = loc; pext_attributes = []}; ptyexn_loc = loc; ptyexn_attributes = attrs};
                                    Parsetree.Psig_exception {Parsetree.ptyexn_constructor = {Parsetree.pext_name = {Location.txt = s; loc = loc}; pext_kind = Parsetree.Pext_rebind {Location.txt = x; loc = loc}; pext_loc = loc; pext_attributes = []}; ptyexn_loc = loc; ptyexn_attributes = []}
                                  ]
          ))
        ; (Pexp_letexception, AddDel (
                                  [],
                                  [
                                    Parsetree.Pexp_letexception( {Parsetree.pext_name = {Location.txt = s; loc = loc}; pext_kind = Parsetree.Pext_rebind {Location.txt = x; loc = loc}; pext_loc = loc; pext_attributes = []}, x2)
                                  ]
          ))
        ]
      ; type_module_map = {
          arg_label = Asttypes
        ; attribute = Parsetree
        ; binding_op = Parsetree
        ; case = Parsetree
        ; class_expr = Parsetree
        ; class_expr_desc = Parsetree
        ; class_field = Parsetree
        ; class_field_desc = Parsetree
        ; class_field_kind = Parsetree
        ; class_infos = Parsetree
        ; class_signature = Parsetree
        ; class_structure = Parsetree
        ; class_type = Parsetree
        ; class_type_desc = Parsetree
        ; class_type_field = Parsetree
        ; class_type_field_desc = Parsetree
        ; constant = Parsetree
        ; constructor_arguments = Parsetree
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
        ; private_flag = Asttypes
        ; row_field = Parsetree
        ; row_field_desc = Parsetree
        ; signature_item_desc = Parsetree
        ; signature_item = Parsetree
        ; structure_item_desc = Parsetree
        ; structure_item = Parsetree
        ; type_declaration = Parsetree
        ; type_exception = Parsetree
        ; type_extension = Parsetree
        ; type_kind = Parsetree
        ; value_binding = Parsetree
        ; value_description = Parsetree
        ; with_constraint = Parsetree
        }
      ; prefix_of_type = [
          ([%typ: (core_type * variance) list Ploc.vala], lxxx)
        ]
      }
  ]
