(**pp -syntax camlp5o -package pa_ppx.import *)
[%%import: MODULE.Reorg_parsetree.attribute]
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
        ; type_extension
        ; value_binding
        ; with_constraint
        ]
      ; default_expression = {
          location_stack = []
        }
      ; expand_types = [
          ([%typ: attribute], Auto)
        ; ([%typ: attributes], Explicit [
                           attrs ;
                           []
                         ])
        ; ([%typ: constructor_arguments], Auto)
        ; ([%typ: class_expr_desc], Auto)
        ; ([%typ: class_field_desc], Auto)
        ; ([%typ: class_field_kind], Auto)
        ; ([%typ: class_infos], Auto)
        ; ([%typ: class_signature], Auto)
        ; ([%typ: class_structure], Auto)
        ; ([%typ: class_type_desc], Auto)
        ; ([%typ: class_type_field_desc], Auto)
        ; ([%typ: core_type_desc], Auto)
        ; ([%typ: expression_desc], Auto)
        ; ([%typ: extension], Auto)
        ; ([%typ: extension_constructor], Auto)
        ; ([%typ: extension_constructor_kind], Auto)
#if OCAML_VERSION >= (5,2,0)
        ; ([%typ: function_body], Auto)
#endif
        ; ([%typ: include_declaration], Auto)
        ; ([%typ: include_description], Auto)
        ; ([%typ: include_infos], Auto)
        ; ([%typ: letop], Auto)
        ; ([%typ: located], Auto)
        ; ([%typ: module_binding], Auto)
        ; ([%typ: module_declaration], Auto)
        ; ([%typ: module_expr_desc], Auto)
        ; ([%typ: module_substitution], Auto)
        ; ([%typ: module_type_declaration], Auto)
        ; ([%typ: module_type_desc], Auto)
        ; ([%typ: object_field_desc], Auto)
        ; ([%typ: open_declaration], Auto)
        ; ([%typ: open_description], Auto)
        ; ([%typ: open_infos], Auto)
        ; ([%typ: open_infos], Auto)
        ; ([%typ: package_type], Auto)
        ; ([%typ: pattern_desc], Auto)
        ; ([%typ: payload], Auto)
        ; ([%typ: row_field_desc], Auto)
        ; ([%typ: signature_item_desc], Auto)
        ; ([%typ: structure_item_desc], AddDel (
                                    [],
                                    [
                                      Parsetree.Pstr_eval(x1, attrs);
                                      Parsetree.Pstr_eval(x1, [])
                                    ]
                                  ))
#if OCAML_VERSION >= (5,2,0)
        ; ([%typ: type_constraint], Auto)
#endif
        ; ([%typ: type_exception], Auto)
        ; ([%typ: type_extension], Auto)
        ; ([%typ: type_kind], Auto)
#if OCAML_VERSION >= (5,1,0)
        ; ([%typ: value_constraint], Auto)
#endif
        ; ([%typ: value_description], Auto)
        ]
      ; expand_types_per_type = {
          type_declaration = [
            ([%typ: private_flag], Auto)
          ; ([%typ: type_declaration], AddDel(
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
                                 ))
          ]
        ; value_binding = [
            ([%typ: value_constraint option],
             AddDel(
                 [],
                 [ox3]
               )
            )
          ]
        }
      ; expand_types_per_constructor = [
          (Pexp_ident,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
             ])
          ])
        ; (Ptyp_package,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
             ])
          ])
        ; (Ptyp_class,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
             ])
          ])
        ; (Ptyp_constr,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
             ])
          ])
        ; (Ppat_type,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
             ])
          ])
        ; (Pmty_ident,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
             ])
          ])
#if OCAML_VERSION >= (5,2,0)
        ; (Pexp_function,
           [ ([%typ: type_constraint], AddDel (
                                   [],
                                   [
                                     Parsetree.Pcoerce(None, x2)
                                   ; Parsetree.Pcoerce(Some x1, x2)
                                   ; Parsetree.Pcoerce(ox1, x2)
                                   ]
             ))
           ; ([%typ: type_constraint Ploc.vala option Ploc.vala], AddDel (
                                   [],
                                   [
                                     Some x1
                                   ; ox1
                                   ]
             ))
          ])
#endif
        ; (Pexp_field,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
             ])
          ])
        ; (Pexp_setfield,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
             ])
          ])
        ; (Pexp_setfield,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
             ])
          ])
        ; (Pexp_for, [ ([%typ: direction_flag], Auto) ])
        ; (Pexp_new,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
             ])
          ])
        ; (Pexp_object, [ ([%typ: class_structure], Auto) ])
        ; (Pexp_constant, [ ([%typ: constant], Auto) ])
        ; (Ppat_constant, [ ([%typ: constant], Auto) ])
        ; (Psig_typext, [ ([%typ: longident_t], Explicit [
                                            Longident.Lident s
                                          ; Longident.Ldot (x, s)
                          ])
          ])
        ; (Pstr_typext, [ ([%typ: longident_t], Explicit [
                                            Longident.Lident s
                                          ; Longident.Ldot (x, s)
                          ])
          ])
        ; (Pcl_constr,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
             ])
          ])
        ; (Pcty_constr,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
             ])
          ])
        ; (Pvc_coercion,
           [ ([%typ: core_type option],
              AddDel(
                  [],
                  [ox1]
                )
             )
          ])
        ; (Pwith_type,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
             ])
          ])
        ; (Pwith_typesubst,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
             ])
          ])
        ; (Pwith_modtype,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
             ])
          ])
        ; (Pwith_modtypesubst,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
                             ; Longident.Ldot (x, s)
             ])
          ])
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
        ; function_body = Parsetree
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
        ; type_constraint = Parsetree
        ; type_declaration = Parsetree
        ; type_exception = Parsetree
        ; type_extension = Parsetree
        ; type_kind = Parsetree
        ; value_binding = Parsetree
        ; value_constraint = Parsetree
        ; value_description = Parsetree
        ; with_constraint = Parsetree
        }
      ; prefix_of_type = [
          ([%typ: (core_type * variance) list Ploc.vala], lxxx)
        ]
      }
  ]
