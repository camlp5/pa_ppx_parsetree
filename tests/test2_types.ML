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
        ; Pstr_xtr
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
      ; expand_test_types = [
          ([%typ: type_declaration],
           DelPatts [
               [%patt? {ptype_kind = Parsetree.Ptype_abstract; ptype_private = Asttypes.Private; ptype_manifest = None}]
             ; [%patt? {ptype_kind = Parsetree.Ptype_abstract; ptype_private = x; ptype_manifest = None}]
#if OVERS >= 505
             ; [%patt? {ptype_kind = Parsetree.Ptype_external _; ptype_private = (x|Asttypes.Private)}]
             ; [%patt? {ptype_kind = Parsetree.Ptype_external s; ptype_manifest = Some _}]
#endif
             ]
          )
          ; ([%typ: class_expr],
           DelPatts [
               [%patt? {Parsetree.pcl_desc = Parsetree.Pcl_open({popen_attributes = attrs}, ce)}]
             ]
          )
          ; ([%typ: class_type],
           DelPatts [
               [%patt? {Parsetree.pcty_desc = Parsetree.Pcty_arrow(_,_,_); pcty_attributes = x}]
             ; [%patt? {Parsetree.pcty_desc = Parsetree.Pcty_open({Parsetree.popen_expr = _; popen_attributes = attrs}, ct)}]
             ]
          )
          ; ([%typ: class_type_field],
           DelPatts [
               [%patt? {Parsetree.pctf_desc = Parsetree.Pctf_attribute _; pctf_attributes = attrs}]
             ]
          )
          ; ([%typ: class_field],
           DelPatts [
               [%patt? {Parsetree.pcf_desc = Parsetree.Pcf_attribute _; pcf_attributes = attrs}]
             ]
          )
          ; ([%typ: signature_item],
           DelPatts [
               [%patt? {Parsetree.psig_desc = Parsetree.Psig_exception {Parsetree.ptyexn_constructor = {pext_kind = Parsetree.Pext_rebind _}}}]
             ; [%patt? {Parsetree.psig_desc = Parsetree.Psig_modtypesubst {pmtd_type = None}}]
             ; [%patt? {Parsetree.psig_desc = Parsetree.Psig_modtypesubst {pmtd_type = x}}]
             ]
          )
        ]

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
        ; ([%typ: closed_flag], Auto)
#if OVERS >= 503
        ; ([%typ: constant_desc], Auto)
#endif
        ; ([%typ: core_type_desc], Auto)
        ; ([%typ: expression_desc], Auto)
        ; ([%typ: extension], Auto)
        ; ([%typ: extension_constructor], Auto)
        ; ([%typ: extension_constructor_kind], Auto)
#if OVERS >= 502
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
        ; ([%typ: object_field], 
           DelPatts [
               [%patt? {Parsetree.pof_desc = Parsetree.Oinherit x; pof_attributes = attrs}]
             ]
          )
        ; ([%typ: open_declaration], Auto)
        ; ([%typ: open_description], Auto)
        ; ([%typ: open_infos], Auto)
        ; ([%typ: open_infos], Auto)
        ; ([%typ: package_type], Auto)
        ; ([%typ: pattern_desc], Auto)
        ; ([%typ: payload], Auto)
        ; ([%typ: row_field_desc], Auto)
        ; ([%typ: row_field], 
           DelPatts [
               [%patt? {Parsetree.prf_desc = Parsetree.Rinherit _; prf_attributes = attrs}]
             ]
          )
        ; ([%typ: signature_item_desc], Auto)
        ; ([%typ: structure_item_desc], Auto
          )
#if OVERS >= 502
        ; ([%typ: type_constraint], Auto)
#endif
        ; ([%typ: type_exception], Auto)
        ; ([%typ: type_extension], Auto)
        ; ([%typ: type_kind], Auto)
#if OVERS >= 501
        ; ([%typ: value_constraint], Auto)
#endif
        ; ([%typ: value_description], Auto)
        ]
      ; expand_types_per_type = {
          type_declaration = [
            ([%typ: private_flag], Auto)
          ; ([%typ: type_declaration], Auto)
          ]
        ; value_description = [
            ([%typ: string list], Explicit [
                                      l
                                    ; []
            ])
          ]

(*
        ; value_binding = [
            ([%typ: value_constraint option],Auto
            )
          ]
 *)
        }
      ; expand_types_per_constructor = [
          (Pexp_ident,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
             ])
          ])
        ; (Ptyp_package,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
             ])
          ])
        ; (Ptyp_class,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
             ])
          ])
        ; (Ptyp_constr,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
             ])
          ])
        ; (Ptyp_functor,
           [ ([%typ: arg_label], Explicit [
                               Asttypes.Nolabel
                             ; Asttypes.Labelled m
             ])
          ])
        ; (Ppat_unpack,
           [ ([%typ: package_type], Auto)
          ])
        ; (Ppat_type,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
             ])
          ])
        ; (Pmty_ident,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
             ])
          ])
#if OVERS >= 502
(*
        ; (Pexp_function,
           [ ([%typ: type_constraint], Auto)
           ; ([%typ: type_constraint Ploc.vala option Ploc.vala], Auto)
          ])
 *)
#endif
        ; (Pexp_field,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
             ])
          ])
        ; (Pexp_setfield,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
             ])
          ])
        ; (Pexp_setfield,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
             ])
          ])
        ; (Pexp_for, [ ([%typ: direction_flag], Auto) ])
        ; (Pexp_new,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
             ])
          ])
        ; (Pexp_object, [ ([%typ: class_structure], Auto) ])
        ; (Pexp_pack,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
             ])
          ])
        ; (Pexp_constant, [ ([%typ: constant], Auto) ])
        ; (Ppat_constant, [ ([%typ: constant], Auto) ])
        ; (Psig_typext, [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
                          ])
          ])
        ; (Pstr_typext, [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
                          ])
          ])
        ; (Pcl_constr,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
             ])
          ])
        ; (Pcty_constr,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
             ])
          ])
(*
        ; (Pvc_coercion,
           [ ([%typ: core_type option],Auto
             )
          ])
 *)
        ; (Pwith_type,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
             ])
          ])
        ; (Pwith_typesubst,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
             ])
          ])
        ; (Pwith_modtype,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
             ])
          ])
        ; (Pwith_modtypesubst,
           [ ([%typ: longident_t], Explicit [
                               Longident.Lident s
#if OVERS >= 504
                             ; Longident.Ldot ({Location.txt=x; loc=loc}, {Location.txt=s; loc=loc})
#else
                             ; Longident.Ldot (x, s)
#endif
             ])
          ])
        ; (Pmod_structure,
           [ ([%typ: structure], Explicit [
                               l
                             ; []
                             ; [x]
                             ; [x;y]
             ])
          ])
        ; (Psig_value,
           [ ([%typ: string list], Explicit [
                               l
                             ; []
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
        ; (Ptyp_variant, DelPatts [
             [%patt? Parsetree.Ptyp_variant(_, Asttypes.Open, Some _)]
           ; [%patt? Parsetree.Ptyp_variant(_, Asttypes.Open, x)]
          ])
        ; (Ppat_record, DelPatts [
             [%patt? Parsetree.Ppat_record(x00, (Asttypes.Closed|Asttypes.Open))]
          ])
        ; (Pexp_function, DelPatts [
             [%patt? Parsetree.Pexp_function(_, Some (Parsetree.Pcoerce(_, _)), _)]
          ])
        ; (Pexp_letexception, DelPatts [
             [%patt? Parsetree.Pexp_letexception({pext_kind = Parsetree.Pext_rebind _}, _)]
          ])
        ; (Pexp_poly, DelPatts [
             [%patt? Parsetree.Pexp_poly(_, _)]
          ])
        ; (Pexp_open, DelPatts [
             [%patt? Parsetree.Pexp_open({popen_attributes = attrs}, _)]
          ])
        ; (Pexp_unreachable, DelPatts [
             [%patt? Parsetree.Pexp_unreachable]
          ])
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
        ; closed_flag = Asttypes
        ; constant = Parsetree
#if OVERS >= 503
        ; constant_desc = Parsetree
#endif
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
        ; package_type = Parsetree
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
