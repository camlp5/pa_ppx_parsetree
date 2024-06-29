(**pp -package pa_ppx.import,pa_ppx_migrate -syntax camlp5o *)

module SRC = Reorg_parsetree
module DST = Reorg_parsetree

exception Migration_error of string * SRC.location option

let migration_error location feature =
  raise (Migration_error (feature, location))

let _migrate_list subrw0 __dt__ l =
  List.map (subrw0 __dt__) l

[%%import: Reorg_parsetree.attribute
]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = SRC
        ; dstmod = DST
        ; types = [
            located
          ; longident_t
          ; arg_label
          ; closed_flag
          ; direction_flag
          ; label
          ; mutable_flag
          ; override_flag
          ; private_flag
          ; rec_flag
          ; virtual_flag
          ; variance
#if OCAML_VERSION >= (4,12,0)
          ; injectivity
#endif
          ; attribute
          ; attributes
          ; binding_op
          ; case
          ; class_declaration
          ; class_description
          ; class_expr
          ; class_expr_desc
          ; class_field
          ; class_field_desc
          ; class_field_kind
          ; class_signature
          ; class_structure
          ; class_type
          ; class_type_declaration
          ; class_type_desc
          ; class_type_field
          ; class_type_field_desc
          ; constant
          ; constructor_arguments
          ; constructor_declaration
          ; core_type
          ; core_type_desc
          ; expression
          ; expression_desc
          ; extension
          ; extension_constructor
          ; extension_constructor_kind
#if OCAML_VERSION >= (5,2,0)
          ; function_body
          ; function_param
          ; function_param_desc
#endif
          ; functor_parameter
          ; include_declaration
          ; include_description
          ; label_declaration
          ; letop
          ; location_stack
          ; module_binding
          ; module_declaration
          ; module_expr
          ; module_expr_desc
          ; module_substitution
          ; module_type
          ; module_type_declaration
          ; module_type_desc
          ; object_field
          ; object_field_desc
          ; open_declaration
          ; open_description
          ; package_type
          ; pattern
          ; pattern_desc
          ; payload
          ; row_field
          ; row_field_desc
          ; signature
          ; signature_item
          ; signature_item_desc
          ; structure
          ; structure_item
          ; structure_item_desc
#if OCAML_VERSION >= (5,2,0)
          ; type_constraint
#endif
          ; type_declaration
          ; type_exception
          ; type_extension
          ; type_kind
          ; value_binding
#if OCAML_VERSION >= (5,1,0)
          ; value_constraint
#endif
          ; value_description
          ; with_constraint
          ]
        }
      ]
    ; dispatchers = {
        migrate_option = {
          srctype = [%typ: 'a option]
        ; dsttype = [%typ: 'b option]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; code = (fun subrw __dt__ x -> Option.map (subrw __dt__) x)
        }
      ; migrate_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _migrate_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; migrate_location = {
          srctype = [%typ: location]
        ; dsttype = [%typ: DST.location]
        ; code = fun _ x -> x
        }
      }
    }
]
