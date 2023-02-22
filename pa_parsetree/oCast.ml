(** -syntax camlp5r *)

module Lexing =
  struct
    type position =

        { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
    ;
  end
;
module Location =
  struct
    type t =

        { loc_start : Lexing.position;
          loc_end : Lexing.position;
          loc_ghost : bool }
    ;
    type loc α =  { txt : α; loc : t };
  end
;
module Longident =
  struct
    type t =

        [ Lident of string
        | Ldot of t and string
        | Lapply of t and t ]
    ;
  end
;
module Asttypes =
  struct
    type loc α = Location.loc α ;
    type arg_label =

        [ Nolabel
        | Labelled of string
        | Optional of string ]
    ;
    type label = string;
    type closed_flag =  [ Closed | Open ];
    type rec_flag =  [ Nonrecursive | Recursive ];
    type direction_flag =  [ Upto | Downto ];
    type private_flag =  [ Private | Public ];
    type mutable_flag =  [ Immutable | Mutable ];
    type virtual_flag =  [ Virtual | Concrete ];
    type override_flag =  [ Override | Fresh ];
    type variance =
       [ Covariant | Contravariant | NoVariance ]
    ;
    type injectivity =  [ Injective | NoInjectivity ];
  end
;
type constant =
  
    [ Pconst_integer of string and option char
    | Pconst_char of char
    | Pconst_string of string and Location.t and option string
    | Pconst_float of string and option char ]
;
type location_stack = list Location.t;
type attribute =
  
    { attr_name : Asttypes.loc string;
      attr_payload : payload;
      attr_loc : Location.t }
and extension = (Asttypes.loc string * payload)
and attributes = list attribute
and payload =
  
    [ PStr of structure
    | PSig of signature
    | PTyp of core_type
    | PPat of pattern and option expression ]
and core_type =
  
    { ptyp_desc : core_type_desc;
      ptyp_loc : Location.t;
      ptyp_loc_stack : location_stack;
      ptyp_attributes : attributes }
and core_type_desc =
  
    [ Ptyp_any
    | Ptyp_var of string
    | Ptyp_arrow of Asttypes.arg_label and core_type and core_type
    | Ptyp_tuple of list core_type
    | Ptyp_constr of Asttypes.loc Longident.t and list core_type
    | Ptyp_object of list object_field and Asttypes.closed_flag
    | Ptyp_class of Asttypes.loc Longident.t and list core_type
    | Ptyp_alias of core_type and string
    | Ptyp_variant of
        list row_field and Asttypes.closed_flag and
          option (list Asttypes.label)
    | Ptyp_poly of list (Asttypes.loc string) and core_type
    | Ptyp_package of package_type
    | Ptyp_extension of extension ]
and package_type =
  (Asttypes.loc Longident.t * list (Asttypes.loc Longident.t * core_type))
and row_field =
  
    { prf_desc : row_field_desc;
      prf_loc : Location.t;
      prf_attributes : attributes }
and row_field_desc =
  
    [ Rtag of Asttypes.loc Asttypes.label and bool and list core_type
    | Rinherit of core_type ]
and object_field =
  
    { pof_desc : object_field_desc;
      pof_loc : Location.t;
      pof_attributes : attributes }
and object_field_desc =
  
    [ Otag of Asttypes.loc Asttypes.label and core_type
    | Oinherit of core_type ]
and pattern =
  
    { ppat_desc : pattern_desc;
      ppat_loc : Location.t;
      ppat_loc_stack : location_stack;
      ppat_attributes : attributes }
and pattern_desc =
  
    [ Ppat_any
    | Ppat_var of Asttypes.loc string
    | Ppat_alias of pattern and Asttypes.loc string
    | Ppat_constant of constant
    | Ppat_interval of constant and constant
    | Ppat_tuple of list pattern
    | Ppat_construct of
        Asttypes.loc Longident.t and
          option (list (Asttypes.loc string) * pattern)
    | Ppat_variant of Asttypes.label and option pattern
    | Ppat_record of
        list (Asttypes.loc Longident.t * pattern) and Asttypes.closed_flag
    | Ppat_array of list pattern
    | Ppat_or of pattern and pattern
    | Ppat_constraint of pattern and core_type
    | Ppat_type of Asttypes.loc Longident.t
    | Ppat_lazy of pattern
    | Ppat_unpack of Asttypes.loc (option string)
    | Ppat_exception of pattern
    | Ppat_extension of extension
    | Ppat_open of Asttypes.loc Longident.t and pattern ]
and expression =
  
    { pexp_desc : expression_desc;
      pexp_loc : Location.t;
      pexp_loc_stack : location_stack;
      pexp_attributes : attributes }
and expression_desc =
  
    [ Pexp_ident of Asttypes.loc Longident.t
    | Pexp_constant of constant
    | Pexp_let of Asttypes.rec_flag and list value_binding and expression
    | Pexp_function of list case
    | Pexp_fun of
        Asttypes.arg_label and option expression and pattern and expression
    | Pexp_apply of expression and list (Asttypes.arg_label * expression)
    | Pexp_match of expression and list case
    | Pexp_try of expression and list case
    | Pexp_tuple of list expression
    | Pexp_construct of Asttypes.loc Longident.t and option expression
    | Pexp_variant of Asttypes.label and option expression
    | Pexp_record of
        list (Asttypes.loc Longident.t * expression) and option expression
    | Pexp_field of expression and Asttypes.loc Longident.t
    | Pexp_setfield of expression and Asttypes.loc Longident.t and expression
    | Pexp_array of list expression
    | Pexp_ifthenelse of expression and expression and option expression
    | Pexp_sequence of expression and expression
    | Pexp_while of expression and expression
    | Pexp_for of
        pattern and expression and expression and Asttypes.direction_flag and
          expression
    | Pexp_constraint of expression and core_type
    | Pexp_coerce of expression and option core_type and core_type
    | Pexp_send of expression and Asttypes.loc Asttypes.label
    | Pexp_new of Asttypes.loc Longident.t
    | Pexp_setinstvar of Asttypes.loc Asttypes.label and expression
    | Pexp_override of list (Asttypes.loc Asttypes.label * expression)
    | Pexp_letmodule of
        Asttypes.loc (option string) and module_expr and expression
    | Pexp_letexception of extension_constructor and expression
    | Pexp_assert of expression
    | Pexp_lazy of expression
    | Pexp_poly of expression and option core_type
    | Pexp_object of class_structure
    | Pexp_newtype of Asttypes.loc string and expression
    | Pexp_pack of module_expr
    | Pexp_open of open_declaration and expression
    | Pexp_letop of letop
    | Pexp_extension of extension
    | Pexp_unreachable ]
and case =
  
    { pc_lhs : pattern; pc_guard : option expression; pc_rhs : expression }
and letop =
  
    { let_ : binding_op; ands : list binding_op; body : expression }
and binding_op =
  
    { pbop_op : Asttypes.loc string;
      pbop_pat : pattern;
      pbop_exp : expression;
      pbop_loc : Location.t }
and value_description =
  
    { pval_name : Asttypes.loc string;
      pval_type : core_type;
      pval_prim : list string;
      pval_attributes : attributes;
      pval_loc : Location.t }
and type_declaration =
  
    { ptype_name : Asttypes.loc string;
      ptype_params :
        list (core_type * (Asttypes.variance * Asttypes.injectivity));
      ptype_cstrs : list (core_type * core_type * Location.t);
      ptype_kind : type_kind;
      ptype_private : Asttypes.private_flag;
      ptype_manifest : option core_type;
      ptype_attributes : attributes;
      ptype_loc : Location.t }
and type_kind =
  
    [ Ptype_abstract
    | Ptype_variant of list constructor_declaration
    | Ptype_record of list label_declaration
    | Ptype_open ]
and label_declaration =
  
    { pld_name : Asttypes.loc string;
      pld_mutable : Asttypes.mutable_flag;
      pld_type : core_type;
      pld_loc : Location.t;
      pld_attributes : attributes }
and constructor_declaration =
  
    { pcd_name : Asttypes.loc string;
      pcd_vars : list (Asttypes.loc string);
      pcd_args : constructor_arguments;
      pcd_res : option core_type;
      pcd_loc : Location.t;
      pcd_attributes : attributes }
and constructor_arguments =
  
    [ Pcstr_tuple of list core_type
    | Pcstr_record of list label_declaration ]
and type_extension =
  
    { ptyext_path : Asttypes.loc Longident.t;
      ptyext_params :
        list (core_type * (Asttypes.variance * Asttypes.injectivity));
      ptyext_constructors : list extension_constructor;
      ptyext_private : Asttypes.private_flag;
      ptyext_loc : Location.t;
      ptyext_attributes : attributes }
and extension_constructor =
  
    { pext_name : Asttypes.loc string;
      pext_kind : extension_constructor_kind;
      pext_loc : Location.t;
      pext_attributes : attributes }
and type_exception =
  
    { ptyexn_constructor : extension_constructor;
      ptyexn_loc : Location.t;
      ptyexn_attributes : attributes }
and extension_constructor_kind =
  
    [ Pext_decl of
        list (Asttypes.loc string) and constructor_arguments and
          option core_type
    | Pext_rebind of Asttypes.loc Longident.t ]
and class_type =
  
    { pcty_desc : class_type_desc;
      pcty_loc : Location.t;
      pcty_attributes : attributes }
and class_type_desc =
  
    [ Pcty_constr of Asttypes.loc Longident.t and list core_type
    | Pcty_signature of class_signature
    | Pcty_arrow of Asttypes.arg_label and core_type and class_type
    | Pcty_extension of extension
    | Pcty_open of open_description and class_type ]
and class_signature =
  
    { pcsig_self : core_type; pcsig_fields : list class_type_field }
and class_type_field =
  
    { pctf_desc : class_type_field_desc;
      pctf_loc : Location.t;
      pctf_attributes : attributes }
and class_type_field_desc =
  
    [ Pctf_inherit of class_type
    | Pctf_val of
        (Asttypes.loc Asttypes.label * Asttypes.mutable_flag *
         Asttypes.virtual_flag * core_type)
    | Pctf_method of
        (Asttypes.loc Asttypes.label * Asttypes.private_flag *
         Asttypes.virtual_flag * core_type)
    | Pctf_constraint of (core_type * core_type)
    | Pctf_attribute of attribute
    | Pctf_extension of extension ]
and class_infos α =
  
    { pci_virt : Asttypes.virtual_flag;
      pci_params :
        list (core_type * (Asttypes.variance * Asttypes.injectivity));
      pci_name : Asttypes.loc string;
      pci_expr : α;
      pci_loc : Location.t;
      pci_attributes : attributes }
and class_description = class_infos class_type
and class_type_declaration = class_infos class_type
and class_expr =
  
    { pcl_desc : class_expr_desc;
      pcl_loc : Location.t;
      pcl_attributes : attributes }
and class_expr_desc =
  
    [ Pcl_constr of Asttypes.loc Longident.t and list core_type
    | Pcl_structure of class_structure
    | Pcl_fun of
        Asttypes.arg_label and option expression and pattern and class_expr
    | Pcl_apply of class_expr and list (Asttypes.arg_label * expression)
    | Pcl_let of Asttypes.rec_flag and list value_binding and class_expr
    | Pcl_constraint of class_expr and class_type
    | Pcl_extension of extension
    | Pcl_open of open_description and class_expr ]
and class_structure =
  
    { pcstr_self : pattern; pcstr_fields : list class_field }
and class_field =
  
    { pcf_desc : class_field_desc;
      pcf_loc : Location.t;
      pcf_attributes : attributes }
and class_field_desc =
  
    [ Pcf_inherit of
        Asttypes.override_flag and class_expr and option (Asttypes.loc string)
    | Pcf_val of
        (Asttypes.loc Asttypes.label * Asttypes.mutable_flag *
         class_field_kind)
    | Pcf_method of
        (Asttypes.loc Asttypes.label * Asttypes.private_flag *
         class_field_kind)
    | Pcf_constraint of (core_type * core_type)
    | Pcf_initializer of expression
    | Pcf_attribute of attribute
    | Pcf_extension of extension ]
and class_field_kind =
  
    [ Cfk_virtual of core_type
    | Cfk_concrete of Asttypes.override_flag and expression ]
and class_declaration = class_infos class_expr
and module_type =
  
    { pmty_desc : module_type_desc;
      pmty_loc : Location.t;
      pmty_attributes : attributes }
and module_type_desc =
  
    [ Pmty_ident of Asttypes.loc Longident.t
    | Pmty_signature of signature
    | Pmty_functor of functor_parameter and module_type
    | Pmty_with of module_type and list with_constraint
    | Pmty_typeof of module_expr
    | Pmty_extension of extension
    | Pmty_alias of Asttypes.loc Longident.t ]
and functor_parameter =
  
    [ Unit
    | Named of Asttypes.loc (option string) and module_type ]
and signature = list signature_item
and signature_item =
  
    { psig_desc : signature_item_desc; psig_loc : Location.t }
and signature_item_desc =
  
    [ Psig_value of value_description
    | Psig_type of Asttypes.rec_flag and list type_declaration
    | Psig_typesubst of list type_declaration
    | Psig_typext of type_extension
    | Psig_exception of type_exception
    | Psig_module of module_declaration
    | Psig_modsubst of module_substitution
    | Psig_recmodule of list module_declaration
    | Psig_modtype of module_type_declaration
    | Psig_modtypesubst of module_type_declaration
    | Psig_open of open_description
    | Psig_include of include_description
    | Psig_class of list class_description
    | Psig_class_type of list class_type_declaration
    | Psig_attribute of attribute
    | Psig_extension of extension and attributes ]
and module_declaration =
  
    { pmd_name : Asttypes.loc (option string);
      pmd_type : module_type;
      pmd_attributes : attributes;
      pmd_loc : Location.t }
and module_substitution =
  
    { pms_name : Asttypes.loc string;
      pms_manifest : Asttypes.loc Longident.t;
      pms_attributes : attributes;
      pms_loc : Location.t }
and module_type_declaration =
  
    { pmtd_name : Asttypes.loc string;
      pmtd_type : option module_type;
      pmtd_attributes : attributes;
      pmtd_loc : Location.t }
and open_infos α =
  
    { popen_expr : α;
      popen_override : Asttypes.override_flag;
      popen_loc : Location.t;
      popen_attributes : attributes }
and open_description = open_infos (Asttypes.loc Longident.t)
and open_declaration = open_infos module_expr
and include_infos α =
  
    { pincl_mod : α; pincl_loc : Location.t; pincl_attributes : attributes }
and include_description = include_infos module_type
and include_declaration = include_infos module_expr
and with_constraint =
  
    [ Pwith_type of Asttypes.loc Longident.t and type_declaration
    | Pwith_module of Asttypes.loc Longident.t and Asttypes.loc Longident.t
    | Pwith_modtype of Asttypes.loc Longident.t and module_type
    | Pwith_modtypesubst of Asttypes.loc Longident.t and module_type
    | Pwith_typesubst of Asttypes.loc Longident.t and type_declaration
    | Pwith_modsubst of
        Asttypes.loc Longident.t and Asttypes.loc Longident.t ]
and module_expr =
  
    { pmod_desc : module_expr_desc;
      pmod_loc : Location.t;
      pmod_attributes : attributes }
and module_expr_desc =
  
    [ Pmod_ident of Asttypes.loc Longident.t
    | Pmod_structure of structure
    | Pmod_functor of functor_parameter and module_expr
    | Pmod_apply of module_expr and module_expr
    | Pmod_constraint of module_expr and module_type
    | Pmod_unpack of expression
    | Pmod_extension of extension ]
and structure = list structure_item
and structure_item =
  
    { pstr_desc : structure_item_desc; pstr_loc : Location.t }
and structure_item_desc =
  
    [ Pstr_eval of expression and attributes
    | Pstr_value of Asttypes.rec_flag and list value_binding
    | Pstr_primitive of value_description
    | Pstr_type of Asttypes.rec_flag and list type_declaration
    | Pstr_typext of type_extension
    | Pstr_exception of type_exception
    | Pstr_module of module_binding
    | Pstr_recmodule of list module_binding
    | Pstr_modtype of module_type_declaration
    | Pstr_open of open_declaration
    | Pstr_class of list class_declaration
    | Pstr_class_type of list class_type_declaration
    | Pstr_include of include_declaration
    | Pstr_attribute of attribute
    | Pstr_extension of extension and attributes ]
and value_binding =
  
    { pvb_pat : pattern;
      pvb_expr : expression;
      pvb_attributes : attributes;
      pvb_loc : Location.t }
and module_binding =
  
    { pmb_name : Asttypes.loc (option string);
      pmb_expr : module_expr;
      pmb_attributes : attributes;
      pmb_loc : Location.t }
;
