(** -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools
open MLast
open Pcaml

open Pa_camlp5
open Q_ast 

module Regular = struct

module MetaE = struct
  include Q_ast_base.E_MetaSig
  let int n = let loc = Ploc.dummy in <:expr< $int:string_of_int n$ >>
end

module MetaP = struct
  include Q_ast_base.P_MetaSig
  let int n = let loc = Ploc.dummy in <:patt< $int:string_of_int n$ >>
end

[%%import: Camlp5_ast.expr]
[@@deriving q_ast {
    data_source_module = Camlp5_ast
  ; expr_meta_module = MetaE
  ; patt_meta_module = MetaP
 }]

end

module OK = struct
module MetaE = Regular.MetaE
module MetaP = Regular.MetaP

[%%import: Camlp5_hashcons.OK.expr]
[@@deriving q_ast {
    data_source_module = Camlp5_hashcons.OK
  ; expr_meta_module = MetaE
  ; patt_meta_module = MetaP
  ; pertype = {
      class_expr_node = {
        custom_branches_code = function
          | CeXtr (loc, s, _) → C.xtr loc s
      }

    ; class_type_node = {
        custom_branches_code = function
          | CtXtr (loc, s, _) → C.xtr loc s
      }

    ; ctyp_node = {
        custom_branches_code = function
          | TyXtr(loc, s, _) → C.xtr loc s
      }

    ; expr_node = {
        custom_branches_code = function
          | ExXtr(loc, s, _) → C.xtr_or_anti loc (fun r → C.node "ExAnt" [r]) s
      }

    ; longid_node = {
        custom_branches_code = function
          | LiXtr(loc, s, _) → C.xtr_typed "longid" loc s
      }

    ; module_type_node = {
        custom_branches_code = function
          | MtXtr (loc, s, _) → C.xtr loc s          
      }

    ; module_expr_node = {
        custom_branches_code = function
          | MeXtr (loc, s, _) → C.xtr loc s          
      }

    ; patt_node = {
        custom_branches_code = function
          | PaXtr (loc, s, _) → C.xtr_or_anti loc (fun r → C.node "PaAnt" [r]) s
      }
    }
  }]

List.map (fun (n,f) -> Quotation.add n f)
[
("okattribute_body",apply_entry Pa_camlp5.attribute_body_eoi E.attribute_body P.attribute_body);
("okclass_expr",apply_entry Pa_camlp5.class_expr_eoi E.class_expr P.class_expr);
("okclass_sig_item",apply_entry Pa_camlp5.class_sig_item_eoi E.class_sig_item P.class_sig_item);
("okclass_str_item",apply_entry Pa_camlp5.class_str_item_eoi E.class_str_item P.class_str_item);
("okclass_type",apply_entry Pa_camlp5.class_type_eoi E.class_type P.class_type);
("okctyp",apply_entry Pa_camlp5.ctyp_eoi E.ctyp P.ctyp);
("okexpr",apply_entry Pa_camlp5.expr_eoi E.expr P.expr);
("okextension_constructor",apply_entry Pa_camlp5.extension_constructor_eoi E.extension_constructor P.extension_constructor);
("okconstructor",apply_entry Pa_camlp5.constructor_eoi E.generic_constructor P.generic_constructor);
("okextended_longident",apply_entry Pa_camlp5.extended_longident_eoi E.longid P.longid);
("oklongident",apply_entry Pa_camlp5.longident_eoi E.longid P.longid);
("okmodule_expr",apply_entry Pa_camlp5.module_expr_eoi E.module_expr P.module_expr);
("okmodule_type",apply_entry Pa_camlp5.module_type_eoi E.module_type P.module_type);
("okpatt",apply_entry Pa_camlp5.patt_eoi E.patt P.patt);
("okpoly_variant",apply_entry Pa_camlp5.poly_variant_eoi E.poly_variant P.poly_variant);
("oksig_item",apply_entry Pa_camlp5.sig_item_eoi E.sig_item P.sig_item);
("okstr_item",apply_entry Pa_camlp5.str_item_eoi E.str_item P.str_item);
("oktype_decl",apply_entry Pa_camlp5.type_decl_eoi E.type_decl P.type_decl);
("oktype_extension",apply_entry Pa_camlp5.type_extension_eoi E.type_extension P.type_extension);
("okwith_constr",apply_entry Pa_camlp5.with_constr_eoi E.with_constr P.with_constr);
]
end

module Hashcons = struct

module MetaE = struct
  include Regular.MetaE
  let app_no_loc ?prefix fid el =
    let prefix = match prefix with None -> <:longident< MLast >> | Some p -> p in
    List.fold_left (fun e1 e2 -> <:expr< $e1$ $e2$ >>)
      <:expr< $longid:prefix$ . $lid:fid$ >> el
end

module MetaP = struct
  include Regular.MetaP
end

[%%import: Camlp5_hashcons.HC.expr]
[@@deriving q_ast {
    data_source_module = Camlp5_hashcons.HC
  ; quotation_source_module = Camlp5_migrate.FromHC
  ; expr_meta_module = MetaE
  ; patt_meta_module = MetaP
  ; hashconsed = true
  ; pertype = {
      class_expr_node = {
        custom_branches_code = function
          | CeXtr (loc, s, _) → C.xtr loc s
      }
    ; class_expr = {
        add_branches_patt_code = function
          | {Hashcons.node=CeXtr (loc, s, _)} → C.xtr loc s
      }

    ; class_type_node = {
        custom_branches_code = function
          | CtXtr (loc, s, _) → C.xtr loc s
      }
    ; class_type = {
        add_branches_patt_code = function
          | {Hashcons.node=CtXtr (loc, s, _)} → C.xtr loc s
      }

    ; ctyp_node = {
        custom_branches_code = function
          | TyXtr(loc, s, _) → C.xtr loc s
      }

    ; ctyp = {
        add_branches_patt_code = function
          | {Hashcons.node=TyXtr(loc, s, _)} → C.xtr loc s
      }

    ; expr_node = {
        custom_branches_code = function
          | ExXtr(loc, s, _) → C.xtr_or_anti loc (fun r → C.node "ExAnt" [r]) s
      }
    ; expr = {
        add_branches_patt_code = function
          | {Hashcons.node=ExXtr(loc, s, _)} → C.xtr_or_anti loc (fun r → C.node "ExAnt" [r]) s
      }

    ; longid_node = {
        custom_branches_code = function
          | LiXtr(loc, s, _) → C.xtr_typed "longid" loc s
      }
    ; longid = {
        add_branches_patt_code = function
          | {Hashcons.node=LiXtr(loc, s, _)} → C.xtr_typed "longid" loc s
      }

    ; module_expr_node = {
        custom_branches_code = function
          | MeXtr (loc, s, _) → C.xtr loc s          
      }

    ; module_expr = {
        add_branches_patt_code = function
          | {Hashcons.node=MeXtr (loc, s, _)} → C.xtr loc s          
      }


    ; module_type_node = {
        custom_branches_code = function
          | MtXtr (loc, s, _) → C.xtr loc s          
      }

    ; module_type = {
        add_branches_patt_code = function
          | {Hashcons.node=MtXtr (loc, s, _)} → C.xtr loc s          
      }

    ; patt_node = {
        custom_branches_code = function
          | PaXtr (loc, s, _) → C.xtr_or_anti loc (fun r → C.node "PaAnt" [r]) s
      }

    ; patt = {
        add_branches_patt_code = function
          | {Hashcons.node=PaXtr (loc, s, _)} → C.xtr_or_anti loc (fun r → C.node "PaAnt" [r]) s
      }

    }
  }]
;;
let hc_apply_entry = Pa_ppx_q_ast_runtime.hc_apply_entry
;;
List.map (fun (n,f) -> Quotation.add n f)
[
("hcattribute_body",hc_apply_entry Pa_camlp5.attribute_body_hashcons_eoi E.attribute_body P.attribute_body);
("hcclass_expr",hc_apply_entry Pa_camlp5.class_expr_hashcons_eoi E.class_expr P.class_expr);
("hcclass_sig_item",hc_apply_entry Pa_camlp5.class_sig_item_hashcons_eoi E.class_sig_item P.class_sig_item);
("hcclass_str_item",hc_apply_entry Pa_camlp5.class_str_item_hashcons_eoi E.class_str_item P.class_str_item);
("hcclass_type",hc_apply_entry Pa_camlp5.class_type_hashcons_eoi E.class_type P.class_type);
("hcctyp",hc_apply_entry Pa_camlp5.ctyp_hashcons_eoi E.ctyp P.ctyp);
("hcexpr",hc_apply_entry Pa_camlp5.expr_hashcons_eoi E.expr P.expr);
("hcextension_constructor",hc_apply_entry Pa_camlp5.extension_constructor_hashcons_eoi E.extension_constructor P.extension_constructor);
("hcconstructor",hc_apply_entry Pa_camlp5.constructor_hashcons_eoi E.generic_constructor P.generic_constructor);
("hcextended_longident",hc_apply_entry Pa_camlp5.extended_longident_hashcons_eoi E.longid P.longid);
("hclongident",hc_apply_entry Pa_camlp5.longident_hashcons_eoi E.longid P.longid);
("hcmodule_expr",hc_apply_entry Pa_camlp5.module_expr_hashcons_eoi E.module_expr P.module_expr);
("hcmodule_type",hc_apply_entry Pa_camlp5.module_type_hashcons_eoi E.module_type P.module_type);
("hcpatt",hc_apply_entry Pa_camlp5.patt_hashcons_eoi E.patt P.patt);
("hcpoly_variant",hc_apply_entry Pa_camlp5.poly_variant_hashcons_eoi E.poly_variant P.poly_variant);
("hcsig_item",hc_apply_entry Pa_camlp5.sig_item_hashcons_eoi E.sig_item P.sig_item);
("hcstr_item",hc_apply_entry Pa_camlp5.str_item_hashcons_eoi E.str_item P.str_item);
("hctype_decl",hc_apply_entry Pa_camlp5.type_decl_hashcons_eoi E.type_decl P.type_decl);
("hctype_extension",hc_apply_entry Pa_camlp5.type_extension_hashcons_eoi E.type_extension P.type_extension);
("hcwith_constr",hc_apply_entry Pa_camlp5.with_constr_hashcons_eoi E.with_constr P.with_constr);
]

end

