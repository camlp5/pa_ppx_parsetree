(**pp -package pa_ppx_parsetree_via_parsetree -syntax camlp5r *)
[@@@"ocaml.text" "attribute";];
<:attribute< [@ $attrid:s$ $list:x$] >>;
<:attribute< [@ $attrid:s$ : $list:x$] >>;
<:attribute< [@ $attrid:s$ : $x$] >>;
<:attribute< [@ $attrid:s$ ? $x1$] >>;
<:attribute< [@ $attrid:s$ ? $x1$ when $x2$] >>;
<:attribute< [@ $attrid:s$ ? $x1$ $expropt:ox2$] >>;
[@@@"ocaml.text" "extension";];
<:extension< [% $attrid:s$ $list:x$] >>;
<:extension< [% $attrid:s$ : $list:x$] >>;
<:extension< [% $attrid:s$ : $x$] >>;
<:extension< [% $attrid:s$ ? $x1$] >>;
<:extension< [% $attrid:s$ ? $x1$ when $x2$] >>;
<:extension< [% $attrid:s$ ? $x1$ $expropt:ox2$] >>;
[@@@"ocaml.text" "attributes";];
[@@@"ocaml.text" "payload";];
[@@@"ocaml.text" "core_type";];
<:core_type< _ >> ;
<:core_type< ' $lid:s$ >> ;
<:core_type< $label:x1$ $x2$ -> $x3$ >> ;
<:core_type< $tuplelist:lx$ >> ;
<:core_type< $list:lx$ $longid:x$ >> ;
<:core_type< < $list:lx$ $closedflag:x$ > >> ;
<:core_type< $list:lx$ # $longid:x$ >> ;
<:core_type< $x$ as ' $lid:s$ >> ;
<:core_type< [ $closedflag:x$ $list:lx1$ $opt:None$ ] >> ;
<:core_type< [ $closedflag:x$ $list:lx1$ > $list:lx2$ ] >> ;
<:core_type< [ $closedflag:x$ $list:lx1$ $opt:olx2$ ] >> ;
<:core_type< [< $list:lx1$ ] >> ;
{Parsetree.ptyp_desc = Parsetree.Ptyp_poly lx x; Parsetree.ptyp_loc = __loc__; Parsetree.ptyp_loc_stack = []; Parsetree.ptyp_attributes = []};
<:core_type< (module $longid:x$ with $list:xf2$) >>;
<:core_type< [% $attrid:s$ $list:x$] >>;
<:core_type< [% $attrid:s$ : $list:x$] >>;
<:core_type< [% $attrid:s$ : $x$] >>;
<:core_type< [% $attrid:s$ ? $x1$] >>;
<:core_type< [% $attrid:s$ ? $x1$ when $x2$] >>;
<:core_type< [% $attrid:s$ ? $x1$ $expropt:ox2$] >>;
[@@@"ocaml.text" "core_type_desc";];
[@@@"ocaml.text" "package_type";];
[@@@"ocaml.text" "row_field";];
<:row_field< ` $id:x$ of & $list:lx$ >> ;
<:row_field< ` $id:x$ of $list:lx$ >> ;
<:row_field< ` $id:x$ of $isconst:b$ $list:lx$ >> ;
<:row_field< $x$ >> ;
[@@@"ocaml.text" "row_field_desc";];
[@@@"ocaml.text" "object_field";];
<:object_field< $lid:x$ : $x2$ >> ;
<:object_field< $x$ >> ;
[@@@"ocaml.text" "object_field_desc";];
[@@@"ocaml.text" "pattern";];
<:pattern< _ >> ;
<:pattern<  $lid:s$ >> ;
<:pattern< $x1$ as $lid:s$ >> ;
<:pattern< $int:s$ >> ;
<:pattern< $int32:s$ >> ;
<:pattern< $int64:s$ >> ;
<:pattern< $nativeint:s$ >> ;
<:pattern< $char:x$ >> ;
<:pattern< $string:s1$ >> ;
<:pattern< $string:s1$ $delim:s2$ >> ;
{Parsetree.ppat_desc = Parsetree.Ppat_constant (Parsetree.Pconst_string s1 __loc__ os2); Parsetree.ppat_loc = __loc__; Parsetree.ppat_loc_stack = []; Parsetree.ppat_attributes = []};
<:pattern< $float:sxf1$ >> ;
{Parsetree.ppat_desc = Parsetree.Ppat_constant x; Parsetree.ppat_loc = __loc__; Parsetree.ppat_loc_stack = []; Parsetree.ppat_attributes = []};
<:pattern< $constant:x1$ .. $constant:x2$ >> ;
<:pattern<  $tuplelist:lx$ >>;
<:pattern< $longid:x$ >>;
<:pattern< $longid:x$ (type $list:lxxf1$) $lxxf2$ >>;
<:pattern< $longid:x$ $pattopt:olxx$ >>;
<:pattern< ` $id:x1$ >>;
<:pattern< ` $id:x1$ $x2$ >>;
<:pattern< ` $id:x1$ $pattopt:ox2$ >>;
<:pattern< { $list:lxx$ $closedflag:x$ } >> ;
<:pattern< [| $list:lx$ |] >>;
<:pattern< $x1$ | $x2$ >>;
<:pattern< ( $x1$ : $x2$ ) >>;
<:pattern< # $longid:x$ >>;
<:pattern< lazy $x$ >>;
<:pattern< (module _) >>;
<:pattern< (module $uid:s$) >>;
<:pattern< (module $opt:os$) >>;
<:pattern< exception $x$ >> ;
<:pattern< [% $attrid:s$ $list:x$] >>;
<:pattern< [% $attrid:s$ : $list:x$] >>;
<:pattern< [% $attrid:s$ : $x$] >>;
<:pattern< [% $attrid:s$ ? $x1$] >>;
<:pattern< [% $attrid:s$ ? $x1$ when $x2$] >>;
<:pattern< [% $attrid:s$ ? $x1$ $expropt:ox2$] >>;
<:pattern< $longid:x$ . $x2$ >>;
[@@@"ocaml.text" "pattern_desc";];
[@@@"ocaml.text" "expression";];
<:expression< $lid:s$ >>;
<:expression< $longid:x$ . $lid:s$ >>;
<:expression< $int:s$ >> ;
<:expression< $int32:s$ >> ;
<:expression< $int64:s$ >> ;
<:expression< $nativeint:s$ >> ;
<:expression< $char:x$ >> ;
<:expression< $string:s1$ >> ;
<:expression< $string:s1$ $delim:s2$ >> ;
{Parsetree.pexp_desc = Parsetree.Pexp_constant (Parsetree.Pconst_string s1 __loc__ os2); Parsetree.pexp_loc = __loc__; Parsetree.pexp_loc_stack = []; Parsetree.pexp_attributes = []};
<:expression< $float:sxf1$ >> ;
<:expression< $constant:x$ >> ;
<:expression< let $recflag:x1$ $list:lx$ in $x2$ >>;
<:expression< function $list:lx$ >>;
<:expression< fun $label:x1$ ( $x3$ ) -> $x4$ >>;
<:expression< fun $label:x1$ ( $x3$ = $x2$ ) -> $x4$ >>;
<:expression< fun $label:x1$ ( $x3$ $expropt:ox2$ ) -> $x4$ >>;
<:expression< $x$ $list:lxx$ >>;
<:expression< match $x$ with $list:lx$ >>;
<:expression< try $x$ with $list:lx$ >>;
<:expression<  $tuplelist:lx$ >>;
<:expression< $longid:x$ >>;
<:expression< $longid:x$ $x2$ >>;
<:expression< $longid:x$ $expropt:ox2$ >>;
<:expression< ` $id:x1$ >>;
<:expression< ` $id:x1$ $x2$ >>;
<:expression< ` $id:x1$ $expropt:ox2$ >>;
<:expression< { $list:lxx$ } >>;
<:expression< { $x$ with $list:lxx$ } >>;
<:expression< { $withe:ox$ $list:lxx$ } >>;
<:expression< $x1$ . $lid:s$ >>;
<:expression< $x1$ . $longid:x$ . $lid:s$ >>;
{Parsetree.pexp_desc = Parsetree.Pexp_field x1 {Location.txt = x; Location.loc = __loc__}; Parsetree.pexp_loc = __loc__; Parsetree.pexp_loc_stack = []; Parsetree.pexp_attributes = []};
<:expression< $x1$ . $lid:s$ <- $x3$ >>;
<:expression< $x1$ . $longid:x$ . $lid:s$ <- $x3$ >>;
{Parsetree.pexp_desc = Parsetree.Pexp_setfield x1 {Location.txt = x; Location.loc = __loc__} x3; Parsetree.pexp_loc = __loc__; Parsetree.pexp_loc_stack = []; Parsetree.pexp_attributes = []};
<:expression< [| $list:lx$ |] >>;
<:expression< if $x1$ then $x2$ >>;
<:expression< if $x1$ then $x2$ else $x3$ >>;
<:expression< if $x1$ then $x2$ $expropt:ox3$ >>;
<:expression< $x1$ ; $x2$ >>;
<:expression< while $x1$ do $x2$ done >>;
<:expression< for $x1$ = $x2$ to $x3$ do $x5$ done >>;
<:expression< for $x1$ = $x2$ downto $x3$ do $x5$ done >>;
<:expression< for $x1$ = $x2$ $dirflag:x4$ $x3$ do $x5$ done >>;
<:expression< ( $x1$ : $x2$ ) >>;
<:expression< ( $x1$ :> $x3$ ) >>;
<:expression< ( $x1$ : $x2$ :> $x3$ ) >>;
<:expression< ( $x1$ $ctypopt:ox2$ :> $x3$ ) >>;
<:expression< $x1$ # $lid:x$ >>;
<:expression< new $lid:s$ >>;
<:expression< new $longid:x$ . $lid:s$ >>;
{Parsetree.pexp_desc = Parsetree.Pexp_new {Location.txt = x; Location.loc = __loc__}; Parsetree.pexp_loc = __loc__; Parsetree.pexp_loc_stack = []; Parsetree.pexp_attributes = []};
<:expression< $lid:x$ <- $x2$ >>;
<:expression< {< $list:lxx$ >} >>;
<:expression< let module _ = $me$ in $x2$ >>;
<:expression< let module $uid:s$ = $me$ in $x2$ >>;
<:expression< let module $opt:os$ = $me$ in $x2$ >>;
<:expression< let $excon:x1$ in $x2$ >>;
<:expression< assert $x$ >>;
<:expression< lazy $x$ >>;
{Parsetree.pexp_desc = Parsetree.Pexp_poly x1 None; Parsetree.pexp_loc = __loc__; Parsetree.pexp_loc_stack = []; Parsetree.pexp_attributes = []};
{Parsetree.pexp_desc = Parsetree.Pexp_poly x1 (Some x2); Parsetree.pexp_loc = __loc__; Parsetree.pexp_loc_stack = []; Parsetree.pexp_attributes = []};
{Parsetree.pexp_desc = Parsetree.Pexp_poly x1 ox2; Parsetree.pexp_loc = __loc__; Parsetree.pexp_loc_stack = []; Parsetree.pexp_attributes = []};
<:expression< object $patt:x$ $list:lx$  end >>;
<:expression< fun (type $lid:s$) -> $x2$ >> ;
<:expression< (module $me$) >> ;
<:expression< let open $overrideflag:x1$ $me$ in $x2$ >> ;
<:expression< $letop:x1$ $list:lx$ in $x2$ >>;
<:expression< [% $attrid:s$ $list:x$] >>;
<:expression< [% $attrid:s$ : $list:x$] >>;
<:expression< [% $attrid:s$ : $x$] >>;
<:expression< [% $attrid:s$ ? $x1$] >>;
<:expression< [% $attrid:s$ ? $x1$ when $x2$] >>;
<:expression< [% $attrid:s$ ? $x1$ $expropt:ox2$] >>;
{Parsetree.pexp_desc = Parsetree.Pexp_unreachable; Parsetree.pexp_loc = __loc__; Parsetree.pexp_loc_stack = []; Parsetree.pexp_attributes = []};
[@@@"ocaml.text" "expression_desc";];
[@@@"ocaml.text" "case";];
<:case< $x1$ -> $x3$ >> ;
<:case< $x1$ when $x2$ -> $x3$ >> ;
<:case< $x1$ $wheno:ox2$ -> $x3$ >> ;
[@@@"ocaml.text" "letop";];
[@@@"ocaml.text" "binding_op";];
<:binding_op< $lid:s$ $x2$ = $x3$ >> ;
[@@@"ocaml.text" "value_description";];
[@@@"ocaml.text" "type_declaration";];
<:type_decl< $list:lxxx$ $lid:s$ = private $constructorlist:lx$ $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $x4$ = private $constructorlist:lx$ $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $opt:ox4$ private $constructorlist:lx$ $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $constructorlist:lx$ $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $x4$ = $constructorlist:lx$ $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $opt:ox4$ $constructorlist:lx$ $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $priv:x3$ $constructorlist:lx$ $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $x4$ = $priv:x3$ $constructorlist:lx$ $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $opt:ox4$ $priv:x3$ $constructorlist:lx$ $list:lxxl$ >> ;

<:type_decl< $list:lxxx$ $lid:s$ = private { $list:lx$ } $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $x4$ = private { $list:lx$ } $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $opt:ox4$ private { $list:lx$ } $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = { $list:lx$ } $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $x4$ = { $list:lx$ } $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $opt:ox4$ { $list:lx$ } $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $priv:x3$ { $list:lx$ } $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $x4$ = $priv:x3$ { $list:lx$ } $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $opt:ox4$ $priv:x3$ { $list:lx$ } $list:lxxl$ >> ;

<:type_decl< $list:lxxx$ $lid:s$ = private .. $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $x4$ = private .. $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $opt:ox4$ private .. $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = .. $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $x4$ = .. $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $opt:ox4$ .. $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $priv:x3$ .. $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $x4$ = $priv:x3$ .. $list:lxxl$ >> ;
<:type_decl< $list:lxxx$ $lid:s$ = $opt:ox4$ $priv:x3$ .. $list:lxxl$ >> ;

(*
<:type_decl< $list:lxxx$ $lid:s$ = $constructorlist:l$ $list:lxxl$ >> ;
 *)
[@@@"ocaml.text" "type_kind";];
[@@@"ocaml.text" "label_declaration";];
<:label_declaration< $mutable:x2$ $lid:s$ : $x3$ >> ;
[@@@"ocaml.text" "constructor_declaration";];
<:constructor_declaration< $uid:s$ : $list:lx$ . $list:lx$ $opt:None$ >>;
<:constructor_declaration< $uid:s$ : $list:lx$ . $list:lx$ -> $x3$ >>;
<:constructor_declaration< $uid:s$ : $list:lx$ . $list:lx$ $opt:ox3$ >>;
<:constructor_declaration< $uid:s$ : $list:lx$ . { $list:lx$} $opt:None$ >>;
<:constructor_declaration< $uid:s$ : $list:lx$ . { $list:lx$} -> $x3$ >>;
<:constructor_declaration< $uid:s$ : $list:lx$ . { $list:lx$} $opt:ox3$ >>;
[@@@"ocaml.text" "constructor_arguments";];
[@@@"ocaml.text" "type_extension";];
[@@@"ocaml.text" "extension_constructor";];
<:extension_constructor< $uid:s$ : $list:lx$ . $list:lx$ $opt:None$ >>;
<:extension_constructor< $uid:s$ : $list:lx$ . $list:lx$ -> $x2$ >>;
<:extension_constructor< $uid:s$ : $list:lx$ . $list:lx$ $opt:ox2$ >>;
<:extension_constructor< $uid:s$ : $list:lx$ . { $list:lx$} $opt:None$ >>;
<:extension_constructor< $uid:s$ : $list:lx$ . { $list:lx$} -> $x2$ >>;
<:extension_constructor< $uid:s$ : $list:lx$ . { $list:lx$} $opt:ox2$ >>;
<:extension_constructor< $uid:s$ = $longid:x$ >>;
[@@@"ocaml.text" "type_exception";];
[@@@"ocaml.text" "extension_constructor_kind";];
[@@@"ocaml.text" "class_type";];
<:class_type< $list:lx$ $lid:s$ >> ;
<:class_type< $list:lx$ $longid:x$ . $lid:s$ >> ;
<:class_type< object ($x$) $list:lx$ end >> ;
<:class_type< $label:x1$ $x2$ -> $ct$ >> ;
<:class_type< [% $attrid:s$ $list:x$] >>;
<:class_type< [% $attrid:s$ : $list:x$] >>;
<:class_type< [% $attrid:s$ : $x$] >>;
<:class_type< [% $attrid:s$ ? $x1$] >>;
<:class_type< [% $attrid:s$ ? $x1$ when $x2$] >>;
<:class_type< [% $attrid:s$ ? $x1$ $expropt:ox2$] >>;
[@@@"ocaml.text" "class_type_desc";];
[@@@"ocaml.text" "class_signature";];
[@@@"ocaml.text" "class_type_field";];
<:class_type_field< inherit $ct$ >>  ;
<:class_type_field< val $mutable:xxxxf2$ $virtual:xxxxf3$ $lid:x$ : $xxxxf4$ >>  ;
<:class_type_field< method $priv:xxxxf2$ $virtual:xxxxf3$ $lid:x$ : $xxxxf4$ >>  ;
<:class_type_field< constraint $xxf1$ = $xxf2$ >>  ;
<:class_type_field< [@@@ $attrid:s$ $list:x$] >>;
<:class_type_field< [@@@ $attrid:s$ : $list:x$] >>;
<:class_type_field< [@@@ $attrid:s$ : $x$] >>;
<:class_type_field< [@@@ $attrid:s$ ? $x1$] >>;
<:class_type_field< [@@@ $attrid:s$ ? $x1$ when $x2$] >>;
<:class_type_field< [@@@ $attrid:s$ ? $x1$ $expropt:ox2$] >>;
<:class_type_field< [%% $attrid:s$ $list:x$] >>;
<:class_type_field< [%% $attrid:s$ : $list:x$] >>;
<:class_type_field< [%% $attrid:s$ : $x$] >>;
<:class_type_field< [%% $attrid:s$ ? $x1$] >>;
<:class_type_field< [%% $attrid:s$ ? $x1$ when $x2$] >>;
<:class_type_field< [%% $attrid:s$ ? $x1$ $expropt:ox2$] >>;
[@@@"ocaml.text" "class_type_field_desc";];
[@@@"ocaml.text" "class_infos";];
[@@@"ocaml.text" "class_description";];
<:class_description< class $virtual:x1$ $list:lxxx$ $lid:s$ : $ct$ >> ;
[@@@"ocaml.text" "class_type_declaration";];
<:class_description< class $virtual:x1$ $list:lxxx$ $lid:s$ : $ct$ >> ;
[@@@"ocaml.text" "class_expr";];
<:class_expr< $list:lx$ $lid:s$ >> ;
<:class_expr< $list:lx$ $longid:x$ . $lid:s$ >> ;
<:class_expr< object $patt:x$ $list:lx$ end >> ;
<:class_expr< fun $label:x1$ ( $x3$ ) -> $ce$ >> ;
<:class_expr< fun $label:x1$ ( $x3$ = $x2$ ) -> $ce$ >> ;
<:class_expr< fun $label:x1$ ( $x3$ $expropt:ox2$ ) -> $ce$ >> ;
<:class_expr< $ce$ $list:lxx$ >> ;
<:class_expr< let $recflag:x$ $list:lx$ in $ce$ >> ;
<:class_expr< ( $ce$ : $ct$ ) >> ;
<:class_expr< [% $attrid:s$ $list:x$] >>;
<:class_expr< [% $attrid:s$ : $list:x$] >>;
<:class_expr< [% $attrid:s$ : $x$] >>;
<:class_expr< [% $attrid:s$ ? $x1$] >>;
<:class_expr< [% $attrid:s$ ? $x1$ when $x2$] >>;
<:class_expr< [% $attrid:s$ ? $x1$ $expropt:ox2$] >>;
<:class_expr< let open $overrideflag:x2$ $longid:x$ in $ce$ >>;
[@@@"ocaml.text" "class_expr_desc";];
[@@@"ocaml.text" "class_structure";];
[@@@"ocaml.text" "class_field";];
<:class_field< inherit $overrideflag:x1$ $ce$ >>  ;
<:class_field< inherit $overrideflag:x1$ $ce$ as $lid:s$ >>  ;
<:class_field< inherit $overrideflag:x1$ $ce$ $opt:ox2$ >>  ;
<:class_field< val $mutable:xxxf2$ $lid:x$ : $x$ >>  ;
<:class_field< val $overrideflag:x1$ $mutable:xxxf2$ $lid:x$ = $x2$ >>  ;
<:class_field< method $priv:xxxf2$ $lid:x$ : $x$ >>  ;
<:class_field< method $overrideflag:x1$ $priv:xxxf2$ $lid:x$ = $x2$ >>  ;
<:class_field< constraint $xxf1$ = $xxf2$ >>  ;
<:class_field< initializer $x$ >>  ;
<:class_field< [@@@ $attrid:s$ $list:x$] >>;
<:class_field< [@@@ $attrid:s$ : $list:x$] >>;
<:class_field< [@@@ $attrid:s$ : $x$] >>;
<:class_field< [@@@ $attrid:s$ ? $x1$] >>;
<:class_field< [@@@ $attrid:s$ ? $x1$ when $x2$] >>;
<:class_field< [@@@ $attrid:s$ ? $x1$ $expropt:ox2$] >>;
<:class_field< [%% $attrid:s$ $list:x$] >>;
<:class_field< [%% $attrid:s$ : $list:x$] >>;
<:class_field< [%% $attrid:s$ : $x$] >>;
<:class_field< [%% $attrid:s$ ? $x1$] >>;
<:class_field< [%% $attrid:s$ ? $x1$ when $x2$] >>;
<:class_field< [%% $attrid:s$ ? $x1$ $expropt:ox2$] >>;
[@@@"ocaml.text" "class_field_desc";];
[@@@"ocaml.text" "class_field_kind";];
[@@@"ocaml.text" "class_declaration";];
[@@@"ocaml.text" "module_type";];
<:module_type< $longid:x$ >>;
<:module_type< sig $list:x$ end >>;
<:module_type< functor $opt:x$ -> $mt$ >>;
<:module_type< $mt$ with $list:lx$ >>;
<:module_type< module type of $me$ >>;
<:module_type< [% $attrid:s$ $list:x$] >>;
<:module_type< [% $attrid:s$ : $list:x$] >>;
<:module_type< [% $attrid:s$ : $x$] >>;
<:module_type< [% $attrid:s$ ? $x1$] >>;
<:module_type< [% $attrid:s$ ? $x1$ when $x2$] >>;
<:module_type< [% $attrid:s$ ? $x1$ $expropt:ox2$] >>;
<:module_type< (module $longid:x$) >>;
[@@@"ocaml.text" "module_type_desc";];
[@@@"ocaml.text" "functor_parameter";];
<:functor_parameter< () >> ;
<:functor_parameter< ( _ : $mt$ ) >> ;
<:functor_parameter< ( $uid:s$ : $mt$ ) >> ;
<:functor_parameter< ( $opt:os$ : $mt$ ) >> ;
[@@@"ocaml.text" "signature";];
[@@@"ocaml.text" "signature_item";];
<:signature_item< external $lid:s$ : $x2$ = $list:ls$ >> ;
<:signature_item< val $lid:s$ : $x2$ >> ;
<:signature_item< type $nonrecflag:x$ $list:lx$ >> ;
<:signature_item< typesubst $list:lx$ >> ;
<:signature_item< type $list:lxxx$ $lid:s$ += $priv:x2$ $list:lx$ >> ;
<:signature_item< type $list:lxxx$ $longid:x$ . $lid:s$ += $priv:x2$ $list:lx$ >> ;
<:signature_item< module _ : $mt$ >> ;
<:signature_item< module $uid:s$ : $mt$ >> ;
<:signature_item< module $opt:os$ : $mt$ >> ;
<:signature_item< module $uid:s$ := $longid:x$ >> ;
<:signature_item< module rec $list:lx$ >> ;
<:signature_item< module type $uid:s$ >> ;
<:signature_item< module type $uid:s$ = $mt$ >> ;
<:signature_item< module type $uid:s$ $opt:omt$ >> ;
<:signature_item< module type $uid:s$ := $mt$ >> ;
<:signature_item< open $overrideflag:x2$ $longid:x$ >> ;
<:signature_item< include $mt$ >> ;
<:signature_item< class $classdesclist:lx$ >> ;
<:signature_item< class type $classtypelist:lx$ >> ;
<:signature_item< [@@@ $attrid:s$ $list:x$] >>;
<:signature_item< [@@@ $attrid:s$ : $list:x$] >>;
<:signature_item< [@@@ $attrid:s$ : $x$] >>;
<:signature_item< [@@@ $attrid:s$ ? $x1$] >>;
<:signature_item< [@@@ $attrid:s$ ? $x1$ when $x2$] >>;
<:signature_item< [@@@ $attrid:s$ ? $x1$ $expropt:ox2$] >>;
<:signature_item< [%% $attrid:s$ $list:x$] >>;
<:signature_item< [%% $attrid:s$ : $list:x$] >>;
<:signature_item< [%% $attrid:s$ : $x$] >>;
<:signature_item< [%% $attrid:s$ ? $x1$] >>;
<:signature_item< [%% $attrid:s$ ? $x1$ when $x2$] >>;
<:signature_item< [%% $attrid:s$ ? $x1$ $expropt:ox2$] >>;
[@@@"ocaml.text" "signature_item_desc";];
[@@@"ocaml.text" "module_declaration";];
<:module_declaration< module _ : $mt$ >> ;
<:module_declaration< module $uid:s$ : $mt$ >> ;
<:module_declaration< module $opt:os$ : $mt$ >> ;
[@@@"ocaml.text" "module_substitution";];
[@@@"ocaml.text" "module_type_declaration";];
[@@@"ocaml.text" "open_infos";];
[@@@"ocaml.text" "open_description";];
[@@@"ocaml.text" "open_declaration";];
[@@@"ocaml.text" "include_infos";];
[@@@"ocaml.text" "include_description";];
[@@@"ocaml.text" "include_declaration";];
[@@@"ocaml.text" "with_constraint";];
<:with_constraint< type $lid:s$ = $typedecl:x2$ >> ;
<:with_constraint< type $longid:x$ . $lid:s$ = $typedecl:x2$ >> ;
<:with_constraint< module $longid:x$ = $longid:x$ >> ;
<:with_constraint< module type $longid:x$ = $mt$ >> ;
<:with_constraint< module type $longid:x$ := $mt$ >> ;
<:with_constraint< type $lid:s$ := $typedecl:x2$ >> ;
<:with_constraint< module $longid:x$ := $longid:x$ >> ;
[@@@"ocaml.text" "module_expr";];
<:module_expr< $longid:x$ >>;
<:module_expr< struct $list:x$ end >>;
<:module_expr< functor $opt:x$ -> $me$ >>;
<:module_expr< $me1$( $me2$ ) >>;
<:module_expr< ( $me$ : $mt$ ) >>;
<:module_expr< ( val $x$ ) >>;
<:module_expr< [% $attrid:s$ $list:x$] >>;
<:module_expr< [% $attrid:s$ : $list:x$] >>;
<:module_expr< [% $attrid:s$ : $x$] >>;
<:module_expr< [% $attrid:s$ ? $x1$] >>;
<:module_expr< [% $attrid:s$ ? $x1$ when $x2$] >>;
<:module_expr< [% $attrid:s$ ? $x1$ $expropt:ox2$] >>;
[@@@"ocaml.text" "module_expr_desc";];
[@@@"ocaml.text" "structure";];
[@@@"ocaml.text" "structure_item";];
<:structure_item< let $recflag:x$ $list:lx$ >>;
<:structure_item< external $lid:s$ : $x2$ = $list:ls$ >>;
<:structure_item< type $nonrecflag:x$ $list:lx$ >> ;
<:structure_item< type $list:lxxx$ $lid:s$ += $priv:x2$ $list:lx$ >> ;
<:structure_item< type $list:lxxx$ $longid:x$ . $lid:s$ += $priv:x2$ $list:lx$ >> ;
<:structure_item< module _ = $me$ >> ;
<:structure_item< module $uid:s$ = $me$ >> ;
<:structure_item< module $opt:os$ = $me$ >> ;
<:structure_item< module rec $list:lx$ >> ;
<:structure_item< module type $uid:s$ >> ;
<:structure_item< module type $uid:s$ = $mt$ >> ;
<:structure_item< module type $uid:s$ $opt:omt$ >> ;
<:structure_item< open $overrideflag:x1$ $me$ >> ;
<:structure_item< class $classlist:lx$ >> ;
<:structure_item< class type $classtypelist:lx$ >> ;
<:structure_item< include $me$ >> ;
<:structure_item< [@@@ $attrid:s$ $list:x$] >>;
<:structure_item< [@@@ $attrid:s$ : $list:x$] >>;
<:structure_item< [@@@ $attrid:s$ : $x$] >>;
<:structure_item< [@@@ $attrid:s$ ? $x1$] >>;
<:structure_item< [@@@ $attrid:s$ ? $x1$ when $x2$] >>;
<:structure_item< [@@@ $attrid:s$ ? $x1$ $expropt:ox2$] >>;
<:structure_item< [%% $attrid:s$ $list:x$] >>;
<:structure_item< [%% $attrid:s$ : $list:x$] >>;
<:structure_item< [%% $attrid:s$ : $x$] >>;
<:structure_item< [%% $attrid:s$ ? $x1$] >>;
<:structure_item< [%% $attrid:s$ ? $x1$ when $x2$] >>;
<:structure_item< [%% $attrid:s$ ? $x1$ $expropt:ox2$] >>;
[@@@"ocaml.text" "structure_item_desc";];
[@@@"ocaml.text" "value_binding";];
<:value_binding< $x1$ = $x2$ >> ;
[@@@"ocaml.text" "module_binding";];
[@@@"ocaml.text" "position";];
[@@@"ocaml.text" "location";];
[@@@"ocaml.text" "located";];
[@@@"ocaml.text" "str_vala";];
[@@@"ocaml.text" "longident_t";];
[@@@"ocaml.text" "ast_constant";];
[@@@"ocaml.text" "arg_label";];
<:arg_label< >> ;
<:arg_label< $lid:s$ : >> ;
<:arg_label< ? $lid:s$ : >> ;
[@@@"ocaml.text" "label";];
[@@@"ocaml.text" "closed_flag";];
[@@@"ocaml.text" "rec_flag";];
[@@@"ocaml.text" "direction_flag";];
[@@@"ocaml.text" "private_flag";];
[@@@"ocaml.text" "mutable_flag";];
[@@@"ocaml.text" "virtual_flag";];
[@@@"ocaml.text" "override_flag";];
[@@@"ocaml.text" "variance";];
[@@@"ocaml.text" "injectivity";];
[@@@"ocaml.text" "constant";];
<:constant< $int:s$ >> ;
<:constant< $int32:s$ >> ;
<:constant< $int64:s$ >> ;
<:constant< $nativeint:s$ >> ;
<:constant< $char:x$ >> ;
<:constant< $string:s1$ >> ;
<:constant< $string:s1$ $delim:s2$ >> ;
Parsetree.Pconst_string s1 __loc__ os2;
<:constant< $float:sxf1$ >> ;
[@@@"ocaml.text" "location_stack";];
[@@@"ocaml.text" "toplevel_phrase";];
[@@@"ocaml.text" "toplevel_directive";];
[@@@"ocaml.text" "directive_argument";];
[@@@"ocaml.text" "directive_argument_desc";];
[@@@"ocaml.text" "functor_parameter_vala";];
