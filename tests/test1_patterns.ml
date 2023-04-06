(**pp -package pa_ppx_parsetree_via_parsetree -syntax camlp5r *)
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
<:expression< let module $uidopt:os$ = $me$ in $x2$ >>;
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
