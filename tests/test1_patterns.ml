(**pp -package pa_ppx_parsetree_via_parsetree -syntax camlp5r *)
<:expression< $lid:s$ >>;
<:expression< $longid:x$ . $lid:s$ >>;
<:expression< $int:s$ >> ;
<:expression< $int32:s$ >> ;
<:expression< $int64:s$ >> ;
<:expression< $nativeint:s$ >> ;
<:expression< $char:x$ >> ;
<:expression< $string:s1$ >> ;
<:expression< $string:s1$ $delim:s2$ >> ;
<:expression< $float:sxf1$ >> ;
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
<:expression< $x1$ . $lid:s$ <- $x3$ >>;
<:expression< $x1$ . $longid:x$ . $lid:s$ <- $x3$ >>;
<:expression< [| $list:lx$ |] >>;
