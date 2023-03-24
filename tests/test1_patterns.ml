(**pp -package pa_ppx_parsetree_via_parsetree -syntax camlp5r *)
<:expression< $lid:s$ >>;
<:expression< $longid:x$ . $lid:s$ >>;
<:expression< $int:s$ >> ;
<:expression< $int32:s$ >> ;
<:expression< $int64:s$ >> ;
<:expression< $nativeint:s$ >> ;
<:expression< $char:x$ >> ;
<:expression< let $recflag:x1$ $list:lx$ in $x2$ >>;
<:expression< function $list:lx$ >>;
<:expression< $x$ $list:lxx$ >>;
<:expression< match $x$ with $list:lx$ >>;
<:expression< try $x$ with $list:lx$ >>;
<:expression<  $tuplelist:lx$ >>;
<:expression< $longid:x$ >>;
<:expression< $longid:x$ $x2$ >>;
<:expression< $longid:x$ $expropt:ox2$ >>;
