
let ploc_of_location loc =
  let open Location in
  let open Lexing in
  Ploc.make_loc loc.loc_start.pos_fname loc.loc_start.pos_lnum loc.loc_start.pos_bol (loc.loc_start.pos_cnum, loc.loc_end.pos_cnum) ""

let make_antiquotation typestr loc payload =
  let open Location in
  let sp = loc.loc_start.pos_cnum in
  let ep = loc.loc_end.pos_cnum in
  Printf.sprintf "%d,%d:%s:%s" sp ep typestr payload

let match_antiquotation s =
  (let __re__ =
       Re.Perl.compile_pat ~opts:[] "^([0-9]+),([0-9]+):([^:]*):(.*)$"
     in
     fun __subj__ ->
       match
         Option.map
           (fun __g__ ->
              Re.Group.get __g__ 1, Re.Group.get __g__ 2,
              Re.Group.get __g__ 3, Re.Group.get __g__ 4)
           (Re.exec_opt __re__ __subj__)
       with
         exception Not_found -> None
       | rv -> rv)
      s

let unmk_antiquotation s =
  match match_antiquotation s with
    Some (sps, eps, typestr, payload) -> (sps, eps, typestr, payload)
  | None -> failwith Fmt.(str "cannot destructure antiquotation string <<%s>>" s)

let pp_antiquotation pps antis =
  let (_, _, ty, pay) = unmk_antiquotation antis in
  if ty = "" then
    Fmt.(pf pps "$%s$" pay)
  else
    Fmt.(pf pps "$%s:%s$" ty pay)
