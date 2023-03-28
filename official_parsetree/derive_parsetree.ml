(**pp $(MIGRATE_OCAMLCFLAGS) -package camlp5.macro,pa_ppx.deriving_plugins.std,pa_ppx_q_ast,pa_ppx.import -syntax camlp5o *)
(* camlp5r *)
(* pp_parsetree.ml,v *)

module Lexing = struct
[%%import: Lexing.position] [@@deriving show,eq]
end
module Location = struct
[%%import: Location.t] [@@deriving show]
let equal a b = true
[%%import: 'a Location.loc] [@@deriving show,eq]
end
module Longident = struct
[%%import: Longident.t] [@@deriving show,eq]
end
module Asttypes = struct
[%%import: Asttypes.loc] [@@deriving show,eq]
[%%import: Asttypes.arg_label] [@@deriving show,eq]
[%%import: Asttypes.label] [@@deriving show,eq]
[%%import: Asttypes.closed_flag] [@@deriving show,eq]
[%%import: Asttypes.rec_flag] [@@deriving show,eq]
[%%import: Asttypes.direction_flag] [@@deriving show,eq]
[%%import: Asttypes.private_flag] [@@deriving show,eq]
[%%import: Asttypes.mutable_flag] [@@deriving show,eq]
[%%import: Asttypes.virtual_flag] [@@deriving show,eq]
[%%import: Asttypes.override_flag] [@@deriving show,eq]
[%%import: Asttypes.variance] [@@deriving show,eq]
IFDEF OCAML_VERSION >= OCAML_4_12_0 THEN
[%%import: Asttypes.injectivity] [@@deriving show,eq]
END
end
module Parsetree = struct
[%%import: Parsetree.constant] [@@deriving show,eq]
[%%import: Parsetree.location_stack] [@@deriving show,eq]
[%%import: Parsetree.attribute] [@@deriving show,eq]
end
