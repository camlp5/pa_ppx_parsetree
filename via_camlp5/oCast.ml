(**pp $(MIGRATE_OCAMLCFLAGS) -ppopt -pa_import-redeclare -package camlp5.macro,pa_ppx.import,compiler-libs.common -syntax camlp5o *)

module Lexing = struct
[%%import: Lexing.position] [@@deriving show]
end
module Location = struct
[%%import: Location.t] [@@deriving show]
[%%import: 'a Location.loc] [@@deriving show]
end
module Longident = struct
[%%import: Longident.t] [@@deriving show]
end
module Asttypes = struct
[%%import: Asttypes.constant] [@@deriving show]
[%%import: Asttypes.arg_label] [@@deriving show]
[%%import: Asttypes.label] [@@deriving show]
[%%import: Asttypes.closed_flag] [@@deriving show]
[%%import: Asttypes.rec_flag] [@@deriving show]
[%%import: Asttypes.direction_flag] [@@deriving show]
[%%import: Asttypes.private_flag] [@@deriving show]
[%%import: Asttypes.mutable_flag] [@@deriving show]
[%%import: Asttypes.virtual_flag] [@@deriving show]
[%%import: Asttypes.override_flag] [@@deriving show]
[%%import: Asttypes.variance] [@@deriving show]
IFDEF OCAML_VERSION >= OCAML_4_12_0 THEN
[%%import: Asttypes.injectivity] [@@deriving show,eq]
END
end
module Parsetree = struct
[%%import: Parsetree.constant] [@@deriving show]
[%%import: Parsetree.location_stack] [@@deriving show]
[%%import: Parsetree.attribute
 [@with Asttypes.loc := Location.loc]
] [@@deriving show]
[%%import: Parsetree.toplevel_phrase
 [@with Asttypes.loc := Location.loc]
] [@@deriving show]
end

