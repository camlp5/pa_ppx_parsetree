(** $(MIGRATE_OCAMLCFLAGS) -ppopt -pa_import-redeclare -package pa_ppx.import,compiler-libs.common -syntax camlp5o *)

module Lexing = struct
[%%import: Lexing.position]
end
module Location = struct
[%%import: Location.t]
[%%import: 'a Location.loc]
end
module Longident = struct
[%%import: Longident.t]
end
module Asttypes = struct
[%%import: Asttypes.loc]
[%%import: Asttypes.arg_label]
[%%import: Asttypes.label]
[%%import: Asttypes.closed_flag]
[%%import: Asttypes.rec_flag]
[%%import: Asttypes.direction_flag]
[%%import: Asttypes.private_flag]
[%%import: Asttypes.mutable_flag]
[%%import: Asttypes.virtual_flag]
[%%import: Asttypes.override_flag]
[%%import: Asttypes.variance]
[%%import: Asttypes.injectivity]
end
[%%import: Parsetree.constant]
[%%import: Parsetree.location_stack]
[%%import: Parsetree.attribute]
