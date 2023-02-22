(** $(MIGRATE_OCAMLCFLAGS) -package pa_ppx_q_ast,pa_ppx.import,compiler-libs.common -syntax camlp5o *)


module Lexing = struct
end
module Location = struct
end
module Longident = struct
end
module Asttypes = struct
end


[%%import: OCast.attribute
             [@add
                 [%%import: OCast.Lexing.position]
              type location_t = [%import: OCast.Location.t
                                            [@with Lexing.position := position]
                                ]
              type 'a loc_t = [%import: 'a OCast.Location.loc
                                          [@with t := location_t]
                              ]
              type longident_t = [%import: OCast.Longident.t
                                             [@with Lexing.position := position]
                                             [@with t := longident_t]
                                 ]
                                   [%%import: OCast.Asttypes.loc
                                                [@with Location.t := location_t]
                                   ]
                                   [%%import: OCast.Asttypes.arg_label]
                                   [%%import: OCast.Asttypes.label]
                                   [%%import: OCast.Asttypes.closed_flag]
                                   [%%import: OCast.Asttypes.rec_flag]
                                   [%%import: OCast.Asttypes.direction_flag]
                                   [%%import: OCast.Asttypes.private_flag]
                                   [%%import: OCast.Asttypes.mutable_flag]
                                   [%%import: OCast.Asttypes.virtual_flag]
                                   [%%import: OCast.Asttypes.override_flag]
                                   [%%import: OCast.Asttypes.variance]
                                   [%%import: OCast.Asttypes.injectivity]
                                   [%%import: OCast.constant
                                                [@with Location.t := location_t]
                                   ]
                                   [%%import: OCast.location_stack
                                                [@with Location.t := location_t]
                                   ]
             ]
 [@with Location.t := location_t]
 [@with Asttypes.loc := loc]
 [@with Longident.t := longident_t]
 [@with Asttypes.arg_label := arg_label]
 [@with Asttypes.label := label]
 [@with Asttypes.closed_flag := closed_flag]
 [@with Asttypes.rec_flag := rec_flag]
 [@with Asttypes.direction_flag := direction_flag]
 [@with Asttypes.private_flag := private_flag]
 [@with Asttypes.mutable_flag := mutable_flag]
 [@with Asttypes.virtual_flag := virtual_flag]
 [@with Asttypes.override_flag := override_flag]
 [@with Asttypes.variance := variance]
 [@with Asttypes.injectivity := injectivity]
]
