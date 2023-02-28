(** $(MIGRATE_OCAMLCFLAGS) -package pa_ppx_q_ast,pa_ppx.import,compiler-libs.common -syntax camlp5o *)

[%%import: Pattern_OCast.attribute
  [@add
      [%%import: Pattern_OCast.Lexing.position]
      type location = [%import: Pattern_OCast.Location.t
                                    [@with Lexing.position := position]
                        ]
      type 'a located = [%import: 'a Pattern_OCast.Location.loc
                                    [@with t := location]
                        ]
      type longident_t = [%import: Pattern_OCast.Longident.t
                                     [@with Lexing.position := position]
                                     [@with t := longident_t]
                         ]
      type ast_constant =
        [%import: Pattern_OCast.Asttypes.constant
                    [@with Location.t := location]
        ]
      [%%import: Pattern_OCast.Asttypes.arg_label]
      [%%import: Pattern_OCast.Asttypes.label]
      [%%import: Pattern_OCast.Asttypes.closed_flag]
      [%%import: Pattern_OCast.Asttypes.rec_flag]
      [%%import: Pattern_OCast.Asttypes.direction_flag]
      [%%import: Pattern_OCast.Asttypes.private_flag]
      [%%import: Pattern_OCast.Asttypes.mutable_flag]
      [%%import: Pattern_OCast.Asttypes.virtual_flag]
      [%%import: Pattern_OCast.Asttypes.override_flag]
      [%%import: Pattern_OCast.Asttypes.variance]
      [%%import: Pattern_OCast.Asttypes.injectivity]
      [%%import: Pattern_OCast.constant
                   [@with Location.t := location]
      ]
      [%%import: Pattern_OCast.location_stack
                   [@with Location.t := location]
      ]
      [%%import: Pattern_OCast.toplevel_phrase
                   [@with Location.t := location]
                   [@with Location.loc := located]
                   [@with Longident.t := longident_t]
            ]
  ]
 [@with Location.t := location]
 [@with Location.loc := located]
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
