(**pp -package camlp5.macro,pa_ppx.deriving_plugins.std,pa_ppx_q_ast,pa_ppx.import -syntax camlp5o *)

let _equal_location a b = true
let _show_location pps _ = Fmt.(pf pps "<loc>")

[%%import: Parsetree.attribute
  [@add
      [%%import: Lexing.position]

      type _location = Location.t =
        { loc_start : position; loc_end : position; loc_ghost : bool }
      type location = _location[@equal _equal_location][@printer _show_location]
    type 'a located = [%import: 'a Asttypes.loc
                                    [@with Location.t := location]
                        ]
      type longident_t = [%import: Longident.t
                                     [@with Lexing.position := position]
                                     [@with t := longident_t]
                         ]
      type ast_constant =
        [%import: Asttypes.constant
                    [@with Location.t := location]
        ]
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
#if OCAML_VERSION >= (4,12,0)
      [%%import: Asttypes.injectivity]
#endif
      [%%import: Parsetree.constant
                   [@with Location.t := location]
      ]
      [%%import: Parsetree.location_stack
                   [@with Location.t := location]
      ]
      [%%import: Parsetree.toplevel_phrase
                   [@with Location.t := location]
                   [@with Asttypes.loc := located]
                   [@with Longident.t := longident_t]
            ]
  ]
 [@with Location.t := location]
 [@with Asttypes.loc := located]
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
  [@@deriving show,eq]
