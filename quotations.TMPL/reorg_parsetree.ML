(**pp -package pa_ppx_q_ast,pa_ppx.import -syntax camlp5o *)

[%%import: Pa_ppx_parsetree_pattern_parsetree_VERSION.Parsetree.attribute
  [@add
      [%%import: Lexing.position]
      type location = [%import: Pa_ppx_parsetree_pattern_parsetree_VERSION.Location.t
                                    [@with Lexing.position := position]
                        ]
      type 'a located = [%import: 'a Pa_ppx_parsetree_pattern_parsetree_VERSION.Asttypes.loc
                                    [@with Location.t := location]
                        ]
      type str_vala = string Ploc.vala located
      type longident_t = [%import: Pa_ppx_parsetree_pattern_parsetree_VERSION.Longident.t
                                     [@with Lexing.position := position]
                                     [@with t := longident_t]
                         ]
      type ast_constant =
        [%import: Pa_ppx_parsetree_pattern_parsetree_VERSION.Asttypes.constant
                    [@with Location.t := location]
        ]
      [%%import: Pa_ppx_parsetree_pattern_parsetree_VERSION.Asttypes.arg_label]
      [%%import: Pa_ppx_parsetree_pattern_parsetree_VERSION.Asttypes.label]
      [%%import: Pa_ppx_parsetree_pattern_parsetree_VERSION.Asttypes.closed_flag]
      [%%import: Pa_ppx_parsetree_pattern_parsetree_VERSION.Asttypes.rec_flag]
      [%%import: Pa_ppx_parsetree_pattern_parsetree_VERSION.Asttypes.direction_flag]
      [%%import: Pa_ppx_parsetree_pattern_parsetree_VERSION.Asttypes.private_flag]
      [%%import: Pa_ppx_parsetree_pattern_parsetree_VERSION.Asttypes.mutable_flag]
      [%%import: Pa_ppx_parsetree_pattern_parsetree_VERSION.Asttypes.virtual_flag]
      [%%import: Pa_ppx_parsetree_pattern_parsetree_VERSION.Asttypes.override_flag]
      [%%import: Pa_ppx_parsetree_pattern_parsetree_VERSION.Asttypes.variance]
#if VERS >= 412
      [%%import: Pa_ppx_parsetree_pattern_parsetree_VERSION.Asttypes.injectivity]
#endif
      [%%import: Pa_ppx_parsetree_pattern_parsetree_VERSION.Parsetree.constant
                   [@with Location.t := location]
      ]
      [%%import: Pa_ppx_parsetree_pattern_parsetree_VERSION.Parsetree.location_stack
                   [@with Location.t := location]
      ]
      [%%import: Pa_ppx_parsetree_pattern_parsetree_VERSION.Parsetree.toplevel_phrase
                   [@with Location.t := location]
                   [@with Asttypes.loc := located]
                   [@with Longident.t := longident_t]
            ]
      type functor_parameter_vala = functor_parameter Ploc.vala
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
