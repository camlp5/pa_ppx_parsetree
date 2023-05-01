#if (OCAML_MAJOR , OCAML_MINOR) = (4, 10) 
module Int = struct
  let max = max
  let min = min
  let to_string = string_of_int
end

module Sys = struct
  include Sys

type extra_prefix = Plus | Tilde

type extra_info = extra_prefix * string

type ocaml_release_info = {
  major : int;
  minor : int;
  patchlevel : int;
  extra : extra_info option
}

let ocaml_release = {
  major = 4;
  minor = 10;
  patchlevel = 2;
  extra = None
}
end

module Config = struct
  include Config
  let with_cmm_invariants = false
end
#endif

