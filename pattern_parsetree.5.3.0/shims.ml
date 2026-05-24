# 2 "shims.ML"
module Int = struct
  let max = max
  let min = min
  let to_string = string_of_int
end

module Config = struct
  include Config
  let with_cmm_invariants = false
end


# 16 "shims.ML"
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
  major = 
# 31 "shims.ML"
           4 
# 31 "shims.ML"
                     ;
  minor = 
# 32 "shims.ML"
           12 
# 32 "shims.ML"
                     ;
  patchlevel = 
# 33 "shims.ML"
                1 
# 33 "shims.ML"
                          ;
  extra = None
}
end
