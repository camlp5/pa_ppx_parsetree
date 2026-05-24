

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
           13 
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
