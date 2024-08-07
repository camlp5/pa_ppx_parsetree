(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Errors *)

exception Fatal_error

let fatal_errorf fmt =
  Format.kfprintf
    (fun _ -> raise Fatal_error)
    Format.err_formatter
    ("@?>> Fatal error: " ^^ fmt ^^ "@.")

let fatal_error msg = fatal_errorf "%s" msg

(* Exceptions *)

let try_finally ?(always=(fun () -> ())) ?(exceptionally=(fun () -> ())) work =
  match work () with
    | result ->
      begin match always () with
        | () -> result
        | exception always_exn ->
          let always_bt = Printexc.get_raw_backtrace () in
          exceptionally ();
          Printexc.raise_with_backtrace always_exn always_bt
      end
    | exception work_exn ->
      let work_bt = Printexc.get_raw_backtrace () in
      begin match always () with
        | () ->
          exceptionally ();
          Printexc.raise_with_backtrace work_exn work_bt
        | exception always_exn ->
          let always_bt = Printexc.get_raw_backtrace () in
          exceptionally ();
          Printexc.raise_with_backtrace always_exn always_bt
      end

let reraise_preserving_backtrace e f =
  let bt = Printexc.get_raw_backtrace () in
  f ();
  Printexc.raise_with_backtrace e bt

type ref_and_value = R : 'a ref * 'a -> ref_and_value

let protect_refs =
  let set_refs l = List.iter (fun (R (r, v)) -> r := v) l in
  fun refs f ->
    let backup = List.map (fun (R (r, _)) -> R (r, !r)) refs in
    set_refs refs;
    Fun.protect ~finally:(fun () -> set_refs backup) f

(* List functions *)

let rec map_end f l1 l2 =
  match l1 with
    [] -> l2
  | hd::tl -> f hd :: map_end f tl l2

let rev_map_end f l1 l2 =
  let rec rmap_f accu = function
    | [] -> accu
    | hd::tl -> rmap_f (f hd :: accu) tl
  in
  rmap_f l2 l1

let rec map_left_right f = function
    [] -> []
  | hd::tl -> let res = f hd in res :: map_left_right f tl

let rec for_all2 pred l1 l2 =
  match (l1, l2) with
    ([], []) -> true
  | (hd1::tl1, hd2::tl2) -> pred hd1 hd2 && for_all2 pred tl1 tl2
  | (_, _) -> false

let rec replicate_list elem n =
  if n <= 0 then [] else elem :: replicate_list elem (n-1)

let rec list_remove x = function
    [] -> []
  | hd :: tl ->
      if hd = x then tl else hd :: list_remove x tl

let rec split_last = function
    [] -> assert false
  | [x] -> ([], x)
  | hd :: tl ->
      let (lst, last) = split_last tl in
      (hd :: lst, last)

module Stdlib = struct
  module List = struct
    type 'a t = 'a list

    let rec compare cmp l1 l2 =
      match l1, l2 with
      | [], [] -> 0
      | [], _::_ -> -1
      | _::_, [] -> 1
      | h1::t1, h2::t2 ->
        let c = cmp h1 h2 in
        if c <> 0 then c
        else compare cmp t1 t2

    let rec equal eq l1 l2 =
      match l1, l2 with
      | ([], []) -> true
      | (hd1 :: tl1, hd2 :: tl2) -> eq hd1 hd2 && equal eq tl1 tl2
      | (_, _) -> false

    let map2_prefix f l1 l2 =
      let rec aux acc l1 l2 =
        match l1, l2 with
        | [], _ -> (List.rev acc, l2)
        | _ :: _, [] -> raise (Invalid_argument "map2_prefix")
        | h1::t1, h2::t2 ->
          let h = f h1 h2 in
          aux (h :: acc) t1 t2
      in
      aux [] l1 l2

    let rec iteri2 i f l1 l2 =
      match (l1, l2) with
        ([], []) -> ()
      | (a1::l1, a2::l2) -> f i a1 a2; iteri2 (i + 1) f l1 l2
      | (_, _) -> raise (Invalid_argument "iteri2")

    let iteri2 f l1 l2 = iteri2 0 f l1 l2

    let some_if_all_elements_are_some l =
      let rec aux acc l =
        match l with
        | [] -> Some (List.rev acc)
        | None :: _ -> None
        | Some h :: t -> aux (h :: acc) t
      in
      aux [] l

    let split_at n l =
      let rec aux n acc l =
        if n = 0
        then List.rev acc, l
        else
          match l with
          | [] -> raise (Invalid_argument "split_at")
          | t::q -> aux (n-1) (t::acc) q
      in
      aux n [] l

    let chunks_of n l =
      if n <= 0 then raise (Invalid_argument "chunks_of");
      (* Invariant: List.length l = remaining *)
      let rec aux n acc l ~remaining =
        match remaining with
        | 0 -> List.rev acc
        | _ when remaining <= n -> List.rev (l :: acc)
        | _ ->
          let chunk, rest = split_at n l in
          aux n (chunk :: acc) rest ~remaining:(remaining - n)
      in
      aux n [] l ~remaining:(List.length l)

    let rec is_prefix ~equal t ~of_ =
      match t, of_ with
      | [], [] -> true
      | _::_, [] -> false
      | [], _::_ -> true
      | x1::t, x2::of_ -> equal x1 x2 && is_prefix ~equal t ~of_

    type 'a longest_common_prefix_result = {
      longest_common_prefix : 'a list;
      first_without_longest_common_prefix : 'a list;
      second_without_longest_common_prefix : 'a list;
    }

    let find_and_chop_longest_common_prefix ~equal ~first ~second =
      let rec find_prefix ~longest_common_prefix_rev l1 l2 =
        match l1, l2 with
        | elt1 :: l1, elt2 :: l2 when equal elt1 elt2 ->
          let longest_common_prefix_rev = elt1 :: longest_common_prefix_rev in
          find_prefix ~longest_common_prefix_rev l1 l2
        | l1, l2 ->
          { longest_common_prefix = List.rev longest_common_prefix_rev;
            first_without_longest_common_prefix = l1;
            second_without_longest_common_prefix = l2;
          }
      in
      find_prefix ~longest_common_prefix_rev:[] first second
  end

  module Option = struct
    type 'a t = 'a option

    let print print_contents ppf t =
      match t with
      | None -> Format.pp_print_string ppf "None"
      | Some contents ->
        Format.fprintf ppf "@[(Some@ %a)@]" print_contents contents
  end

  module Array = struct
    let exists2 p a1 a2 =
      let n = Array.length a1 in
      if Array.length a2 <> n then invalid_arg "Misc.Stdlib.Array.exists2";
      let rec loop i =
        if i = n then false
        else if p (Array.unsafe_get a1 i) (Array.unsafe_get a2 i) then true
        else loop (succ i) in
      loop 0

    let for_alli p a =
      let n = Array.length a in
      let rec loop i =
        if i = n then true
        else if p i (Array.unsafe_get a i) then loop (succ i)
        else false in
      loop 0

    let all_somes a =
      try
        Some (Array.map (function None -> raise_notrace Exit | Some x -> x) a)
      with
      | Exit -> None
  end

  module String = struct
    include String
    module Set = Set.Make(String)
    module Map = Map.Make(String)
    module Tbl = Hashtbl.Make(struct
      include String
      let hash = Hashtbl.hash
    end)

    let for_all f t =
      let len = String.length t in
      let rec loop i =
        i = len || (f t.[i] && loop (i + 1))
      in
      loop 0

    let print ppf t =
      Format.pp_print_string ppf t
  end

  external compare : 'a -> 'a -> int = "%compare"
end

(* File functions *)

let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
      [] -> raise Not_found
    | dir::rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end

let find_in_path_rel path name =
  let rec simplify s =
    let open Filename in
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then simplify dir
    else concat (simplify dir) base
  in
  let rec try_dir = function
    [] -> raise Not_found
  | dir::rem ->
      let fullname = simplify (Filename.concat dir name) in
      if Sys.file_exists fullname then fullname else try_dir rem
  in try_dir path

let normalized_unit_filename = String.uncapitalize_ascii

let find_in_path_normalized path name =
  let uname = normalized_unit_filename name in
  let rec try_dir = function
    [] -> raise Not_found
  | dir::rem ->
      let fullname = Filename.concat dir name
      and ufullname = Filename.concat dir uname in
      if Sys.file_exists ufullname then ufullname
      else if Sys.file_exists fullname then fullname
      else try_dir rem
  in try_dir path

let remove_file filename =
  try
   if Sys.file_exists filename
    then Sys.remove filename
  with Sys_error _msg ->
    ()

(* Expand a -I option: if it starts with +, make it relative to the standard
   library directory *)

let expand_directory alt s =
  if String.length s > 0 && s.[0] = '+'
  then Filename.concat alt
                       (String.sub s 1 (String.length s - 1))
  else s

let path_separator =
  match Sys.os_type with
  | "Win32" -> ';'
  | _ -> ':'

let split_path_contents ?(sep = path_separator) = function
  | "" -> []
  | s -> String.split_on_char sep s

(* Hashtable functions *)

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

(* File copy *)

let copy_file ic oc =
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let string_of_file ic =
  let b = Buffer.create 0x10000 in
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then Buffer.contents b else
      (Buffer.add_subbytes b buff 0 n; copy())
  in copy()

let output_to_file_via_temporary ?(mode = [Open_text]) filename fn =
  let (temp_filename, oc) =
    Filename.open_temp_file
       ~mode ~perms:0o666 ~temp_dir:(Filename.dirname filename)
       (Filename.basename filename) ".tmp" in
    (* The 0o666 permissions will be modified by the umask.  It's just
       like what [open_out] and [open_out_bin] do.
       With temp_dir = dirname filename, we ensure that the returned
       temp file is in the same directory as filename itself, making
       it safe to rename temp_filename to filename later.
       With prefix = basename filename, we are almost certain that
       the first generated name will be unique.  A fixed prefix
       would work too but might generate more collisions if many
       files are being produced simultaneously in the same directory. *)
  match fn temp_filename oc with
  | res ->
      close_out oc;
      begin try
        Sys.rename temp_filename filename; res
      with exn ->
        remove_file temp_filename; raise exn
      end
  | exception exn ->
      close_out oc; remove_file temp_filename; raise exn

let protect_writing_to_file ~filename ~f =
  let outchan = open_out_bin filename in
  try_finally ~always:(fun () -> close_out outchan)
    ~exceptionally:(fun () -> remove_file filename)
    (fun () -> f outchan)

(* Integer operations *)

let rec log2 n =
  if n <= 1 then 0 else 1 + log2(n asr 1)

let align n a =
  if n >= 0 then (n + a - 1) land (-a) else n land (-a)

let no_overflow_add a b = (a lxor b) lor (a lxor (lnot (a+b))) < 0

let no_overflow_sub a b = (a lxor (lnot b)) lor (b lxor (a-b)) < 0

(* Taken from Hacker's Delight, chapter "Overflow Detection" *)
let no_overflow_mul a b =
  not ((a = min_int && b < 0) || (b <> 0 && (a * b) / b <> a))

let no_overflow_lsl a k =
  0 <= k && k < Sys.word_size - 1 && min_int asr k <= a && a <= max_int asr k

let letter_of_int n =
  let letter = String.make 1 (Char.chr (Char.code 'a' + n mod 26)) in
  let num = n / 26 in
  if num = 0 then letter
  else letter ^ Int.to_string num

module Int_literal_converter = struct
  (* To convert integer literals, allowing max_int + 1 (PR#4210) *)
  let cvt_int_aux str neg of_string =
    if String.length str = 0 || str.[0]= '-'
    then of_string str
    else neg (of_string ("-" ^ str))
  let int s = cvt_int_aux s (~-) int_of_string
  let int32 s = cvt_int_aux s Int32.neg Int32.of_string
  let int64 s = cvt_int_aux s Int64.neg Int64.of_string
  let nativeint s = cvt_int_aux s Nativeint.neg Nativeint.of_string
end

(* [find_first_mono p] assumes that there exists a natural number
   N such that [p] is false on [0; N[ and true on [N; max_int], and
   returns this N. (See misc.mli for the detailed specification.) *)
let find_first_mono =
  let rec find p ~low ~jump ~high =
    (* Invariants:
       [low, jump, high] are non-negative with [low < high],
       [p low = false],
       [p high = true]. *)
    if low + 1 = high then high
    (* ensure that [low + jump] is in ]low; high[ *)
    else if jump < 1 then find p ~low ~jump:1 ~high
    else if jump >= high - low then find p ~low ~jump:((high - low) / 2) ~high
    else if p (low + jump) then
      (* We jumped too high: continue with a smaller jump and lower limit *)
      find p ~low:low ~jump:(jump / 2) ~high:(low + jump)
    else
      (* we jumped too low:
         continue from [low + jump] with a larger jump *)
      let next_jump = max jump (2 * jump) (* avoid overflows *) in
      find p ~low:(low + jump) ~jump:next_jump ~high
  in
  fun p ->
    if p 0 then 0
    else find p ~low:0 ~jump:1 ~high:max_int

(* String operations *)

let split_null_terminated s =
  let[@tail_mod_cons] rec discard_last_sep = function
    | [] | [""] -> []
    | x :: xs -> x :: discard_last_sep xs
  in
  discard_last_sep (String.split_on_char '\000' s)

let concat_null_terminated = function
  | [] -> ""
  | l -> String.concat "\000" (l @ [""])

let chop_extensions file =
  let dirname = Filename.dirname file and basename = Filename.basename file in
  try
    let pos = String.index basename '.' in
    let basename = String.sub basename 0 pos in
    if Filename.is_implicit file && dirname = Filename.current_dir_name then
      basename
    else
      Filename.concat dirname basename
  with Not_found -> file

let search_substring pat str start =
  let rec search i j =
    if j >= String.length pat then i
    else if i + j >= String.length str then raise Not_found
    else if str.[i + j] = pat.[j] then search i (j+1)
    else search (i+1) 0
  in search start 0

let replace_substring ~before ~after str =
  let rec search acc curr =
    match search_substring before str curr with
      | next ->
         let prefix = String.sub str curr (next - curr) in
         search (prefix :: acc) (next + String.length before)
      | exception Not_found ->
        let suffix = String.sub str curr (String.length str - curr) in
        List.rev (suffix :: acc)
  in String.concat after (search [] 0)

let rev_split_words s =
  let rec split1 res i =
    if i >= String.length s then res else begin
      match s.[i] with
        ' ' | '\t' | '\r' | '\n' -> split1 res (i+1)
      | _ -> split2 res i (i+1)
    end
  and split2 res i j =
    if j >= String.length s then String.sub s i (j-i) :: res else begin
      match s.[j] with
        ' ' | '\t' | '\r' | '\n' -> split1 (String.sub s i (j-i) :: res) (j+1)
      | _ -> split2 res i (j+1)
    end
  in split1 [] 0

let get_ref r =
  let v = !r in
  r := []; v

let set_or_ignore f opt x =
  match f x with
  | None -> ()
  | Some y -> opt := Some y

let fst3 (x, _, _) = x
let snd3 (_,x,_) = x
let thd3 (_,_,x) = x

let fst4 (x, _, _, _) = x
let snd4 (_,x,_, _) = x
let thd4 (_,_,x,_) = x
let for4 (_,_,_,x) = x


let cut_at s c =
  let pos = String.index s c in
  String.sub s 0 pos, String.sub s (pos+1) (String.length s - pos - 1)

let ordinal_suffix n =
  let teen = (n mod 100)/10 = 1 in
  match n mod 10 with
  | 1 when not teen -> "st"
  | 2 when not teen -> "nd"
  | 3 when not teen -> "rd"
  | _ -> "th"

(* Color support handling *)
module Color = struct
  external isatty : out_channel -> bool = "caml_sys_isatty"

  (* reasonable heuristic on whether colors should be enabled *)
  let should_enable_color () =
    let term = try Sys.getenv "TERM" with Not_found -> "" in
    term <> "dumb"
    && term <> ""
    && isatty stderr

  type setting = Auto | Always | Never

  let default_setting = Auto
  let enabled = ref true

end

(* Terminal styling handling *)
module Style = struct
  (* use ANSI color codes, see https://en.wikipedia.org/wiki/ANSI_escape_code *)
  type color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

  type style =
    | FG of color (* foreground *)
    | BG of color (* background *)
    | Bold
    | Reset

  let ansi_of_color = function
    | Black -> "0"
    | Red -> "1"
    | Green -> "2"
    | Yellow -> "3"
    | Blue -> "4"
    | Magenta -> "5"
    | Cyan -> "6"
    | White -> "7"

  let code_of_style = function
    | FG c -> "3" ^ ansi_of_color c
    | BG c -> "4" ^ ansi_of_color c
    | Bold -> "1"
    | Reset -> "0"

  let ansi_of_style_l l =
    let s = match l with
      | [] -> code_of_style Reset
      | [s] -> code_of_style s
      | _ -> String.concat ";" (List.map code_of_style l)
    in
    "\x1b[" ^ s ^ "m"


  type Format.stag += Style of style list

  type tag_style ={
    ansi: style list;
    text_open:string;
    text_close:string
  }

  type styles = {
    error: tag_style;
    warning: tag_style;
    loc: tag_style;
    hint: tag_style;
    inline_code: tag_style;
  }

  let no_markup stl = { ansi = stl; text_close = ""; text_open = "" }

  let default_styles = {
      warning = no_markup [Bold; FG Magenta];
      error = no_markup [Bold; FG Red];
      loc = no_markup [Bold];
      hint = no_markup [Bold; FG Blue];
      inline_code= { ansi=[Bold]; text_open = {|"|}; text_close = {|"|} }
    }

  let cur_styles = ref default_styles
  let get_styles () = !cur_styles
  let set_styles s = cur_styles := s

  (* map a tag to a style, if the tag is known.
     @raise Not_found otherwise *)
  let style_of_tag s = match s with
    | Format.String_tag "error" ->  (!cur_styles).error
    | Format.String_tag "warning" ->(!cur_styles).warning
    | Format.String_tag "loc" -> (!cur_styles).loc
    | Format.String_tag "hint" -> (!cur_styles).hint
    | Format.String_tag "inline_code" -> (!cur_styles).inline_code
    | Style s -> no_markup s
    | _ -> raise Not_found


  let as_inline_code printer ppf x =
    Format.pp_open_stag ppf (Format.String_tag "inline_code");
    printer ppf x;
    Format.pp_close_stag ppf ()

  let inline_code ppf s = as_inline_code Format.pp_print_string ppf s

  (* either prints the tag of [s] or delegates to [or_else] *)
  let mark_open_tag ~or_else s =
    try
      let style = style_of_tag s in
      if !Color.enabled then ansi_of_style_l style.ansi else style.text_open
    with Not_found -> or_else s

  let mark_close_tag ~or_else s =
    try
      let style = style_of_tag s in
      if !Color.enabled then ansi_of_style_l [Reset] else style.text_close
    with Not_found -> or_else s

  (* add tag handling to formatter [ppf] *)
  let set_tag_handling ppf =
    let open Format in
    let functions = pp_get_formatter_stag_functions ppf () in
    let functions' = {functions with
      mark_open_stag=(mark_open_tag ~or_else:functions.mark_open_stag);
      mark_close_stag=(mark_close_tag ~or_else:functions.mark_close_stag);
    } in
    pp_set_mark_tags ppf true; (* enable tags *)
    pp_set_formatter_stag_functions ppf functions';
    ()

  let setup =
    let first = ref true in (* initialize only once *)
    let formatter_l =
      [Format.std_formatter; Format.err_formatter; Format.str_formatter]
    in
    let enable_color = function
      | Color.Auto -> Color.should_enable_color ()
      | Color.Always -> true
      | Color.Never -> false
    in
    fun o ->
      if !first then (
        first := false;
        Format.set_mark_tags true;
        List.iter set_tag_handling formatter_l;
        Color.enabled := (match o with
          | Some s -> enable_color s
          | None -> enable_color Color.default_setting)
      );
      ()
end

let did_you_mean ppf get_choices =
  (* flush now to get the error report early, in the (unheard of) case
     where the search in the get_choices function would take a bit of
     time; in the worst case, the user has seen the error, she can
     interrupt the process before the spell-checking terminates. *)
  Format.fprintf ppf "@?";
  match get_choices () with
  | [] -> ()
  | choices ->
    let rest, last = split_last choices in
    let comma ppf () = Format.fprintf ppf ", " in
     Format.fprintf ppf "@\n@{<hint>Hint@}: Did you mean %a%s%a?@?"
       (Format.pp_print_list ~pp_sep:comma Style.inline_code) rest
       (if rest = [] then "" else " or ")
       Style.inline_code last

module Error_style = struct
  type setting =
    | Contextual
    | Short

  let default_setting = Contextual
end

let normalise_eol s =
  let b = Buffer.create 80 in
    for i = 0 to String.length s - 1 do
      if s.[i] <> '\r' then Buffer.add_char b s.[i]
    done;
    Buffer.contents b

let delete_eol_spaces src =
  let len_src = String.length src in
  let dst = Bytes.create len_src in
  let rec loop i_src i_dst =
    if i_src = len_src then
      i_dst
    else
      match src.[i_src] with
      | ' ' | '\t' ->
        loop_spaces 1 (i_src + 1) i_dst
      | c ->
        Bytes.set dst i_dst c;
        loop (i_src + 1) (i_dst + 1)
  and loop_spaces spaces i_src i_dst =
    if i_src = len_src then
      i_dst
    else
      match src.[i_src] with
      | ' ' | '\t' ->
        loop_spaces (spaces + 1) (i_src + 1) i_dst
      | '\n' ->
        Bytes.set dst i_dst '\n';
        loop (i_src + 1) (i_dst + 1)
      | _ ->
        for n = 0 to spaces do
          Bytes.set dst (i_dst + n) src.[i_src - spaces + n]
        done;
        loop (i_src + 1) (i_dst + spaces + 1)
  in
  let stop = loop 0 0 in
  Bytes.sub_string dst 0 stop

(* showing configuration and configuration variables *)
let show_config_and_exit () =
  Config.print_config stdout;
  exit 0

let show_config_variable_and_exit x =
  match Config.config_var x with
  | Some v ->
      (* we intentionally don't print a newline to avoid Windows \r
         issues: bash only strips the trailing \n when using a command
         substitution $(ocamlc -config-var foo), so a trailing \r would
         remain if printing a newline under Windows and scripts would
         have to use $(ocamlc -config-var foo | tr -d '\r')
         for portability. Ugh. *)
      print_string v;
      exit 0
  | None ->
      exit 2

let get_build_path_prefix_map =
  let init = ref false in
  let map_cache = ref None in
  fun () ->
    if not !init then begin
      init := true;
      match Sys.getenv "BUILD_PATH_PREFIX_MAP" with
      | exception Not_found -> ()
      | encoded_map ->
        match Build_path_prefix_map.decode_map encoded_map with
          | Error err ->
              fatal_errorf
                "Invalid value for the environment variable \
                 BUILD_PATH_PREFIX_MAP: %s" err
          | Ok map -> map_cache := Some map
    end;
    !map_cache

let debug_prefix_map_flags () =
  if not Config.as_has_debug_prefix_map then
    []
  else begin
    match get_build_path_prefix_map () with
    | None -> []
    | Some map ->
      List.fold_right
        (fun map_elem acc ->
           match map_elem with
           | None -> acc
           | Some { Build_path_prefix_map.target; source; } ->
             (Printf.sprintf "--debug-prefix-map %s=%s"
                (Filename.quote source)
                (Filename.quote target)) :: acc)
        map
        []
  end

let print_if ppf flag printer arg =
  if !flag then Format.fprintf ppf "%a@." printer arg;
  arg

let print_see_manual ppf manual_section =
  let open Format in
  fprintf ppf "(see manual section %a)"
    (pp_print_list ~pp_sep:(fun f () -> pp_print_char f '.') pp_print_int)
    manual_section


type filepath = string
type modname = string
type crcs = (modname * Digest.t option) list

type alerts = string Stdlib.String.Map.t

