
# Notes for developers

## How to add a new OCaml AST version

1. copy <ocaml-src-dir>/parsing to parsing.<ocaml-version>

E.g.

```
$ cp ~/Hack/Opam-2.1.2/GENERIC/5.2.0/.opam-switch/sources/ocaml-base-compiler.5.2.0/parsing parsing.5.2.0
```

2. copy to pattern_parsing.5.2.0:

```
utils/build_path_prefix_map.ml
utils/build_path_prefix_map.mli
utils/warnings.ml
utils/warnings.mli
utils/clflags.ml
utils/clflags.mli
utils/misc.ml
utils/misc.mli
```

3. from pattern_parsing.<previous-ocaml-version> copy to pattern_parsing.5.2.0:

```
mk_meta.ML
pattern_misc.ml
shims.ML
```

2. Apply differences between previous parsing.<ocaml-version. and adjusted-parsing.<ocaml-version>

E.g.
```
$ diff --recursive -Bwiu {,adjusted-}parsing.5.1.0/ > diffs.5.1.0
$ patch -p1 -d parsing.5.2.0 < diffs.5.1.0
```

3. resolve any failed patches

4. copy "Makefile" from adjusted-parsing.<previous-ocaml-version>

5. build `parser.ml`

```
$ make parser.ml
```

This might require compiling other files, e.g. `ast_helper.mli`

6. make depend

```
$ make depend
```

7. then build everything

```
$ make
```

You might encounter build errors, and have to track them down.
