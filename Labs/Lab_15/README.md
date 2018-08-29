Step One - Become familiar with the project layout
--------------------------------------------------

Copy the `Lab_15` directory from the public class repo to your personal repo.
It contains many files, but we will cover the most important ones here.
The `examples` directory contains several example programs written in `Src_lang`, as well as the numeric results they should produce.
The `src` directory contains the OCaml source of the project.
The `build.sh` script compiles and runs the `main.ml` file, compiles its output into an executable, and runs it.

There are a few OCaml files in the `src` directory:

 * `common.ml` contains miscellaneous utilities common to both languages, namely `BinOps`and the concept of an environment.
 * `driver.ml` contains glue code that parses and compiles `Src_lang` code, handling exceptions.
 * `lexer.mll` and `parser.mly` contain the parser. See "Going Further" below if you're interested in these.
 * `main.ml` simply reads a file in and runs it.
    If you want to do debugging, this is the place.
 * `src_lang.ml` contains the definition of `Src_lang`, as well as pretty-printing and type-checking.
 * `tgt_lang.ml` contains the definition of `Tgt_lang`, as well as pretty-printing and type-checking.
 * `translate.ml` is the main file you should be editing.
    It contains a function `translate : Src_lang.program -> Tgt_lang.program`.
