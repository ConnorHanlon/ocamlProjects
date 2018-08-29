open Common
open Translate

let parse (src: string) : Src_lang.program =
  Parser.program Lexer.src_lang (Lexing.from_string src)

let parse_file (filename: string) : Src_lang.program =
  Parser.program Lexer.src_lang (Lexing.from_channel (open_in filename))

let compile (src: Src_lang.program) : string =
  try
    let tgt = translate src
    in Tgt_lang_tyck.type_check_program Env.empty tgt;
    String.concat ""
      [ "/* Source program:\n"
      ; Src_lang_pp.string_of_program src
      ; "\n*/\n\n"
      ; Tgt_lang_pp.string_of_program tgt
      ]
  with
  | Src_lang_tyck.TypeError (expr, got, wanted) ->
    Printexc.print_backtrace stderr;
    failwith (String.concat ""
                [ "In source language: Type error in `"
                ; Src_lang_pp.string_of_expr expr
                ; "': got type `"
                ; Src_lang_pp.string_of_type got
                ; "', wanted type `"
                ; Src_lang_pp.string_of_type wanted
                ; "'."
                ])
  | Src_lang_tyck.UndefinedVar name ->
    Printexc.print_backtrace stderr;
    failwith (String.concat ""
                [ "In source language: Undefined variable `"
                ; name
                ; "'."
                ])
  | Tgt_lang_tyck.CallTypeError (expr, got, wanted) ->
    Printexc.print_backtrace stderr;
    failwith (String.concat ""
                [ "In target language: Type error in arguments in expression `"
                ; Tgt_lang_pp.string_of_expr expr
                ; "': got types `"
                ; String.concat ", " (List.map Tgt_lang_pp.string_of_type got)
                ; "', wanted types `"
                ; String.concat ", " (List.map Tgt_lang_pp.string_of_type wanted)
                ; "'."
                ])
  | Tgt_lang_tyck.ExprTypeError (expr, got, wanted) ->
    Printexc.print_backtrace stderr;
    failwith (String.concat ""
                [ "In target language: Type error in expression `"
                ; Tgt_lang_pp.string_of_expr expr
                ; "': got type `"
                ; Tgt_lang_pp.string_of_type got
                ; "', wanted type `"
                ; Tgt_lang_pp.string_of_type wanted
                ; "'."
                ])
  | Tgt_lang_tyck.StmtTypeError (stmt, expr, got, wanted) ->
    Printexc.print_backtrace stderr;
    failwith (String.concat ""
                [ "In target language: Type error in expression `"
                ; Tgt_lang_pp.string_of_expr expr
                ; "' in statement `"
                ; Tgt_lang_pp.string_of_stmt stmt
                ; "': got type `"
                ; Tgt_lang_pp.string_of_type got
                ; "', wanted type `"
                ; Tgt_lang_pp.string_of_type wanted
                ; "'."
                ])
  | Tgt_lang_tyck.InitReturn typ_ ->
    Printexc.print_backtrace stderr;
    failwith (String.concat ""
                [ "In target language: init returns `"
                ; Tgt_lang_pp.string_of_type typ_
                ; "', wanted type `"
                ; Tgt_lang_pp.string_of_type Tgt_lang.IntType
                ; "'."
                ])
  | Tgt_lang_tyck.NotAFunction (expr, name) ->
    Printexc.print_backtrace stderr;
    failwith (String.concat ""
                [ "In target language: `"
                ; name
                ; "' in expression `"
                ; Tgt_lang_pp.string_of_expr expr
                ; "' is not a function."
                ])
  | Tgt_lang_tyck.UndefinedFunc (expr, name) ->
    Printexc.print_backtrace stderr;
    failwith (String.concat ""
                [ "In target language: Undefined function `"
                ; name
                ; "' in expression `"
                ; Tgt_lang_pp.string_of_expr expr
                ; "'."
                ])
  | Tgt_lang_tyck.UndefinedVar name ->
    Printexc.print_backtrace stderr;
    failwith (String.concat ""
                [ "In target language: Undefined variable `"
                ; name
                ; "'."
                ])
  | Match_failure (file, line, column) ->
    Printexc.print_backtrace stderr;
    failwith (String.concat ""
                [ "Match failure at "
                ; file
                ; ", line "
                ; string_of_int line
                ; ", column "
                ; string_of_int column
                ; "."
                ])
  | err -> Printexc.print_backtrace stderr; raise err
