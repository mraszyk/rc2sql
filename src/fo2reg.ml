let _ =
  let prefix = Sys.argv.(1) in
  let f =
    (let ch = open_in (prefix ^ ".fo") in
     let f = Fo_parser.formula Fo_lexer.token (Lexing.from_channel ch) in
     (close_in ch; f)) in
  let _ =
    (let ch = open_out (prefix ^ ".mrfotl") in
     Printf.fprintf ch "%s\n" (FO.FO.string_of_fmla string_of_int (fun x -> "?" ^ x) f); close_out ch) in
  ()
