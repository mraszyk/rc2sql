open Trans

let _ =
  let prefix = Sys.argv.(1) in
  let (f, tdb(*, db*)) = parse prefix in
  let _ = rtrans (prefix ^ ".") tdb (*db *)f in
  let _ =
    (let ch = open_out (prefix ^ ".mfotl") in
     Printf.fprintf ch "%s\n" (FO.FO.string_of_fmla string_of_int (fun x -> "?" ^ x) f); close_out ch) in
  ()
