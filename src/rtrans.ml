open Trans

let _ =
  let prefix = Sys.argv.(1) in
  let (f, tdb) = parse prefix in
  let _ = rtrans (prefix ^ ".") tdb f in
  ()
