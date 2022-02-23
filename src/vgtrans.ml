open Trans

let _ =
  let prefix = Sys.argv.(1) in
  let (f, tdb) = parse prefix in
  let _ = vgtrans (prefix ^ ".v") tdb f in
  ()
