open Trans

let _ =
  let prefix = Sys.argv.(1) in
  let (f, tdb(*, db*)) = parse prefix in
  let _ = vgtrans (prefix ^ ".v") tdb (*db *)f in
  ()
