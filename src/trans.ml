open FO.FO
open Verified

let string_of_list string_of_val xs = String.concat ", " (List.map string_of_val xs)

let rec conv_db = function
  | [] -> []
  | (r, t) :: rts ->
    let rec aux = function
    | [] -> [(r, Monitor.rbt_insert t Monitor.rbt_empty)]
    | ((r', ts) :: rtss) -> if r = r' then (r, Monitor.rbt_insert t ts) :: rtss else (r', ts) :: aux rtss
    in aux (conv_db rts)

let nat_of_int n = Monitor.nat_of_integer (Z.of_int n)

let var x = Monitor.Var (nat_of_int x)
let const c = Monitor.Const (Monitor.EInt (Z.of_int c))

let tt = Monitor.Eq (const 0, const 0)
let ff = Monitor.Neg tt

let rec conv_fo fv =
  let rec lup i n = function
    | (v :: vs) -> if i = v then n else lup i (n + 1) vs in
  let rec conv_trm = function
    | Const c -> const c
    | Var v -> var (lup v 0 fv)
    | Mult (t1, t2) -> Monitor.Mult (conv_trm t1, conv_trm t2) in
  let rec aux fv = function
    | False -> ff
    | True -> tt
    | Eq (x, t) -> Monitor.Eq (conv_trm (Var x), conv_trm t)
    | Pred (r, ts) -> Monitor.Pred (r, List.map conv_trm ts)
    | Neg f -> Monitor.Neg (conv_fo fv f)
    | Conj (f, g) -> Monitor.And (conv_fo fv f, conv_fo fv g)
    | Disj (f, g) -> Monitor.Or (conv_fo fv f, conv_fo fv g)
    | Exists (v, f) -> Monitor.Exists (conv_fo (v :: fv) f)
    | Cnt (c, vs, f) -> Monitor.Agg (nat_of_int (lup c 0 fv), (Monitor.Agg_Cnt, Monitor.EInt (Z.of_int 0)), nat_of_int (List.length vs), const 1, conv_fo (vs @ fv) f)
  in aux fv

let map = Hashtbl.create 100000

let eval f db =
  try
    Hashtbl.find map f
  with
    | Not_found ->
      let init_state = Monitor.minit_safe (conv_fo (fv_fmla f) f) in
      let ([(_, RBT_set ts)], _) = Monitor.mstep (db, Monitor.nat_of_integer (Z.of_int 0)) init_state in
      let res = List.length (fv_fmla f) * Monitor.rbt_fold (fun _ c -> c + 1) ts 0 in
      Hashtbl.add map f res; res

let rec subs = function
  | Neg f -> Misc.union (subs f) [Neg f]
  | Conj (f, g) -> Misc.union (Misc.union (subs f) (subs g)) [Conj (f, g)]
  | Disj (f, g) -> Misc.union (Misc.union (subs f) (subs g)) [Disj (f, g)]
  | Exists (v, f) -> Misc.union (subs f) [Exists (v, f)]
  | Cnt (c, vs, f) -> Misc.union (subs f) [Cnt (c, vs, f)]
  | f -> [f]

let rec sum = function
  | [] -> 0
  | x :: xs -> x + sum xs

let cost db f =
  (* let _ = Printf.printf "cost: %s\n" (string_of_fmla string_of_int (fun f -> f) f) in *)
  sum (List.map (fun f -> let n = eval f db in (* Printf.printf "  %s: %d\n" (string_of_fmla string_of_int (fun f -> f) f) n; *) n) (List.filter ranf (subs f)))

let sdump prefix sfin sinf =
  let _ =
      (let ch = open_out (prefix ^ "sfin") in
      Printf.fprintf ch "%s\n" (string_of_fmla string_of_int (fun f -> f) sfin); close_out ch) in
  let _ =
      (let ch = open_out (prefix ^ "sinf") in
      Printf.fprintf ch "%s\n" (string_of_fmla string_of_int (fun f -> f) sinf); close_out ch) in
  let _ =
      (let ch = open_out (prefix ^ "srfin") in
      Printf.fprintf ch "%s\n" (ra_of_fmla string_of_int (fun f -> f) sfin); close_out ch) in
  let _ =
      (let ch = open_out (prefix ^ "srinf") in
      Printf.fprintf ch "%s\n" (ra_of_fmla string_of_int (fun f -> f) sinf); close_out ch) in
  ()

let adump prefix afin ainf =
  let _ =
      (let ch = open_out (prefix ^ "afin") in
      Printf.fprintf ch "%s\n" (string_of_fmla string_of_int (fun f -> f) afin); close_out ch) in
  let _ =
      (let ch = open_out (prefix ^ "ainf") in
      Printf.fprintf ch "%s\n" (string_of_fmla string_of_int (fun f -> f) ainf); close_out ch) in
  let _ =
      (let ch = open_out (prefix ^ "arfin") in
      Printf.fprintf ch "%s\n" (ra_of_fmla string_of_int (fun f -> f) afin); close_out ch) in
  let _ =
      (let ch = open_out (prefix ^ "arinf") in
      Printf.fprintf ch "%s\n" (ra_of_fmla string_of_int (fun f -> f) ainf); close_out ch) in
  ()

let parse prefix =
  let f =
    (let ch = open_in (prefix ^ ".fo") in
     let f = Fo_parser.formula Fo_lexer.token (Lexing.from_channel ch) in
     (close_in ch; f)) in
  let tdb =
    (let ch = open_in (prefix ^ ".tdb") in
     let db = Db_parser.db Db_lexer.token (Lexing.from_channel ch) in
     (close_in ch; Monitor.mk_db (List.map (fun (r, ts) -> (r, Monitor.RBT_set ts)) (conv_db db)))) in
  (*
  let db =
    (let ch = open_in (prefix ^ ".db") in
     let db = Db_parser.db Db_lexer.token (Lexing.from_channel ch) in
     (close_in ch; Monitor.mk_db (List.map (fun (r, ts) -> (r, Monitor.RBT_set ts)) (conv_db db)))) in
  *)
  (f, tdb(*, db*))

let rtrans prefix tdb (*db *)f =
  let (sfin, sinf) = rtrans (cost tdb) f in
  let _ = assert (is_srnf sfin) in
  let _ = assert (is_srnf sinf) in
  let _ = assert (ranf sfin) in
  let _ = assert (ranf sinf) in
  let _ = sdump prefix sfin sinf in
  let (afin, ainf) = (agg_of_fmla (cost tdb) sfin, agg_of_fmla (cost tdb) sinf) in
  let _ = assert (is_srnf afin) in
  let _ = assert (is_srnf ainf) in
  let _ = assert (ranf afin) in
  let _ = assert (ranf ainf) in
  let _ = adump prefix afin ainf in
  (*
  let _ = Hashtbl.clear map in
  let _ = Monitor.lupclear () in
  let _ = Printf.printf "%d\n" (cost db sfin) in
  let _ = Printf.printf "%d\n" (cost db afin) in
  let _ =
      (let ch = open_out (prefix ^ "rstxt") in
      Printf.fprintf ch "%s\n" (pp_fmla string_of_int (fun f -> f) sfin); close_out ch) in
  let _ =
      (let ch = open_out (prefix ^ "ratxt") in
      Printf.fprintf ch "%s\n" (pp_fmla string_of_int (fun f -> f) afin); close_out ch) in
  *)
  ()

let vgtrans prefix tdb (*db *)f =
  if not (evaluable f) then () else
  let (vsfin, vsinf) = vgtrans (cost tdb) f in
  let _ = assert (is_srnf vsfin) in
  let _ = assert (is_srnf vsinf) in
  let _ = assert (ranf vsfin) in
  let _ = assert (ranf vsinf) in
  let _ = sdump prefix vsfin vsinf in
  let (vafin, vainf) = (agg_of_fmla (cost tdb) vsfin, agg_of_fmla (cost tdb) vsinf) in
  let _ = assert (is_srnf vafin) in
  let _ = assert (is_srnf vainf) in
  let _ = assert (ranf vafin) in
  let _ = assert (ranf vainf) in
  let _ = adump prefix vafin vainf in
  (*
  let _ = Hashtbl.clear map in
  let _ = Monitor.lupclear () in
  let _ = Printf.printf "%d\n" (cost db vsfin) in
  let _ = Printf.printf "%d\n" (cost db vafin) in
  let _ =
      (let ch = open_out (prefix ^ "vstxt") in
      Printf.fprintf ch "%s\n" (pp_fmla string_of_int (fun f -> f) vsfin); close_out ch) in
  let _ =
      (let ch = open_out (prefix ^ "vatxt") in
      Printf.fprintf ch "%s\n" (pp_fmla string_of_int (fun f -> f) vafin); close_out ch) in
  *)
  ()
