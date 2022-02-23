open FO.FO
open Verified

let string_of_list string_of_val xs = String.concat ", " (List.map string_of_val xs)

let nat_of_int n = Monitor.nat_of_integer (Z.of_int n)

let rec conv_db = function
  | [] -> Monitor.empty_db
  | (r, t) :: rts -> Monitor.insert_into_db (r, nat_of_int (List.length t)) (List.map (fun x -> Some x) t) (conv_db rts)

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
      let ([(_, (_, Monitor.RBT_set x))], _) = Monitor.mstep (db, nat_of_int 0) init_state in
      let sz = Monitor.rbt_fold (fun _ c -> c + 1) x 0 in
      let res = List.length (fv_fmla f) * sz in
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

let cost db f = sum (List.map (fun f -> eval f db) (List.filter ranf (subs f)))

let dump prefix fin inf nfin ninf nrfin nrinf =
  let _ =
      (let ch = open_out (prefix ^ nfin) in
      Printf.fprintf ch "%s\n" (string_of_fmla string_of_int (fun f -> f) fin); close_out ch) in
  let _ =
      (let ch = open_out (prefix ^ ninf) in
      Printf.fprintf ch "%s\n" (string_of_fmla string_of_int (fun f -> f) inf); close_out ch) in
  let _ =
      (let ch = open_out (prefix ^ nrfin) in
      Printf.fprintf ch "%s\n" (ra_of_fmla string_of_int (fun f -> f) fin); close_out ch) in
  let _ =
      (let ch = open_out (prefix ^ nrinf) in
      Printf.fprintf ch "%s\n" (ra_of_fmla string_of_int (fun f -> f) inf); close_out ch) in
  ()

let parse prefix =
  let f =
    (let ch = open_in (prefix ^ ".fo") in
     let f = Fo_parser.formula Fo_lexer.token (Lexing.from_channel ch) in
     (close_in ch; f)) in
  let tdb =
    (let ch = open_in (prefix ^ ".tdb") in
     let db = Db_parser.db Db_lexer.token (Lexing.from_channel ch) in
     (close_in ch; conv_db db)) in
  (f, tdb)

let rtrans prefix db f =
  let (sfin, sinf) = rtrans (cost db) f in
  let _ = assert (is_srnf sfin) in
  let _ = assert (is_srnf sinf) in
  let _ = assert (ranf sfin) in
  let _ = assert (ranf sinf) in
  let _ = dump prefix sfin sinf "sfin" "sinf" "srfin" "srinf" in
  let (afin, ainf) = (agg_of_fmla (cost db) sfin, agg_of_fmla (cost db) sinf) in
  let _ = assert (is_srnf afin) in
  let _ = assert (is_srnf ainf) in
  let _ = assert (ranf afin) in
  let _ = assert (ranf ainf) in
  let _ = dump prefix afin ainf "afin" "ainf" "arfin" "arinf" in
  ()

let vgtrans prefix db f =
  if not (evaluable f) then () else
  let (vsfin, vsinf) = vgtrans (cost db) f in
  let _ = assert (is_srnf vsfin) in
  let _ = assert (is_srnf vsinf) in
  let _ = assert (ranf vsfin) in
  let _ = assert (ranf vsinf) in
  let _ = dump prefix vsfin vsinf "sfin" "sinf" "srfin" "srinf" in
  let (vafin, vainf) = (agg_of_fmla (cost db) vsfin, agg_of_fmla (cost db) vsinf) in
  let _ = assert (is_srnf vafin) in
  let _ = assert (is_srnf vainf) in
  let _ = assert (ranf vafin) in
  let _ = assert (ranf vainf) in
  let _ = dump prefix vafin vainf "afin" "ainf" "arfin" "arinf" in
  ()
