module Eval_FO : sig
  type nat
  val integer_of_nat : nat -> Z.t
  type 'a set_dlist
  type ('b, 'a) mapping_rbt
  type 'a set = Collect_set of ('a -> bool) | DList_set of 'a set_dlist |
    RBT_set of ('a, unit) mapping_rbt | Set_Monad of 'a list |
    Complement of 'a set
  val nat_of_integer : Z.t -> nat
  type ('a, 'b) sum = Inl of 'a | Inr of 'b
  type 'a fo_term = Const of 'a | Var of nat
  type ('a, 'b) fo_fmla = Pred of 'b * 'a fo_term list | Bool of bool |
    Eqa of 'a fo_term * 'a fo_term | Neg of ('a, 'b) fo_fmla |
    Conj of ('a, 'b) fo_fmla * ('a, 'b) fo_fmla |
    Disj of ('a, 'b) fo_fmla * ('a, 'b) fo_fmla |
    Exists of nat * ('a, 'b) fo_fmla | Forall of nat * ('a, 'b) fo_fmla
  type 'a eval_res = Fin of ('a list) set | Infin | Wf_error
  val fv_fo_fmla_list : ('a, 'b) fo_fmla -> nat list
  val sat_fin_nat :
    (nat, string) fo_fmla ->
      (string * nat -> (nat list) set) -> (nat -> nat) -> bool
  val eval_fin_nat :
    (nat, string) fo_fmla -> (string * nat -> (nat list) set) -> nat eval_res
  val rbt_nat_fold : (nat -> 'a -> 'a) -> (nat, unit) mapping_rbt -> 'a -> 'a
  val convert_nat_db :
    (string * nat list) list -> string * nat -> (nat list) set
  val rbt_nat_list_fold :
    (nat list -> 'a -> 'a) -> ((nat list), unit) mapping_rbt -> 'a -> 'a
  val rbt_sum_list_fold :
    ((nat, nat) sum list -> 'a -> 'a) ->
      (((nat, nat) sum list), unit) mapping_rbt -> 'a -> 'a
end = struct

type nat = Nat of Z.t;;

let rec integer_of_nat (Nat x) = x;;

let rec equal_nata m n = Z.equal (integer_of_nat m) (integer_of_nat n);;

type 'a equal = {equal : 'a -> 'a -> bool};;
let equal _A = _A.equal;;

let equal_nat = ({equal = equal_nata} : nat equal);;

let rec less_eq_nat m n = Z.leq (integer_of_nat m) (integer_of_nat n);;

type 'a ord = {less_eq : 'a -> 'a -> bool; less : 'a -> 'a -> bool};;
let less_eq _A = _A.less_eq;;
let less _A = _A.less;;

let rec less_nat m n = Z.lt (integer_of_nat m) (integer_of_nat n);;

let ord_nat = ({less_eq = less_eq_nat; less = less_nat} : nat ord);;

type 'a preorder = {ord_preorder : 'a ord};;

type 'a order = {preorder_order : 'a preorder};;

let preorder_nat = ({ord_preorder = ord_nat} : nat preorder);;

let order_nat = ({preorder_order = preorder_nat} : nat order);;

let ceq_nata : (nat -> nat -> bool) option = Some equal_nata;;

type 'a ceq = {ceq : ('a -> 'a -> bool) option};;
let ceq _A = _A.ceq;;

let ceq_nat = ({ceq = ceq_nata} : nat ceq);;

type 'a infinite = unit;;

let infinite_nat = (() : nat infinite);;

type ('a, 'b) phantom = Phantom of 'b;;

type set_impla = Set_Choose | Set_Collect | Set_DList | Set_RBT | Set_Monada;;

let set_impl_nata : (nat, set_impla) phantom = Phantom Set_RBT;;

type 'a set_impl = {set_impl : ('a, set_impla) phantom};;
let set_impl _A = _A.set_impl;;

let set_impl_nat = ({set_impl = set_impl_nata} : nat set_impl);;

type 'a linorder = {order_linorder : 'a order};;

let linorder_nat = ({order_linorder = order_nat} : nat linorder);;

let finite_UNIV_nata : (nat, bool) phantom = Phantom false;;

let zero_nat : nat = Nat Z.zero;;

let card_UNIV_nata : (nat, nat) phantom = Phantom zero_nat;;

type 'a finite_UNIV = {finite_UNIV : ('a, bool) phantom};;
let finite_UNIV _A = _A.finite_UNIV;;

type 'a card_UNIV =
  {finite_UNIV_card_UNIV : 'a finite_UNIV; card_UNIV : ('a, nat) phantom};;
let card_UNIV _A = _A.card_UNIV;;

let finite_UNIV_nat = ({finite_UNIV = finite_UNIV_nata} : nat finite_UNIV);;

let card_UNIV_nat =
  ({finite_UNIV_card_UNIV = finite_UNIV_nat; card_UNIV = card_UNIV_nata} :
    nat card_UNIV);;

let cEnum_nat :
  (nat list * (((nat -> bool) -> bool) * ((nat -> bool) -> bool))) option
  = None;;

type 'a cenum =
  {cEnum :
     ('a list * ((('a -> bool) -> bool) * (('a -> bool) -> bool))) option};;
let cEnum _A = _A.cEnum;;

let cenum_nat = ({cEnum = cEnum_nat} : nat cenum);;

type ordera = Eq | Lt | Gt;;

let rec eq _A a b = equal _A a b;;

let rec comparator_of (_A1, _A2)
  x y = (if less _A2.order_linorder.preorder_order.ord_preorder x y then Lt
          else (if eq _A1 x y then Eq else Gt));;

let rec compare_nat x = comparator_of (equal_nat, linorder_nat) x;;

let ccompare_nata : (nat -> nat -> ordera) option = Some compare_nat;;

type 'a ccompare = {ccompare : ('a -> 'a -> ordera) option};;
let ccompare _A = _A.ccompare;;

let ccompare_nat = ({ccompare = ccompare_nata} : nat ccompare);;

type mapping_impla = Mapping_Choose | Mapping_Assoc_List | Mapping_RBT |
  Mapping_Mapping;;

let mapping_impl_nata : (nat, mapping_impla) phantom = Phantom Mapping_RBT;;

type 'a mapping_impl = {mapping_impl : ('a, mapping_impla) phantom};;
let mapping_impl _A = _A.mapping_impl;;

let mapping_impl_nat = ({mapping_impl = mapping_impl_nata} : nat mapping_impl);;

let rec max _A a b = (if less_eq _A a b then b else a);;

let ord_integer = ({less_eq = Z.leq; less = Z.lt} : Z.t ord);;

let rec minus_nat
  m n = Nat (max ord_integer Z.zero
              (Z.sub (integer_of_nat m) (integer_of_nat n)));;

type num = One | Bit0 of num | Bit1 of num;;

let one_nat : nat = Nat (Z.of_int 1);;

let rec proper_interval_nat
  no x1 = match no, x1 with no, None -> true
    | None, Some x -> less_nat zero_nat x
    | Some x, Some y -> less_nat one_nat (minus_nat y x);;

let rec cproper_interval_nata x = proper_interval_nat x;;

type 'a cproper_interval =
  {ccompare_cproper_interval : 'a ccompare;
    cproper_interval : 'a option -> 'a option -> bool};;
let cproper_interval _A = _A.cproper_interval;;

let cproper_interval_nat =
  ({ccompare_cproper_interval = ccompare_nat;
     cproper_interval = cproper_interval_nata}
    : nat cproper_interval);;

type 'a set_dlist = Abs_dlist of 'a list;;

let rec list_of_dlist _A (Abs_dlist x) = x;;

let rec list_member
  equal x1 y = match equal, x1, y with
    equal, x :: xs, y -> equal x y || list_member equal xs y
    | equal, [], y -> false;;

type color = R | B;;

type ('a, 'b) rbt = Empty |
  Branch of color * ('a, 'b) rbt * 'a * 'b * ('a, 'b) rbt;;

type ('b, 'a) mapping_rbt = Mapping_RBTa of ('b, 'a) rbt;;

type 'a set = Collect_set of ('a -> bool) | DList_set of 'a set_dlist |
  RBT_set of ('a, unit) mapping_rbt | Set_Monad of 'a list |
  Complement of 'a set;;

let rec uminus_set = function Complement b -> b
                     | Collect_set p -> Collect_set (fun x -> not (p x))
                     | a -> Complement a;;

let rec balance
  x0 s t x3 = match x0, s, t, x3 with
    Branch (R, a, w, x, b), s, t, Branch (R, c, y, z, d) ->
      Branch (R, Branch (B, a, w, x, b), s, t, Branch (B, c, y, z, d))
    | Branch (R, Branch (R, a, w, x, b), s, t, c), y, z, Empty ->
        Branch (R, Branch (B, a, w, x, b), s, t, Branch (B, c, y, z, Empty))
    | Branch (R, Branch (R, a, w, x, b), s, t, c), y, z,
        Branch (B, va, vb, vc, vd)
        -> Branch
             (R, Branch (B, a, w, x, b), s, t,
               Branch (B, c, y, z, Branch (B, va, vb, vc, vd)))
    | Branch (R, Empty, w, x, Branch (R, b, s, t, c)), y, z, Empty ->
        Branch (R, Branch (B, Empty, w, x, b), s, t, Branch (B, c, y, z, Empty))
    | Branch (R, Branch (B, va, vb, vc, vd), w, x, Branch (R, b, s, t, c)), y,
        z, Empty
        -> Branch
             (R, Branch (B, Branch (B, va, vb, vc, vd), w, x, b), s, t,
               Branch (B, c, y, z, Empty))
    | Branch (R, Empty, w, x, Branch (R, b, s, t, c)), y, z,
        Branch (B, va, vb, vc, vd)
        -> Branch
             (R, Branch (B, Empty, w, x, b), s, t,
               Branch (B, c, y, z, Branch (B, va, vb, vc, vd)))
    | Branch (R, Branch (B, ve, vf, vg, vh), w, x, Branch (R, b, s, t, c)), y,
        z, Branch (B, va, vb, vc, vd)
        -> Branch
             (R, Branch (B, Branch (B, ve, vf, vg, vh), w, x, b), s, t,
               Branch (B, c, y, z, Branch (B, va, vb, vc, vd)))
    | Empty, w, x, Branch (R, b, s, t, Branch (R, c, y, z, d)) ->
        Branch (R, Branch (B, Empty, w, x, b), s, t, Branch (B, c, y, z, d))
    | Branch (B, va, vb, vc, vd), w, x,
        Branch (R, b, s, t, Branch (R, c, y, z, d))
        -> Branch
             (R, Branch (B, Branch (B, va, vb, vc, vd), w, x, b), s, t,
               Branch (B, c, y, z, d))
    | Empty, w, x, Branch (R, Branch (R, b, s, t, c), y, z, Empty) ->
        Branch (R, Branch (B, Empty, w, x, b), s, t, Branch (B, c, y, z, Empty))
    | Empty, w, x,
        Branch (R, Branch (R, b, s, t, c), y, z, Branch (B, va, vb, vc, vd))
        -> Branch
             (R, Branch (B, Empty, w, x, b), s, t,
               Branch (B, c, y, z, Branch (B, va, vb, vc, vd)))
    | Branch (B, va, vb, vc, vd), w, x,
        Branch (R, Branch (R, b, s, t, c), y, z, Empty)
        -> Branch
             (R, Branch (B, Branch (B, va, vb, vc, vd), w, x, b), s, t,
               Branch (B, c, y, z, Empty))
    | Branch (B, va, vb, vc, vd), w, x,
        Branch (R, Branch (R, b, s, t, c), y, z, Branch (B, ve, vf, vg, vh))
        -> Branch
             (R, Branch (B, Branch (B, va, vb, vc, vd), w, x, b), s, t,
               Branch (B, c, y, z, Branch (B, ve, vf, vg, vh)))
    | Empty, s, t, Empty -> Branch (B, Empty, s, t, Empty)
    | Empty, s, t, Branch (B, va, vb, vc, vd) ->
        Branch (B, Empty, s, t, Branch (B, va, vb, vc, vd))
    | Empty, s, t, Branch (v, Empty, vb, vc, Empty) ->
        Branch (B, Empty, s, t, Branch (v, Empty, vb, vc, Empty))
    | Empty, s, t, Branch (v, Branch (B, ve, vf, vg, vh), vb, vc, Empty) ->
        Branch
          (B, Empty, s, t,
            Branch (v, Branch (B, ve, vf, vg, vh), vb, vc, Empty))
    | Empty, s, t, Branch (v, Empty, vb, vc, Branch (B, vf, vg, vh, vi)) ->
        Branch
          (B, Empty, s, t,
            Branch (v, Empty, vb, vc, Branch (B, vf, vg, vh, vi)))
    | Empty, s, t,
        Branch
          (v, Branch (B, ve, vj, vk, vl), vb, vc, Branch (B, vf, vg, vh, vi))
        -> Branch
             (B, Empty, s, t,
               Branch
                 (v, Branch (B, ve, vj, vk, vl), vb, vc,
                   Branch (B, vf, vg, vh, vi)))
    | Branch (B, va, vb, vc, vd), s, t, Empty ->
        Branch (B, Branch (B, va, vb, vc, vd), s, t, Empty)
    | Branch (B, va, vb, vc, vd), s, t, Branch (B, ve, vf, vg, vh) ->
        Branch (B, Branch (B, va, vb, vc, vd), s, t, Branch (B, ve, vf, vg, vh))
    | Branch (B, va, vb, vc, vd), s, t, Branch (v, Empty, vf, vg, Empty) ->
        Branch
          (B, Branch (B, va, vb, vc, vd), s, t,
            Branch (v, Empty, vf, vg, Empty))
    | Branch (B, va, vb, vc, vd), s, t,
        Branch (v, Branch (B, vi, vj, vk, vl), vf, vg, Empty)
        -> Branch
             (B, Branch (B, va, vb, vc, vd), s, t,
               Branch (v, Branch (B, vi, vj, vk, vl), vf, vg, Empty))
    | Branch (B, va, vb, vc, vd), s, t,
        Branch (v, Empty, vf, vg, Branch (B, vj, vk, vl, vm))
        -> Branch
             (B, Branch (B, va, vb, vc, vd), s, t,
               Branch (v, Empty, vf, vg, Branch (B, vj, vk, vl, vm)))
    | Branch (B, va, vb, vc, vd), s, t,
        Branch
          (v, Branch (B, vi, vn, vo, vp), vf, vg, Branch (B, vj, vk, vl, vm))
        -> Branch
             (B, Branch (B, va, vb, vc, vd), s, t,
               Branch
                 (v, Branch (B, vi, vn, vo, vp), vf, vg,
                   Branch (B, vj, vk, vl, vm)))
    | Branch (v, Empty, vb, vc, Empty), s, t, Empty ->
        Branch (B, Branch (v, Empty, vb, vc, Empty), s, t, Empty)
    | Branch (v, Empty, vb, vc, Branch (B, ve, vf, vg, vh)), s, t, Empty ->
        Branch
          (B, Branch (v, Empty, vb, vc, Branch (B, ve, vf, vg, vh)), s, t,
            Empty)
    | Branch (v, Branch (B, vf, vg, vh, vi), vb, vc, Empty), s, t, Empty ->
        Branch
          (B, Branch (v, Branch (B, vf, vg, vh, vi), vb, vc, Empty), s, t,
            Empty)
    | Branch
        (v, Branch (B, vf, vg, vh, vi), vb, vc, Branch (B, ve, vj, vk, vl)),
        s, t, Empty
        -> Branch
             (B, Branch
                   (v, Branch (B, vf, vg, vh, vi), vb, vc,
                     Branch (B, ve, vj, vk, vl)),
               s, t, Empty)
    | Branch (v, Empty, vf, vg, Empty), s, t, Branch (B, va, vb, vc, vd) ->
        Branch
          (B, Branch (v, Empty, vf, vg, Empty), s, t,
            Branch (B, va, vb, vc, vd))
    | Branch (v, Empty, vf, vg, Branch (B, vi, vj, vk, vl)), s, t,
        Branch (B, va, vb, vc, vd)
        -> Branch
             (B, Branch (v, Empty, vf, vg, Branch (B, vi, vj, vk, vl)), s, t,
               Branch (B, va, vb, vc, vd))
    | Branch (v, Branch (B, vj, vk, vl, vm), vf, vg, Empty), s, t,
        Branch (B, va, vb, vc, vd)
        -> Branch
             (B, Branch (v, Branch (B, vj, vk, vl, vm), vf, vg, Empty), s, t,
               Branch (B, va, vb, vc, vd))
    | Branch
        (v, Branch (B, vj, vk, vl, vm), vf, vg, Branch (B, vi, vn, vo, vp)),
        s, t, Branch (B, va, vb, vc, vd)
        -> Branch
             (B, Branch
                   (v, Branch (B, vj, vk, vl, vm), vf, vg,
                     Branch (B, vi, vn, vo, vp)),
               s, t, Branch (B, va, vb, vc, vd));;

let rec rbt_comp_ins
  c f k v x4 = match c, f, k, v, x4 with
    c, f, k, v, Empty -> Branch (R, Empty, k, v, Empty)
    | c, f, k, v, Branch (B, l, x, y, r) ->
        (match c k x with Eq -> Branch (B, l, x, f k y v, r)
          | Lt -> balance (rbt_comp_ins c f k v l) x y r
          | Gt -> balance l x y (rbt_comp_ins c f k v r))
    | c, f, k, v, Branch (R, l, x, y, r) ->
        (match c k x with Eq -> Branch (R, l, x, f k y v, r)
          | Lt -> Branch (R, rbt_comp_ins c f k v l, x, y, r)
          | Gt -> Branch (R, l, x, y, rbt_comp_ins c f k v r));;

let rec paint c x1 = match c, x1 with c, Empty -> Empty
                | c, Branch (uu, l, k, v, r) -> Branch (c, l, k, v, r);;

let rec rbt_comp_insert_with_key c f k v t = paint B (rbt_comp_ins c f k v t);;

let rec rbt_comp_insert c = rbt_comp_insert_with_key c (fun _ _ nv -> nv);;

let rec impl_ofa _B (Mapping_RBTa x) = x;;

let rec the (Some x2) = x2;;

let rec insertb _A
  xc xd xe =
    Mapping_RBTa (rbt_comp_insert (the (ccompare _A)) xc xd (impl_ofa _A xe));;

let rec rbt_baliR
  t1 ab bb x3 = match t1, ab, bb, x3 with
    t1, ab, bb, Branch (R, t2, aa, ba, Branch (R, t3, a, b, t4)) ->
      Branch (R, Branch (B, t1, ab, bb, t2), aa, ba, Branch (B, t3, a, b, t4))
    | t1, ab, bb, Branch (R, Branch (R, t2, aa, ba, t3), a, b, Empty) ->
        Branch
          (R, Branch (B, t1, ab, bb, t2), aa, ba, Branch (B, t3, a, b, Empty))
    | t1, ab, bb,
        Branch (R, Branch (R, t2, aa, ba, t3), a, b, Branch (B, va, vb, vc, vd))
        -> Branch
             (R, Branch (B, t1, ab, bb, t2), aa, ba,
               Branch (B, t3, a, b, Branch (B, va, vb, vc, vd)))
    | t1, a, b, Empty -> Branch (B, t1, a, b, Empty)
    | t1, a, b, Branch (B, va, vb, vc, vd) ->
        Branch (B, t1, a, b, Branch (B, va, vb, vc, vd))
    | t1, a, b, Branch (v, Empty, vb, vc, Empty) ->
        Branch (B, t1, a, b, Branch (v, Empty, vb, vc, Empty))
    | t1, a, b, Branch (v, Branch (B, ve, vf, vg, vh), vb, vc, Empty) ->
        Branch
          (B, t1, a, b, Branch (v, Branch (B, ve, vf, vg, vh), vb, vc, Empty))
    | t1, a, b, Branch (v, Empty, vb, vc, Branch (B, vf, vg, vh, vi)) ->
        Branch
          (B, t1, a, b, Branch (v, Empty, vb, vc, Branch (B, vf, vg, vh, vi)))
    | t1, a, b,
        Branch
          (v, Branch (B, ve, vj, vk, vl), vb, vc, Branch (B, vf, vg, vh, vi))
        -> Branch
             (B, t1, a, b,
               Branch
                 (v, Branch (B, ve, vj, vk, vl), vb, vc,
                   Branch (B, vf, vg, vh, vi)));;

let rec equal_color x0 x1 = match x0, x1 with R, B -> false
                      | B, R -> false
                      | B, B -> true
                      | R, R -> true;;

let rec plus_nat m n = Nat (Z.add (integer_of_nat m) (integer_of_nat n));;

let rec suc n = plus_nat n one_nat;;

let rec bheight
  = function Empty -> zero_nat
    | Branch (c, lt, k, v, rt) ->
        (if equal_color c B then suc (bheight lt) else bheight lt);;

let rec rbt_joinR
  l a b r =
    (if less_eq_nat (bheight l) (bheight r) then Branch (R, l, a, b, r)
      else (match l
             with Branch (R, la, ab, ba, ra) ->
               Branch (R, la, ab, ba, rbt_joinR ra a b r)
             | Branch (B, la, ab, ba, ra) ->
               rbt_baliR la ab ba (rbt_joinR ra a b r)));;

let rec rbt_baliL
  x0 a b t4 = match x0, a, b, t4 with
    Branch (R, Branch (R, t1, ab, bb, t2), aa, ba, t3), a, b, t4 ->
      Branch (R, Branch (B, t1, ab, bb, t2), aa, ba, Branch (B, t3, a, b, t4))
    | Branch (R, Empty, ab, bb, Branch (R, t2, aa, ba, t3)), a, b, t4 ->
        Branch
          (R, Branch (B, Empty, ab, bb, t2), aa, ba, Branch (B, t3, a, b, t4))
    | Branch
        (R, Branch (B, va, vb, vc, vd), ab, bb, Branch (R, t2, aa, ba, t3)),
        a, b, t4
        -> Branch
             (R, Branch (B, Branch (B, va, vb, vc, vd), ab, bb, t2), aa, ba,
               Branch (B, t3, a, b, t4))
    | Empty, a, b, t2 -> Branch (B, Empty, a, b, t2)
    | Branch (B, va, vb, vc, vd), a, b, t2 ->
        Branch (B, Branch (B, va, vb, vc, vd), a, b, t2)
    | Branch (v, Empty, vb, vc, Empty), a, b, t2 ->
        Branch (B, Branch (v, Empty, vb, vc, Empty), a, b, t2)
    | Branch (v, Empty, vb, vc, Branch (B, ve, vf, vg, vh)), a, b, t2 ->
        Branch
          (B, Branch (v, Empty, vb, vc, Branch (B, ve, vf, vg, vh)), a, b, t2)
    | Branch (v, Branch (B, vf, vg, vh, vi), vb, vc, Empty), a, b, t2 ->
        Branch
          (B, Branch (v, Branch (B, vf, vg, vh, vi), vb, vc, Empty), a, b, t2)
    | Branch
        (v, Branch (B, vf, vg, vh, vi), vb, vc, Branch (B, ve, vj, vk, vl)),
        a, b, t2
        -> Branch
             (B, Branch
                   (v, Branch (B, vf, vg, vh, vi), vb, vc,
                     Branch (B, ve, vj, vk, vl)),
               a, b, t2);;

let rec rbt_joinL
  l a b r =
    (if less_eq_nat (bheight r) (bheight l) then Branch (R, l, a, b, r)
      else (match r
             with Branch (R, la, ab, ba, ra) ->
               Branch (R, rbt_joinL l a b la, ab, ba, ra)
             | Branch (B, la, ab, ba, ra) ->
               rbt_baliL (rbt_joinL l a b la) ab ba ra));;

let rec rbt_join
  l a b r =
    (let bhl = bheight l in
     let bhr = bheight r in
      (if less_nat bhr bhl then paint B (rbt_joinR l a b r)
        else (if less_nat bhl bhr then paint B (rbt_joinL l a b r)
               else Branch (B, l, a, b, r))));;

let rec rbt_split_comp
  c x1 k = match c, x1, k with c, Empty, k -> (Empty, (None, Empty))
    | c, Branch (uu, l, a, b, r), x ->
        (match c x a with Eq -> (l, (Some b, r))
          | Lt ->
            (let (l1, (beta, l2)) = rbt_split_comp c l x in
              (l1, (beta, rbt_join l2 a b r)))
          | Gt ->
            (let (r1, (beta, r2)) = rbt_split_comp c r x in
              (rbt_join l a b r1, (beta, r2))));;

let rec nat_of_integer k = Nat (max ord_integer Z.zero k);;

let rec folda
  f xa1 x = match f, xa1, x with
    f, Branch (c, lt, k, v, rt), x -> folda f rt (f k v (folda f lt x))
    | f, Empty, x -> x;;

let rec rbt_comp_union_swap_rec
  c f gamma t1 t2 =
    (let bh1 = bheight t1 in
     let bh2 = bheight t2 in
     let (gammaa, (t2a, (bh2a, (t1a, _)))) =
       (if less_nat bh1 bh2 then (not gamma, (t1, (bh1, (t2, bh2))))
         else (gamma, (t2, (bh2, (t1, bh1)))))
       in
     let fa = (if gammaa then (fun k v va -> f k va v) else f) in
      (if less_nat bh2a (nat_of_integer (Z.of_int 4))
        then folda (rbt_comp_insert_with_key c fa) t2a t1a
        else (match t1a with Empty -> t2a
               | Branch (_, l1, a, b, r1) ->
                 (let (l2, (beta, r2)) = rbt_split_comp c t2a a in
                   rbt_join (rbt_comp_union_swap_rec c f gammaa l1 l2) a
                     (match beta with None -> b | Some ca -> fa a b ca)
                     (rbt_comp_union_swap_rec c f gammaa r1 r2)))));;

let rec rbt_comp_union_with_key
  c f t1 t2 = paint B (rbt_comp_union_swap_rec c f false t1 t2);;

let rec join _A
  xc xd xe =
    Mapping_RBTa
      (rbt_comp_union_with_key (the (ccompare _A)) xc (impl_ofa _A xd)
        (impl_ofa _A xe));;

let rec list_insert
  equal x xs = (if list_member equal xs x then xs else x :: xs);;

let rec inserta _A
  xb xc = Abs_dlist (list_insert (the (ceq _A)) xb (list_of_dlist _A xc));;

let rec fold f x1 s = match f, x1, s with f, x :: xs, s -> fold f xs (f x s)
               | f, [], s -> s;;

let rec foldc _A x xc = fold x (list_of_dlist _A xc);;

let rec union _A = foldc _A (inserta _A);;

let rec memberc _A xa = list_member (the (ceq _A)) (list_of_dlist _A xa);;

let rec equal_option _A x0 x1 = match x0, x1 with None, Some x2 -> false
                          | Some x2, None -> false
                          | Some x2, Some y2 -> eq _A x2 y2
                          | None, None -> true;;

let rec rbt_comp_lookup
  c x1 k = match c, x1, k with c, Empty, k -> None
    | c, Branch (uu, l, x, y, r), k ->
        (match c k x with Eq -> Some y | Lt -> rbt_comp_lookup c l k
          | Gt -> rbt_comp_lookup c r k);;

let rec lookupb _A xa = rbt_comp_lookup (the (ccompare _A)) (impl_ofa _A xa);;

let rec equal_unita u v = true;;

let equal_unit = ({equal = equal_unita} : unit equal);;

let rec memberb _A t x = equal_option equal_unit (lookupb _A t x) (Some ());;

let rec member (_A1, _A2)
  x xa1 = match x, xa1 with
    x, Set_Monad xs ->
      (match ceq _A1
        with None ->
          failwith "member Set_Monad: ceq = None"
            (fun _ -> member (_A1, _A2) x (Set_Monad xs))
        | Some eq -> list_member eq xs x)
    | xa, Complement x -> not (member (_A1, _A2) xa x)
    | x, RBT_set rbt -> memberb _A2 rbt x
    | x, DList_set dxs -> memberc _A1 dxs x
    | x, Collect_set a -> a x;;

let rec id x = (fun xa -> xa) x;;

let rec fst (x1, x2) = x1;;

let rec is_none = function Some x -> false
                  | None -> true;;

let rec filter
  p x1 = match p, x1 with p, [] -> []
    | p, x :: xs -> (if p x then x :: filter p xs else filter p xs);;

let rec inter_list _A
  xb xc =
    Mapping_RBTa
      (fold (fun k -> rbt_comp_insert (the (ccompare _A)) k ())
        (filter
          (fun x ->
            not (is_none
                  (rbt_comp_lookup (the (ccompare _A)) (impl_ofa _A xb) x)))
          xc)
        Empty);;

let rec gen_length n x1 = match n, x1 with n, x :: xs -> gen_length (suc n) xs
                     | n, [] -> n;;

let rec size_list x = gen_length zero_nat x;;

let rec apfst f (x, y) = (f x, y);;

let rec map_prod f g (a, b) = (f a, g b);;

let rec divmod_nat
  m n = (let k = integer_of_nat m in
         let l = integer_of_nat n in
          map_prod nat_of_integer nat_of_integer
            (if Z.equal k Z.zero then (Z.zero, Z.zero)
              else (if Z.equal l Z.zero then (Z.zero, k)
                     else (fun k l -> if Z.equal Z.zero l then (Z.zero, l) else
                            Z.div_rem (Z.abs k) (Z.abs l))
                            k l)));;

let rec rbtreeify_g
  n kvs =
    (if equal_nata n zero_nat || equal_nata n one_nat then (Empty, kvs)
      else (let (na, r) = divmod_nat n (nat_of_integer (Z.of_int 2)) in
             (if equal_nata r zero_nat
               then (let (t1, (k, v) :: kvsa) = rbtreeify_g na kvs in
                      apfst (fun a -> Branch (B, t1, k, v, a))
                        (rbtreeify_g na kvsa))
               else (let (t1, (k, v) :: kvsa) = rbtreeify_f na kvs in
                      apfst (fun a -> Branch (B, t1, k, v, a))
                        (rbtreeify_g na kvsa)))))
and rbtreeify_f
  n kvs =
    (if equal_nata n zero_nat then (Empty, kvs)
      else (if equal_nata n one_nat
             then (let (k, v) :: kvsa = kvs in
                    (Branch (R, Empty, k, v, Empty), kvsa))
             else (let (na, r) = divmod_nat n (nat_of_integer (Z.of_int 2)) in
                    (if equal_nata r zero_nat
                      then (let (t1, (k, v) :: kvsa) = rbtreeify_f na kvs in
                             apfst (fun a -> Branch (B, t1, k, v, a))
                               (rbtreeify_g na kvsa))
                      else (let (t1, (k, v) :: kvsa) = rbtreeify_f na kvs in
                             apfst (fun a -> Branch (B, t1, k, v, a))
                               (rbtreeify_f na kvsa))))));;

let rec rbtreeify kvs = fst (rbtreeify_g (suc (size_list kvs)) kvs);;

let rec gen_entries
  kvts x1 = match kvts, x1 with
    kvts, Branch (c, l, k, v, r) -> gen_entries (((k, v), r) :: kvts) l
    | (kv, t) :: kvts, Empty -> kv :: gen_entries kvts t
    | [], Empty -> [];;

let rec entries x = gen_entries [] x;;

let rec filterd _A
  xb xc = Mapping_RBTa (rbtreeify (filter xb (entries (impl_ofa _A xc))));;

let rec map_filter
  f x1 = match f, x1 with f, [] -> []
    | f, x :: xs ->
        (match f x with None -> map_filter f xs
          | Some y -> y :: map_filter f xs);;

let rec map_filter_comp_inter
  c f t1 t2 =
    map_filter
      (fun (k, v) ->
        (match rbt_comp_lookup c t1 k with None -> None
          | Some va -> Some (k, f k va v)))
      (entries t2);;

let rec is_rbt_empty
  t = (match t with Empty -> true | Branch (_, _, _, _, _) -> false);;

let rec rbt_split_min
  = function Empty -> failwith "undefined"
    | Branch (uu, l, a, b, r) ->
        (if is_rbt_empty l then (a, (b, r))
          else (let (aa, (ba, la)) = rbt_split_min l in
                 (aa, (ba, rbt_join la a b r))));;

let rec rbt_join2
  l r = (if is_rbt_empty r then l
          else (let a = rbt_split_min r in
                let (aa, b) = a in
                let (ba, c) = b in
                 rbt_join l aa ba c));;

let rec rbt_comp_inter_swap_rec
  c f gamma t1 t2 =
    (let bh1 = bheight t1 in
     let bh2 = bheight t2 in
     let (gammaa, (t2a, (bh2a, (t1a, _)))) =
       (if less_nat bh1 bh2 then (not gamma, (t1, (bh1, (t2, bh2))))
         else (gamma, (t2, (bh2, (t1, bh1)))))
       in
     let fa = (if gammaa then (fun k v va -> f k va v) else f) in
      (if less_nat bh2a (nat_of_integer (Z.of_int 4))
        then rbtreeify (map_filter_comp_inter c fa t1a t2a)
        else (match t1a with Empty -> Empty
               | Branch (_, l1, a, b, r1) ->
                 (let (l2, (beta, r2)) = rbt_split_comp c t2a a in
                  let l = rbt_comp_inter_swap_rec c f gammaa l1 l2 in
                  let r = rbt_comp_inter_swap_rec c f gammaa r1 r2 in
                   (match beta with None -> rbt_join2 l r
                     | Some ba -> rbt_join l a (fa a b ba) r)))));;

let rec rbt_comp_inter_with_key
  c f t1 t2 = paint B (rbt_comp_inter_swap_rec c f false t1 t2);;

let rec meet _A
  xc xd xe =
    Mapping_RBTa
      (rbt_comp_inter_with_key (the (ccompare _A)) xc (impl_ofa _A xd)
        (impl_ofa _A xe));;

let rec filterc _A xb xc = Abs_dlist (filter xb (list_of_dlist _A xc));;

let rec comp f g = (fun x -> f (g x));;

let rec inf_seta (_A1, _A2)
  g ga = match g, ga with
    RBT_set rbt1, Set_Monad xs ->
      (match ccompare _A2
        with None ->
          failwith "inter RBT_set Set_Monad: ccompare = None"
            (fun _ -> inf_seta (_A1, _A2) (RBT_set rbt1) (Set_Monad xs))
        | Some _ -> RBT_set (inter_list _A2 rbt1 xs))
    | RBT_set rbt, DList_set dxs ->
        (match ccompare _A2
          with None ->
            failwith "inter RBT_set DList_set: ccompare = None"
              (fun _ -> inf_seta (_A1, _A2) (RBT_set rbt) (DList_set dxs))
          | Some _ ->
            (match ceq _A1
              with None ->
                failwith "inter RBT_set DList_set: ceq = None"
                  (fun _ -> inf_seta (_A1, _A2) (RBT_set rbt) (DList_set dxs))
              | Some _ -> RBT_set (inter_list _A2 rbt (list_of_dlist _A1 dxs))))
    | RBT_set rbt1, RBT_set rbt2 ->
        (match ccompare _A2
          with None ->
            failwith "inter RBT_set RBT_set: ccompare = None"
              (fun _ -> inf_seta (_A1, _A2) (RBT_set rbt1) (RBT_set rbt2))
          | Some _ -> RBT_set (meet _A2 (fun _ _ -> id) rbt1 rbt2))
    | DList_set dxs1, Set_Monad xs ->
        (match ceq _A1
          with None ->
            failwith "inter DList_set Set_Monad: ceq = None"
              (fun _ -> inf_seta (_A1, _A2) (DList_set dxs1) (Set_Monad xs))
          | Some eq -> DList_set (filterc _A1 (list_member eq xs) dxs1))
    | DList_set dxs1, DList_set dxs2 ->
        (match ceq _A1
          with None ->
            failwith "inter DList_set DList_set: ceq = None"
              (fun _ -> inf_seta (_A1, _A2) (DList_set dxs1) (DList_set dxs2))
          | Some _ -> DList_set (filterc _A1 (memberc _A1 dxs2) dxs1))
    | DList_set dxs, RBT_set rbt ->
        (match ccompare _A2
          with None ->
            failwith "inter DList_set RBT_set: ccompare = None"
              (fun _ -> inf_seta (_A1, _A2) (DList_set dxs) (RBT_set rbt))
          | Some _ ->
            (match ceq _A1
              with None ->
                failwith "inter DList_set RBT_set: ceq = None"
                  (fun _ -> inf_seta (_A1, _A2) (DList_set dxs) (RBT_set rbt))
              | Some _ -> RBT_set (inter_list _A2 rbt (list_of_dlist _A1 dxs))))
    | Set_Monad xs1, Set_Monad xs2 ->
        (match ceq _A1
          with None ->
            failwith "inter Set_Monad Set_Monad: ceq = None"
              (fun _ -> inf_seta (_A1, _A2) (Set_Monad xs1) (Set_Monad xs2))
          | Some eq -> Set_Monad (filter (list_member eq xs2) xs1))
    | Set_Monad xs, DList_set dxs2 ->
        (match ceq _A1
          with None ->
            failwith "inter Set_Monad DList_set: ceq = None"
              (fun _ -> inf_seta (_A1, _A2) (Set_Monad xs) (DList_set dxs2))
          | Some eq -> DList_set (filterc _A1 (list_member eq xs) dxs2))
    | Set_Monad xs, RBT_set rbt1 ->
        (match ccompare _A2
          with None ->
            failwith "inter Set_Monad RBT_set: ccompare = None"
              (fun _ -> inf_seta (_A1, _A2) (RBT_set rbt1) (Set_Monad xs))
          | Some _ -> RBT_set (inter_list _A2 rbt1 xs))
    | Complement ba, Complement b -> Complement (sup_seta (_A1, _A2) ba b)
    | g, RBT_set rbt2 ->
        (match ccompare _A2
          with None ->
            failwith "inter RBT_set2: ccompare = None"
              (fun _ -> inf_seta (_A1, _A2) g (RBT_set rbt2))
          | Some _ ->
            RBT_set
              (filterd _A2 (comp (fun x -> member (_A1, _A2) x g) fst) rbt2))
    | RBT_set rbt1, g ->
        (match ccompare _A2
          with None ->
            failwith "inter RBT_set1: ccompare = None"
              (fun _ -> inf_seta (_A1, _A2) (RBT_set rbt1) g)
          | Some _ ->
            RBT_set
              (filterd _A2 (comp (fun x -> member (_A1, _A2) x g) fst) rbt1))
    | h, DList_set dxs2 ->
        (match ceq _A1
          with None ->
            failwith "inter DList_set2: ceq = None"
              (fun _ -> inf_seta (_A1, _A2) h (DList_set dxs2))
          | Some _ ->
            DList_set (filterc _A1 (fun x -> member (_A1, _A2) x h) dxs2))
    | DList_set dxs1, h ->
        (match ceq _A1
          with None ->
            failwith "inter DList_set1: ceq = None"
              (fun _ -> inf_seta (_A1, _A2) (DList_set dxs1) h)
          | Some _ ->
            DList_set (filterc _A1 (fun x -> member (_A1, _A2) x h) dxs1))
    | i, Set_Monad xs -> Set_Monad (filter (fun x -> member (_A1, _A2) x i) xs)
    | Set_Monad xs, i -> Set_Monad (filter (fun x -> member (_A1, _A2) x i) xs)
    | j, Collect_set a -> Collect_set (fun x -> a x && member (_A1, _A2) x j)
    | Collect_set a, j -> Collect_set (fun x -> a x && member (_A1, _A2) x j)
and sup_seta (_A1, _A2)
  ba b = match ba, b with
    ba, Complement b -> Complement (inf_seta (_A1, _A2) (uminus_set ba) b)
    | Complement ba, b -> Complement (inf_seta (_A1, _A2) ba (uminus_set b))
    | b, Collect_set a -> Collect_set (fun x -> a x || member (_A1, _A2) x b)
    | Collect_set a, b -> Collect_set (fun x -> a x || member (_A1, _A2) x b)
    | Set_Monad xs, Set_Monad ys -> Set_Monad (xs @ ys)
    | DList_set dxs1, Set_Monad ws ->
        (match ceq _A1
          with None ->
            failwith "union DList_set Set_Monad: ceq = None"
              (fun _ -> sup_seta (_A1, _A2) (DList_set dxs1) (Set_Monad ws))
          | Some _ -> DList_set (fold (inserta _A1) ws dxs1))
    | Set_Monad ws, DList_set dxs2 ->
        (match ceq _A1
          with None ->
            failwith "union Set_Monad DList_set: ceq = None"
              (fun _ -> sup_seta (_A1, _A2) (Set_Monad ws) (DList_set dxs2))
          | Some _ -> DList_set (fold (inserta _A1) ws dxs2))
    | RBT_set rbt1, Set_Monad zs ->
        (match ccompare _A2
          with None ->
            failwith "union RBT_set Set_Monad: ccompare = None"
              (fun _ -> sup_seta (_A1, _A2) (RBT_set rbt1) (Set_Monad zs))
          | Some _ -> RBT_set (fold (fun k -> insertb _A2 k ()) zs rbt1))
    | Set_Monad zs, RBT_set rbt2 ->
        (match ccompare _A2
          with None ->
            failwith "union Set_Monad RBT_set: ccompare = None"
              (fun _ -> sup_seta (_A1, _A2) (Set_Monad zs) (RBT_set rbt2))
          | Some _ -> RBT_set (fold (fun k -> insertb _A2 k ()) zs rbt2))
    | DList_set dxs1, DList_set dxs2 ->
        (match ceq _A1
          with None ->
            failwith "union DList_set DList_set: ceq = None"
              (fun _ -> sup_seta (_A1, _A2) (DList_set dxs1) (DList_set dxs2))
          | Some _ -> DList_set (union _A1 dxs1 dxs2))
    | DList_set dxs, RBT_set rbt ->
        (match ccompare _A2
          with None ->
            failwith "union DList_set RBT_set: ccompare = None"
              (fun _ -> sup_seta (_A1, _A2) (RBT_set rbt) (DList_set dxs))
          | Some _ ->
            (match ceq _A1
              with None ->
                failwith "union DList_set RBT_set: ceq = None"
                  (fun _ -> sup_seta (_A1, _A2) (RBT_set rbt) (DList_set dxs))
              | Some _ ->
                RBT_set (foldc _A1 (fun k -> insertb _A2 k ()) dxs rbt)))
    | RBT_set rbt, DList_set dxs ->
        (match ccompare _A2
          with None ->
            failwith "union RBT_set DList_set: ccompare = None"
              (fun _ -> sup_seta (_A1, _A2) (RBT_set rbt) (DList_set dxs))
          | Some _ ->
            (match ceq _A1
              with None ->
                failwith "union RBT_set DList_set: ceq = None"
                  (fun _ -> sup_seta (_A1, _A2) (RBT_set rbt) (DList_set dxs))
              | Some _ ->
                RBT_set (foldc _A1 (fun k -> insertb _A2 k ()) dxs rbt)))
    | RBT_set rbt1, RBT_set rbt2 ->
        (match ccompare _A2
          with None ->
            failwith "union RBT_set RBT_set: ccompare = None"
              (fun _ -> sup_seta (_A1, _A2) (RBT_set rbt1) (RBT_set rbt2))
          | Some _ -> RBT_set (join _A2 (fun _ _ -> id) rbt1 rbt2));;

type 'a inf = {inf : 'a -> 'a -> 'a};;
let inf _A = _A.inf;;

let rec inf_set (_A1, _A2) = ({inf = inf_seta (_A1, _A2)} : 'a set inf);;

type 'a sup = {sup : 'a -> 'a -> 'a};;
let sup _A = _A.sup;;

let rec sup_set (_A1, _A2) = ({sup = sup_seta (_A1, _A2)} : 'a set sup);;

let rec equal_order x0 x1 = match x0, x1 with Lt, Gt -> false
                      | Gt, Lt -> false
                      | Eq, Gt -> false
                      | Gt, Eq -> false
                      | Eq, Lt -> false
                      | Lt, Eq -> false
                      | Gt, Gt -> true
                      | Lt, Lt -> true
                      | Eq, Eq -> true;;

type ('a, 'b) generator = Generator of (('b -> bool) * ('b -> 'a * 'b));;

let rec generator (Generator x) = x;;

let rec has_next g = fst (generator g);;

let rec snd (x1, x2) = x2;;

let rec next g = snd (generator g);;

let rec sorted_list_subset_fusion
  less eq g1 g2 s1 s2 =
    (if has_next g1 s1
      then (let (x, s1a) = next g1 s1 in
             has_next g2 s2 &&
               (let (y, s2a) = next g2 s2 in
                 (if eq x y then sorted_list_subset_fusion less eq g1 g2 s1a s2a
                   else less y x &&
                          sorted_list_subset_fusion less eq g1 g2 s1 s2a)))
      else true);;

let rec list_all_fusion
  g p s =
    (if has_next g s
      then (let (x, sa) = next g s in p x && list_all_fusion g p sa)
      else true);;

let rec rbt_keys_next
  = function ((k, t) :: kts, Empty) -> (k, (kts, t))
    | (kts, Branch (c, l, k, v, r)) -> rbt_keys_next ((k, r) :: kts, l);;

let rec rbt_has_next = function ([], Empty) -> false
                       | (vb :: vc, va) -> true
                       | (v, Branch (vb, vc, vd, ve, vf)) -> true;;

let rbt_keys_generator :
  ('a, (('a * ('a, 'b) rbt) list * ('a, 'b) rbt)) generator
  = Generator (rbt_has_next, rbt_keys_next);;

let rec lt_of_comp
  acomp x y = (match acomp x y with Eq -> false | Lt -> true | Gt -> false);;

let rec list_all p x1 = match p, x1 with p, [] -> true
                   | p, x :: xs -> p x && list_all p xs;;

let rec dlist_all _A x xc = list_all x (list_of_dlist _A xc);;

let rec rbt_init x = ([], x);;

let rec init _A xa = rbt_init (impl_ofa _A xa);;

let rec collect _A
  p = (match cEnum _A with None -> Collect_set p
        | Some (enum, _) -> Set_Monad (filter p enum));;

let rec less_eq_set (_A1, _A2, _A3)
  x0 c = match x0, c with
    RBT_set rbt1, RBT_set rbt2 ->
      (match ccompare _A3
        with None ->
          failwith "subset RBT_set RBT_set: ccompare = None"
            (fun _ -> less_eq_set (_A1, _A2, _A3) (RBT_set rbt1) (RBT_set rbt2))
        | Some c ->
          (match ceq _A2
            with None ->
              sorted_list_subset_fusion (lt_of_comp c)
                (fun x y -> equal_order (c x y) Eq) rbt_keys_generator
                rbt_keys_generator (init _A3 rbt1) (init _A3 rbt2)
            | Some eq ->
              sorted_list_subset_fusion (lt_of_comp c) eq rbt_keys_generator
                rbt_keys_generator (init _A3 rbt1) (init _A3 rbt2)))
    | Complement a1, Complement a2 -> less_eq_set (_A1, _A2, _A3) a2 a1
    | Collect_set p, Complement a ->
        less_eq_set (_A1, _A2, _A3) a (collect _A1 (fun x -> not (p x)))
    | Set_Monad xs, c -> list_all (fun x -> member (_A2, _A3) x c) xs
    | DList_set dxs, c ->
        (match ceq _A2
          with None ->
            failwith "subset DList_set1: ceq = None"
              (fun _ -> less_eq_set (_A1, _A2, _A3) (DList_set dxs) c)
          | Some _ -> dlist_all _A2 (fun x -> member (_A2, _A3) x c) dxs)
    | RBT_set rbt, b ->
        (match ccompare _A3
          with None ->
            failwith "subset RBT_set1: ccompare = None"
              (fun _ -> less_eq_set (_A1, _A2, _A3) (RBT_set rbt) b)
          | Some _ ->
            list_all_fusion rbt_keys_generator (fun x -> member (_A2, _A3) x b)
              (init _A3 rbt));;

let rec less_set (_A1, _A2, _A3)
  a b = less_eq_set (_A1, _A2, _A3) a b &&
          not (less_eq_set (_A1, _A2, _A3) b a);;

let rec ord_set (_A1, _A2, _A3) =
  ({less_eq = less_eq_set (_A1, _A2, _A3); less = less_set (_A1, _A2, _A3)} :
    'a set ord);;

let rec preorder_set (_A1, _A2, _A3) =
  ({ord_preorder = (ord_set (_A1, _A2, _A3))} : 'a set preorder);;

let rec order_set (_A1, _A2, _A3) =
  ({preorder_order = (preorder_set (_A1, _A2, _A3))} : 'a set order);;

type 'a semilattice_sup =
  {sup_semilattice_sup : 'a sup; order_semilattice_sup : 'a order};;

type 'a semilattice_inf =
  {inf_semilattice_inf : 'a inf; order_semilattice_inf : 'a order};;

type 'a lattice =
  {semilattice_inf_lattice : 'a semilattice_inf;
    semilattice_sup_lattice : 'a semilattice_sup};;

let rec semilattice_sup_set (_A1, _A2, _A3) =
  ({sup_semilattice_sup = (sup_set (_A2, _A3));
     order_semilattice_sup = (order_set (_A1, _A2, _A3))}
    : 'a set semilattice_sup);;

let rec semilattice_inf_set (_A1, _A2, _A3) =
  ({inf_semilattice_inf = (inf_set (_A2, _A3));
     order_semilattice_inf = (order_set (_A1, _A2, _A3))}
    : 'a set semilattice_inf);;

let rec lattice_set (_A1, _A2, _A3) =
  ({semilattice_inf_lattice = (semilattice_inf_set (_A1, _A2, _A3));
     semilattice_sup_lattice = (semilattice_sup_set (_A1, _A2, _A3))}
    : 'a set lattice);;

let rec list_all2_fusion
  p g1 g2 s1 s2 =
    (if has_next g1 s1
      then has_next g2 s2 &&
             (let (x, s1a) = next g1 s1 in
              let (y, s2a) = next g2 s2 in
               p x y && list_all2_fusion p g1 g2 s1a s2a)
      else not (has_next g2 s2));;

let rec set_eq (_A1, _A2, _A3)
  a b = match a, b with
    RBT_set rbt1, RBT_set rbt2 ->
      (match ccompare _A3
        with None ->
          failwith "set_eq RBT_set RBT_set: ccompare = None"
            (fun _ -> set_eq (_A1, _A2, _A3) (RBT_set rbt1) (RBT_set rbt2))
        | Some c ->
          (match ceq _A2
            with None ->
              list_all2_fusion (fun x y -> equal_order (c x y) Eq)
                rbt_keys_generator rbt_keys_generator (init _A3 rbt1)
                (init _A3 rbt2)
            | Some eq ->
              list_all2_fusion eq rbt_keys_generator rbt_keys_generator
                (init _A3 rbt1) (init _A3 rbt2)))
    | Complement a, Complement b -> set_eq (_A1, _A2, _A3) a b
    | a, b ->
        less_eq_set (_A1, _A2, _A3) a b && less_eq_set (_A1, _A2, _A3) b a;;

let rec ceq_seta (_A1, _A2, _A3)
  = (match ceq _A2 with None -> None
      | Some _ -> Some (set_eq (_A1, _A2, _A3)));;

let rec ceq_set (_A1, _A2, _A3) =
  ({ceq = ceq_seta (_A1, _A2, _A3)} : 'a set ceq);;

let set_impl_seta : ('a set, set_impla) phantom = Phantom Set_Choose;;

let set_impl_set = ({set_impl = set_impl_seta} : 'a set set_impl);;

let rec of_phantom (Phantom x) = x;;

let rec finite_UNIV_seta _A = Phantom (of_phantom (finite_UNIV _A));;

let rec finite_UNIV_set _A =
  ({finite_UNIV = finite_UNIV_seta _A} : 'a set finite_UNIV);;

let rec set_less_eq_aux_Compl_fusion
  less proper_interval g1 g2 ao s1 s2 =
    (if has_next g1 s1
      then (if has_next g2 s2
             then (let (x, s1a) = next g1 s1 in
                   let (y, s2a) = next g2 s2 in
                    (if less x y
                      then proper_interval ao (Some x) ||
                             set_less_eq_aux_Compl_fusion less proper_interval
                               g1 g2 (Some x) s1a s2
                      else (if less y x
                             then proper_interval ao (Some y) ||
                                    set_less_eq_aux_Compl_fusion less
                                      proper_interval g1 g2 (Some y) s1 s2a
                             else proper_interval ao (Some y))))
             else true)
      else true);;

let rec compl_set_less_eq_aux_fusion
  less proper_interval g1 g2 ao s1 s2 =
    (if has_next g1 s1
      then (let (x, s1a) = next g1 s1 in
             (if has_next g2 s2
               then (let (y, s2a) = next g2 s2 in
                      (if less x y
                        then not (proper_interval ao (Some x)) &&
                               compl_set_less_eq_aux_fusion less proper_interval
                                 g1 g2 (Some x) s1a s2
                        else (if less y x
                               then not (proper_interval ao (Some y)) &&
                                      compl_set_less_eq_aux_fusion less
proper_interval g1 g2 (Some y) s1 s2a
                               else not (proper_interval ao (Some y)))))
               else not (proper_interval ao (Some x)) &&
                      compl_set_less_eq_aux_fusion less proper_interval g1 g2
                        (Some x) s1a s2))
      else (if has_next g2 s2
             then (let (y, s2a) = next g2 s2 in
                    not (proper_interval ao (Some y)) &&
                      compl_set_less_eq_aux_fusion less proper_interval g1 g2
                        (Some y) s1 s2a)
             else not (proper_interval ao None)));;

let rec set_less_eq_aux_Compl
  less proper_interval ao xs ys = match less, proper_interval, ao, xs, ys with
    less, proper_interval, ao, x :: xs, y :: ys ->
      (if less x y
        then proper_interval ao (Some x) ||
               set_less_eq_aux_Compl less proper_interval (Some x) xs (y :: ys)
        else (if less y x
               then proper_interval ao (Some y) ||
                      set_less_eq_aux_Compl less proper_interval (Some y)
                        (x :: xs) ys
               else proper_interval ao (Some y)))
    | less, proper_interval, ao, xs, [] -> true
    | less, proper_interval, ao, [], ys -> true;;

let rec compl_set_less_eq_aux
  less proper_interval ao x3 x4 = match less, proper_interval, ao, x3, x4 with
    less, proper_interval, ao, x :: xs, y :: ys ->
      (if less x y
        then not (proper_interval ao (Some x)) &&
               compl_set_less_eq_aux less proper_interval (Some x) xs (y :: ys)
        else (if less y x
               then not (proper_interval ao (Some y)) &&
                      compl_set_less_eq_aux less proper_interval (Some y)
                        (x :: xs) ys
               else not (proper_interval ao (Some y))))
    | less, proper_interval, ao, x :: xs, [] ->
        not (proper_interval ao (Some x)) &&
          compl_set_less_eq_aux less proper_interval (Some x) xs []
    | less, proper_interval, ao, [], y :: ys ->
        not (proper_interval ao (Some y)) &&
          compl_set_less_eq_aux less proper_interval (Some y) [] ys
    | less, proper_interval, ao, [], [] -> not (proper_interval ao None);;

let rec lexord_eq_fusion
  less g1 g2 s1 s2 =
    (if has_next g1 s1
      then has_next g2 s2 &&
             (let (x, s1a) = next g1 s1 in
              let (y, s2a) = next g2 s2 in
               less x y ||
                 not (less y x) && lexord_eq_fusion less g1 g2 s1a s2a)
      else true);;

let rec remdups_sorted
  less x1 = match less, x1 with
    less, x :: y :: xs ->
      (if less x y then x :: remdups_sorted less (y :: xs)
        else remdups_sorted less (y :: xs))
    | less, [x] -> [x]
    | less, [] -> [];;

let rec quicksort_acc
  less ac x2 = match less, ac, x2 with
    less, ac, x :: v :: va -> quicksort_part less ac x [] [] [] (v :: va)
    | less, ac, [x] -> x :: ac
    | less, ac, [] -> ac
and quicksort_part
  less ac x lts eqs gts xa6 = match less, ac, x, lts, eqs, gts, xa6 with
    less, ac, x, lts, eqs, gts, z :: zs ->
      (if less x z then quicksort_part less ac x lts eqs (z :: gts) zs
        else (if less z x then quicksort_part less ac x (z :: lts) eqs gts zs
               else quicksort_part less ac x lts (z :: eqs) gts zs))
    | less, ac, x, lts, eqs, gts, [] ->
        quicksort_acc less (eqs @ x :: quicksort_acc less ac gts) lts;;

let rec quicksort less = quicksort_acc less [];;

let rec gen_keys
  kts x1 = match kts, x1 with
    kts, Branch (c, l, k, v, r) -> gen_keys ((k, r) :: kts) l
    | (k, t) :: kts, Empty -> k :: gen_keys kts t
    | [], Empty -> [];;

let rec keysa x = gen_keys [] x;;

let rec keysb _A xa = keysa (impl_ofa _A xa);;

let rec csorted_list_of_set (_A1, _A2)
  = function
    Set_Monad xs ->
      (match ccompare _A2
        with None ->
          failwith "csorted_list_of_set Set_Monad: ccompare = None"
            (fun _ -> csorted_list_of_set (_A1, _A2) (Set_Monad xs))
        | Some c -> remdups_sorted (lt_of_comp c) (quicksort (lt_of_comp c) xs))
    | DList_set dxs ->
        (match ceq _A1
          with None ->
            failwith "csorted_list_of_set DList_set: ceq = None"
              (fun _ -> csorted_list_of_set (_A1, _A2) (DList_set dxs))
          | Some _ ->
            (match ccompare _A2
              with None ->
                failwith "csorted_list_of_set DList_set: ccompare = None"
                  (fun _ -> csorted_list_of_set (_A1, _A2) (DList_set dxs))
              | Some c -> quicksort (lt_of_comp c) (list_of_dlist _A1 dxs)))
    | RBT_set rbt ->
        (match ccompare _A2
          with None ->
            failwith "csorted_list_of_set RBT_set: ccompare = None"
              (fun _ -> csorted_list_of_set (_A1, _A2) (RBT_set rbt))
          | Some _ -> keysb _A2 rbt);;

let rec emptyc _A = Mapping_RBTa Empty;;

let rec emptyb _A = Abs_dlist [];;

let rec set_empty_choose (_A1, _A2)
  = (match ccompare _A2
      with None ->
        (match ceq _A1 with None -> Set_Monad []
          | Some _ -> DList_set (emptyb _A1))
      | Some _ -> RBT_set (emptyc _A2));;

let rec set_empty (_A1, _A2)
  = function Set_Choose -> set_empty_choose (_A1, _A2)
    | Set_Monada -> Set_Monad []
    | Set_RBT -> RBT_set (emptyc _A2)
    | Set_DList -> DList_set (emptyb _A1)
    | Set_Collect -> Collect_set (fun _ -> false);;

let rec bot_set (_A1, _A2, _A3)
  = set_empty (_A1, _A2) (of_phantom (set_impl _A3));;

let rec top_set (_A1, _A2, _A3) = uminus_set (bot_set (_A1, _A2, _A3));;

let rec le_of_comp
  acomp x y = (match acomp x y with Eq -> true | Lt -> true | Gt -> false);;

let rec null = function [] -> true
               | x :: xs -> false;;

let rec lexordp_eq
  less xs ys = match less, xs, ys with
    less, x :: xs, y :: ys ->
      less x y || not (less y x) && lexordp_eq less xs ys
    | less, x :: xs, [] -> false
    | less, xs, [] -> null xs
    | less, [], ys -> true;;

let rec finite (_A1, _A2, _A3)
  = function
    Collect_set p ->
      of_phantom (finite_UNIV _A1) ||
        failwith "finite Collect_set"
          (fun _ -> finite (_A1, _A2, _A3) (Collect_set p))
    | Set_Monad xs -> true
    | Complement a ->
        (if of_phantom (finite_UNIV _A1) then true
          else (if finite (_A1, _A2, _A3) a then false
                 else failwith "finite Complement: infinite set"
                        (fun _ -> finite (_A1, _A2, _A3) (Complement a))))
    | RBT_set rbt ->
        (match ccompare _A3
          with None ->
            failwith "finite RBT_set: ccompare = None"
              (fun _ -> finite (_A1, _A2, _A3) (RBT_set rbt))
          | Some _ -> true)
    | DList_set dxs ->
        (match ceq _A2
          with None ->
            failwith "finite DList_set: ceq = None"
              (fun _ -> finite (_A1, _A2, _A3) (DList_set dxs))
          | Some _ -> true);;

let rec set_less_aux_Compl_fusion
  less proper_interval g1 g2 ao s1 s2 =
    (if has_next g1 s1
      then (let (x, s1a) = next g1 s1 in
             (if has_next g2 s2
               then (let (y, s2a) = next g2 s2 in
                      (if less x y
                        then proper_interval ao (Some x) ||
                               set_less_aux_Compl_fusion less proper_interval g1
                                 g2 (Some x) s1a s2
                        else (if less y x
                               then proper_interval ao (Some y) ||
                                      set_less_aux_Compl_fusion less
proper_interval g1 g2 (Some y) s1 s2a
                               else proper_interval ao (Some y))))
               else proper_interval ao (Some x) ||
                      set_less_aux_Compl_fusion less proper_interval g1 g2
                        (Some x) s1a s2))
      else (if has_next g2 s2
             then (let (y, s2a) = next g2 s2 in
                    proper_interval ao (Some y) ||
                      set_less_aux_Compl_fusion less proper_interval g1 g2
                        (Some y) s1 s2a)
             else proper_interval ao None));;

let rec compl_set_less_aux_fusion
  less proper_interval g1 g2 ao s1 s2 =
    has_next g1 s1 &&
      (has_next g2 s2 &&
        (let (x, s1a) = next g1 s1 in
         let (y, s2a) = next g2 s2 in
          (if less x y
            then not (proper_interval ao (Some x)) &&
                   compl_set_less_aux_fusion less proper_interval g1 g2 (Some x)
                     s1a s2
            else (if less y x
                   then not (proper_interval ao (Some y)) &&
                          compl_set_less_aux_fusion less proper_interval g1 g2
                            (Some y) s1 s2a
                   else not (proper_interval ao (Some y))))));;

let rec set_less_aux_Compl
  less proper_interval ao x3 x4 = match less, proper_interval, ao, x3, x4 with
    less, proper_interval, ao, x :: xs, y :: ys ->
      (if less x y
        then proper_interval ao (Some x) ||
               set_less_aux_Compl less proper_interval (Some x) xs (y :: ys)
        else (if less y x
               then proper_interval ao (Some y) ||
                      set_less_aux_Compl less proper_interval (Some y) (x :: xs)
                        ys
               else proper_interval ao (Some y)))
    | less, proper_interval, ao, x :: xs, [] ->
        proper_interval ao (Some x) ||
          set_less_aux_Compl less proper_interval (Some x) xs []
    | less, proper_interval, ao, [], y :: ys ->
        proper_interval ao (Some y) ||
          set_less_aux_Compl less proper_interval (Some y) [] ys
    | less, proper_interval, ao, [], [] -> proper_interval ao None;;

let rec compl_set_less_aux
  less proper_interval ao xs ys = match less, proper_interval, ao, xs, ys with
    less, proper_interval, ao, x :: xs, y :: ys ->
      (if less x y
        then not (proper_interval ao (Some x)) &&
               compl_set_less_aux less proper_interval (Some x) xs (y :: ys)
        else (if less y x
               then not (proper_interval ao (Some y)) &&
                      compl_set_less_aux less proper_interval (Some y) (x :: xs)
                        ys
               else not (proper_interval ao (Some y))))
    | less, proper_interval, ao, xs, [] -> false
    | less, proper_interval, ao, [], ys -> false;;

let rec lexord_fusion
  less g1 g2 s1 s2 =
    (if has_next g1 s1
      then (if has_next g2 s2
             then (let (x, s1a) = next g1 s1 in
                   let (y, s2a) = next g2 s2 in
                    less x y ||
                      not (less y x) && lexord_fusion less g1 g2 s1a s2a)
             else false)
      else has_next g2 s2);;

let rec lexordp
  less xs ys = match less, xs, ys with
    less, x :: xs, y :: ys -> less x y || not (less y x) && lexordp less xs ys
    | less, xs, [] -> false
    | less, [], ys -> not (null ys);;

let rec comp_of_ords
  le lt x y = (if lt x y then Lt else (if le x y then Eq else Gt));;

let rec ccompare_seta (_A1, _A2, _A3, _A4)
  = (match ccompare _A3.ccompare_cproper_interval with None -> None
      | Some _ ->
        Some (comp_of_ords (cless_eq_set (_A1, _A2, _A3, _A4))
               (cless_set (_A1, _A2, _A3, _A4))))
and cless_set (_A1, _A2, _A3, _A4)
  a b = match a, b with
    Complement (RBT_set rbt1), RBT_set rbt2 ->
      (match ccompare _A3.ccompare_cproper_interval
        with None ->
          failwith "cless_set (Complement RBT_set) RBT_set: ccompare = None"
            (fun _ ->
              cless_set (_A1, _A2, _A3, _A4) (Complement (RBT_set rbt1))
                (RBT_set rbt2))
        | Some c ->
          finite (_A1, _A2, _A3.ccompare_cproper_interval)
            (top_set (_A2, _A3.ccompare_cproper_interval, _A4)) &&
            compl_set_less_aux_fusion (lt_of_comp c) (cproper_interval _A3)
              rbt_keys_generator rbt_keys_generator None
              (init _A3.ccompare_cproper_interval rbt1)
              (init _A3.ccompare_cproper_interval rbt2))
    | RBT_set rbt1, Complement (RBT_set rbt2) ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_set RBT_set (Complement RBT_set): ccompare = None"
              (fun _ ->
                cless_set (_A1, _A2, _A3, _A4) (RBT_set rbt1)
                  (Complement (RBT_set rbt2)))
          | Some c ->
            (if finite (_A1, _A2, _A3.ccompare_cproper_interval)
                  (top_set (_A2, _A3.ccompare_cproper_interval, _A4))
              then set_less_aux_Compl_fusion (lt_of_comp c)
                     (cproper_interval _A3) rbt_keys_generator
                     rbt_keys_generator None
                     (init _A3.ccompare_cproper_interval rbt1)
                     (init _A3.ccompare_cproper_interval rbt2)
              else true))
    | RBT_set rbta, RBT_set rbt ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_set RBT_set RBT_set: ccompare = None"
              (fun _ ->
                cless_set (_A1, _A2, _A3, _A4) (RBT_set rbta) (RBT_set rbt))
          | Some c ->
            lexord_fusion (fun x y -> lt_of_comp c y x) rbt_keys_generator
              rbt_keys_generator (init _A3.ccompare_cproper_interval rbta)
              (init _A3.ccompare_cproper_interval rbt))
    | Complement a, Complement b ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_set Complement Complement: ccompare = None"
              (fun _ ->
                cless_set (_A1, _A2, _A3, _A4) (Complement a) (Complement b))
          | Some _ -> lt_of_comp (the (ccompare_seta (_A1, _A2, _A3, _A4))) b a)
    | Complement a, b ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_set Complement1: ccompare = None"
              (fun _ -> cless_set (_A1, _A2, _A3, _A4) (Complement a) b)
          | Some c ->
            (if finite (_A1, _A2, _A3.ccompare_cproper_interval) a &&
                  finite (_A1, _A2, _A3.ccompare_cproper_interval) b
              then finite (_A1, _A2, _A3.ccompare_cproper_interval)
                     (top_set (_A2, _A3.ccompare_cproper_interval, _A4)) &&
                     compl_set_less_aux (lt_of_comp c) (cproper_interval _A3)
                       None
                       (csorted_list_of_set (_A2, _A3.ccompare_cproper_interval)
                         a)
                       (csorted_list_of_set (_A2, _A3.ccompare_cproper_interval)
                         b)
              else failwith "cless_set Complement1: infinite set"
                     (fun _ ->
                       cless_set (_A1, _A2, _A3, _A4) (Complement a) b)))
    | a, Complement b ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_set Complement2: ccompare = None"
              (fun _ -> cless_set (_A1, _A2, _A3, _A4) a (Complement b))
          | Some c ->
            (if finite (_A1, _A2, _A3.ccompare_cproper_interval) a &&
                  finite (_A1, _A2, _A3.ccompare_cproper_interval) b
              then (if finite (_A1, _A2, _A3.ccompare_cproper_interval)
                         (top_set (_A2, _A3.ccompare_cproper_interval, _A4))
                     then set_less_aux_Compl (lt_of_comp c)
                            (cproper_interval _A3) None
                            (csorted_list_of_set
                              (_A2, _A3.ccompare_cproper_interval) a)
                            (csorted_list_of_set
                              (_A2, _A3.ccompare_cproper_interval) b)
                     else true)
              else failwith "cless_set Complement2: infinite set"
                     (fun _ ->
                       cless_set (_A1, _A2, _A3, _A4) a (Complement b))))
    | a, b ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_set: ccompare = None"
              (fun _ -> cless_set (_A1, _A2, _A3, _A4) a b)
          | Some c ->
            (if finite (_A1, _A2, _A3.ccompare_cproper_interval) a &&
                  finite (_A1, _A2, _A3.ccompare_cproper_interval) b
              then lexordp (fun x y -> lt_of_comp c y x)
                     (csorted_list_of_set (_A2, _A3.ccompare_cproper_interval)
                       a)
                     (csorted_list_of_set (_A2, _A3.ccompare_cproper_interval)
                       b)
              else failwith "cless_set: infinite set"
                     (fun _ -> cless_set (_A1, _A2, _A3, _A4) a b)))
and cless_eq_set (_A1, _A2, _A3, _A4)
  a b = match a, b with
    Complement (RBT_set rbt1), RBT_set rbt2 ->
      (match ccompare _A3.ccompare_cproper_interval
        with None ->
          failwith "cless_eq_set (Complement RBT_set) RBT_set: ccompare = None"
            (fun _ ->
              cless_eq_set (_A1, _A2, _A3, _A4) (Complement (RBT_set rbt1))
                (RBT_set rbt2))
        | Some c ->
          finite (_A1, _A2, _A3.ccompare_cproper_interval)
            (top_set (_A2, _A3.ccompare_cproper_interval, _A4)) &&
            compl_set_less_eq_aux_fusion (lt_of_comp c) (cproper_interval _A3)
              rbt_keys_generator rbt_keys_generator None
              (init _A3.ccompare_cproper_interval rbt1)
              (init _A3.ccompare_cproper_interval rbt2))
    | RBT_set rbt1, Complement (RBT_set rbt2) ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith
              "cless_eq_set RBT_set (Complement RBT_set): ccompare = None"
              (fun _ ->
                cless_eq_set (_A1, _A2, _A3, _A4) (RBT_set rbt1)
                  (Complement (RBT_set rbt2)))
          | Some c ->
            (if finite (_A1, _A2, _A3.ccompare_cproper_interval)
                  (top_set (_A2, _A3.ccompare_cproper_interval, _A4))
              then set_less_eq_aux_Compl_fusion (lt_of_comp c)
                     (cproper_interval _A3) rbt_keys_generator
                     rbt_keys_generator None
                     (init _A3.ccompare_cproper_interval rbt1)
                     (init _A3.ccompare_cproper_interval rbt2)
              else true))
    | RBT_set rbta, RBT_set rbt ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_eq_set RBT_set RBT_set: ccompare = None"
              (fun _ ->
                cless_eq_set (_A1, _A2, _A3, _A4) (RBT_set rbta) (RBT_set rbt))
          | Some c ->
            lexord_eq_fusion (fun x y -> lt_of_comp c y x) rbt_keys_generator
              rbt_keys_generator (init _A3.ccompare_cproper_interval rbta)
              (init _A3.ccompare_cproper_interval rbt))
    | Complement a, Complement b ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_eq_set Complement Complement: ccompare = None"
              (fun _ ->
                le_of_comp (the (ccompare_seta (_A1, _A2, _A3, _A4)))
                  (Complement a) (Complement b))
          | Some _ -> cless_eq_set (_A1, _A2, _A3, _A4) b a)
    | Complement a, b ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_eq_set Complement1: ccompare = None"
              (fun _ -> cless_eq_set (_A1, _A2, _A3, _A4) (Complement a) b)
          | Some c ->
            (if finite (_A1, _A2, _A3.ccompare_cproper_interval) a &&
                  finite (_A1, _A2, _A3.ccompare_cproper_interval) b
              then finite (_A1, _A2, _A3.ccompare_cproper_interval)
                     (top_set (_A2, _A3.ccompare_cproper_interval, _A4)) &&
                     compl_set_less_eq_aux (lt_of_comp c) (cproper_interval _A3)
                       None
                       (csorted_list_of_set (_A2, _A3.ccompare_cproper_interval)
                         a)
                       (csorted_list_of_set (_A2, _A3.ccompare_cproper_interval)
                         b)
              else failwith "cless_eq_set Complement1: infinite set"
                     (fun _ ->
                       cless_eq_set (_A1, _A2, _A3, _A4) (Complement a) b)))
    | a, Complement b ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_eq_set Complement2: ccompare = None"
              (fun _ -> cless_eq_set (_A1, _A2, _A3, _A4) a (Complement b))
          | Some c ->
            (if finite (_A1, _A2, _A3.ccompare_cproper_interval) a &&
                  finite (_A1, _A2, _A3.ccompare_cproper_interval) b
              then (if finite (_A1, _A2, _A3.ccompare_cproper_interval)
                         (top_set (_A2, _A3.ccompare_cproper_interval, _A4))
                     then set_less_eq_aux_Compl (lt_of_comp c)
                            (cproper_interval _A3) None
                            (csorted_list_of_set
                              (_A2, _A3.ccompare_cproper_interval) a)
                            (csorted_list_of_set
                              (_A2, _A3.ccompare_cproper_interval) b)
                     else true)
              else failwith "cless_eq_set Complement2: infinite set"
                     (fun _ ->
                       cless_eq_set (_A1, _A2, _A3, _A4) a (Complement b))))
    | a, b ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "cless_eq_set: ccompare = None"
              (fun _ -> cless_eq_set (_A1, _A2, _A3, _A4) a b)
          | Some c ->
            (if finite (_A1, _A2, _A3.ccompare_cproper_interval) a &&
                  finite (_A1, _A2, _A3.ccompare_cproper_interval) b
              then lexordp_eq (fun x y -> lt_of_comp c y x)
                     (csorted_list_of_set (_A2, _A3.ccompare_cproper_interval)
                       a)
                     (csorted_list_of_set (_A2, _A3.ccompare_cproper_interval)
                       b)
              else failwith "cless_eq_set: infinite set"
                     (fun _ -> cless_eq_set (_A1, _A2, _A3, _A4) a b)));;

let rec ccompare_set (_A1, _A2, _A3, _A4) =
  ({ccompare = ccompare_seta (_A1, _A2, _A3, _A4)} : 'a set ccompare);;

let rec equal_lista _A
  x0 x1 = match x0, x1 with [], x21 :: x22 -> false
    | x21 :: x22, [] -> false
    | x21 :: x22, y21 :: y22 -> eq _A x21 y21 && equal_lista _A x22 y22
    | [], [] -> true;;

let rec equal_list _A = ({equal = equal_lista _A} : ('a list) equal);;

let rec equality_list
  eq_a x1 x2 = match eq_a, x1, x2 with
    eq_a, x :: xa, y :: ya -> eq_a x y && equality_list eq_a xa ya
    | eq_a, x :: xa, [] -> false
    | eq_a, [], y :: ya -> false
    | eq_a, [], [] -> true;;

let rec ceq_lista _A
  = (match ceq _A with None -> None | Some eq_a -> Some (equality_list eq_a));;

let rec ceq_list _A = ({ceq = ceq_lista _A} : ('a list) ceq);;

let set_impl_lista : (('a list), set_impla) phantom = Phantom Set_Choose;;

let set_impl_list = ({set_impl = set_impl_lista} : ('a list) set_impl);;

let finite_UNIV_lista : (('a list), bool) phantom = Phantom false;;

let card_UNIV_lista : (('a list), nat) phantom = Phantom zero_nat;;

let finite_UNIV_list =
  ({finite_UNIV = finite_UNIV_lista} : ('a list) finite_UNIV);;

let card_UNIV_list =
  ({finite_UNIV_card_UNIV = finite_UNIV_list; card_UNIV = card_UNIV_lista} :
    ('a list) card_UNIV);;

let cEnum_list :
  (('a list) list *
    ((('a list -> bool) -> bool) * (('a list -> bool) -> bool))) option
  = None;;

let cenum_list = ({cEnum = cEnum_list} : ('a list) cenum);;

let rec comparator_list
  comp_a x1 x2 = match comp_a, x1, x2 with
    comp_a, x :: xa, y :: ya ->
      (match comp_a x y with Eq -> comparator_list comp_a xa ya | Lt -> Lt
        | Gt -> Gt)
    | comp_a, x :: xa, [] -> Gt
    | comp_a, [], y :: ya -> Lt
    | comp_a, [], [] -> Eq;;

let rec ccompare_lista _A
  = (match ccompare _A with None -> None
      | Some comp_a -> Some (comparator_list comp_a));;

let rec ccompare_list _A =
  ({ccompare = ccompare_lista _A} : ('a list) ccompare);;

let rec cproper_interval_lista _A xso yso = failwith "undefined";;

let rec cproper_interval_list _A =
  ({ccompare_cproper_interval = (ccompare_list _A);
     cproper_interval = cproper_interval_lista _A}
    : ('a list) cproper_interval);;

type ('a, 'b) sum = Inl of 'a | Inr of 'b;;

let rec equal_suma _A _B x0 x1 = match x0, x1 with Inl x1, Inr x2 -> false
                           | Inr x2, Inl x1 -> false
                           | Inr x2, Inr y2 -> eq _B x2 y2
                           | Inl x1, Inl y1 -> eq _A x1 y1;;

let rec equal_sum _A _B = ({equal = equal_suma _A _B} : ('a, 'b) sum equal);;

let rec equality_sum
  eq_a eq_b x2 x3 = match eq_a, eq_b, x2, x3 with
    eq_a, eq_b, Inr x, Inr ya -> eq_b x ya
    | eq_a, eq_b, Inr x, Inl y -> false
    | eq_a, eq_b, Inl x, Inr ya -> false
    | eq_a, eq_b, Inl x, Inl y -> eq_a x y;;

let rec ceq_suma _A _B
  = (match ceq _A with None -> None
      | Some eq_a ->
        (match ceq _B with None -> None
          | Some eq_b -> Some (equality_sum eq_a eq_b)));;

let rec ceq_sum _A _B = ({ceq = ceq_suma _A _B} : ('a, 'b) sum ceq);;

let rec set_impl_choose2
  x y = match x, y with Set_Monada, Set_Monada -> Set_Monada
    | Set_RBT, Set_RBT -> Set_RBT
    | Set_DList, Set_DList -> Set_DList
    | Set_Collect, Set_Collect -> Set_Collect
    | x, y -> Set_Choose;;

let rec set_impl_suma _A _B
  = Phantom
      (set_impl_choose2 (of_phantom (set_impl _A)) (of_phantom (set_impl _B)));;

let rec set_impl_sum _A _B =
  ({set_impl = set_impl_suma _A _B} : ('a, 'b) sum set_impl);;

let rec mapa f x1 = match f, x1 with f, [] -> []
               | f, x21 :: x22 -> f x21 :: mapa f x22;;

let rec cEnum_sum _A _B
  = (match cEnum _A with None -> None
      | Some (enum_a, (enum_all_a, enum_ex_a)) ->
        (match cEnum _B with None -> None
          | Some (enum_b, (enum_all_b, enum_ex_b)) ->
            Some (mapa (fun a -> Inl a) enum_a @ mapa (fun a -> Inr a) enum_b,
                   ((fun p ->
                      enum_all_a (fun x -> p (Inl x)) &&
                        enum_all_b (fun x -> p (Inr x))),
                     (fun p ->
                       enum_ex_a (fun x -> p (Inl x)) ||
                         enum_ex_b (fun x -> p (Inr x)))))));;

let rec cenum_sum _A _B = ({cEnum = cEnum_sum _A _B} : ('a, 'b) sum cenum);;

let rec finite_UNIV_suma _A _B
  = Phantom (of_phantom (finite_UNIV _A) && of_phantom (finite_UNIV _B));;

let rec finite_UNIV_sum _A _B =
  ({finite_UNIV = finite_UNIV_suma _A _B} : ('a, 'b) sum finite_UNIV);;

let rec comparator_sum
  comp_a comp_b x2 x3 = match comp_a, comp_b, x2, x3 with
    comp_a, comp_b, Inr x, Inr ya -> comp_b x ya
    | comp_a, comp_b, Inr x, Inl y -> Gt
    | comp_a, comp_b, Inl x, Inr ya -> Lt
    | comp_a, comp_b, Inl x, Inl y -> comp_a x y;;

let rec ccompare_suma _A _B
  = (match ccompare _A with None -> None
      | Some comp_a ->
        (match ccompare _B with None -> None
          | Some comp_b -> Some (comparator_sum comp_a comp_b)));;

let rec ccompare_sum _A _B =
  ({ccompare = ccompare_suma _A _B} : ('a, 'b) sum ccompare);;

let rec cproper_interval_suma _A _B
  x0 x1 = match x0, x1 with None, None -> true
    | None, Some (Inl x) -> cproper_interval _A None (Some x)
    | None, Some (Inr y) -> true
    | Some (Inl x), None -> true
    | Some (Inl x), Some (Inl y) -> cproper_interval _A (Some x) (Some y)
    | Some (Inl x), Some (Inr y) ->
        cproper_interval _A (Some x) None || cproper_interval _B None (Some y)
    | Some (Inr y), None -> cproper_interval _B (Some y) None
    | Some (Inr y), Some (Inl x) -> false
    | Some (Inr x), Some (Inr y) -> cproper_interval _B (Some x) (Some y);;

let rec cproper_interval_sum _A _B =
  ({ccompare_cproper_interval =
      (ccompare_sum _A.ccompare_cproper_interval _B.ccompare_cproper_interval);
     cproper_interval = cproper_interval_suma _A _B}
    : ('a, 'b) sum cproper_interval);;

let equal_literal = ({equal = (fun a b -> ((a : string) = b))} : string equal);;

let ord_literal =
  ({less_eq = (fun a b -> ((a : string) <= b));
     less = (fun a b -> ((a : string) < b))}
    : string ord);;

let preorder_literal = ({ord_preorder = ord_literal} : string preorder);;

let order_literal = ({preorder_order = preorder_literal} : string order);;

let linorder_literal = ({order_linorder = order_literal} : string linorder);;

let rec compare_literal x = comparator_of (equal_literal, linorder_literal) x;;

let ccompare_literala : (string -> string -> ordera) option
  = Some compare_literal;;

let ccompare_literal = ({ccompare = ccompare_literala} : string ccompare);;

let mapping_impl_literala : (string, mapping_impla) phantom
  = Phantom Mapping_RBT;;

let mapping_impl_literal =
  ({mapping_impl = mapping_impl_literala} : string mapping_impl);;

let rec equal_proda _A _B (x1, x2) (y1, y2) = eq _A x1 y1 && eq _B x2 y2;;

let rec equal_prod _A _B = ({equal = equal_proda _A _B} : ('a * 'b) equal);;

let rec equality_prod eq_a eq_b (x, xa) (y, ya) = eq_a x y && eq_b xa ya;;

let rec ceq_proda _A _B
  = (match ceq _A with None -> None
      | Some eq_a ->
        (match ceq _B with None -> None
          | Some eq_b -> Some (equality_prod eq_a eq_b)));;

let rec ceq_prod _A _B = ({ceq = ceq_proda _A _B} : ('a * 'b) ceq);;

let rec set_impl_proda _A _B
  = Phantom
      (set_impl_choose2 (of_phantom (set_impl _A)) (of_phantom (set_impl _B)));;

let rec set_impl_prod _A _B =
  ({set_impl = set_impl_proda _A _B} : ('a * 'b) set_impl);;

let rec comparator_prod
  comp_a comp_b (x, xa) (y, ya) =
    (match comp_a x y with Eq -> comp_b xa ya | Lt -> Lt | Gt -> Gt);;

let rec ccompare_proda _A _B
  = (match ccompare _A with None -> None
      | Some comp_a ->
        (match ccompare _B with None -> None
          | Some comp_b -> Some (comparator_prod comp_a comp_b)));;

let rec ccompare_prod _A _B =
  ({ccompare = ccompare_proda _A _B} : ('a * 'b) ccompare);;

let rec mapping_impl_choose2
  x y = match x, y with Mapping_RBT, Mapping_RBT -> Mapping_RBT
    | Mapping_Assoc_List, Mapping_Assoc_List -> Mapping_Assoc_List
    | Mapping_Mapping, Mapping_Mapping -> Mapping_Mapping
    | x, y -> Mapping_Choose;;

let rec mapping_impl_proda _A _B
  = Phantom
      (mapping_impl_choose2 (of_phantom (mapping_impl _A))
        (of_phantom (mapping_impl _B)));;

let rec mapping_impl_prod _A _B =
  ({mapping_impl = mapping_impl_proda _A _B} : ('a * 'b) mapping_impl);;

type 'a fo_term = Const of 'a | Var of nat;;

type ('a, 'b) fo_fmla = Pred of 'b * 'a fo_term list | Bool of bool |
  Eqa of 'a fo_term * 'a fo_term | Neg of ('a, 'b) fo_fmla |
  Conj of ('a, 'b) fo_fmla * ('a, 'b) fo_fmla |
  Disj of ('a, 'b) fo_fmla * ('a, 'b) fo_fmla | Exists of nat * ('a, 'b) fo_fmla
  | Forall of nat * ('a, 'b) fo_fmla;;

type ('b, 'a) alist = Alist of ('b * 'a) list;;

type ('a, 'b) mapping = Assoc_List_Mapping of ('a, 'b) alist |
  RBT_Mapping of ('a, 'b) mapping_rbt | Mapping of ('a -> 'b option);;

type 'a eval_res = Fin of ('a list) set | Infin | Wf_error;;

type ('b, 'a) comp_fun_idem = Abs_comp_fun_idem of ('b -> 'a -> 'a);;

let rec rem_nth
  uu x1 = match uu, x1 with uu, [] -> []
    | n, x :: xs ->
        (if equal_nata n zero_nat then xs
          else x :: rem_nth (minus_nat n one_nat) xs);;

let rec fun_upd _A f a b = (fun x -> (if eq _A x a then b else f x));;

let rec fo_nmlz_rec (_A1, _A2, _A3)
  i m ad x3 = match i, m, ad, x3 with i, m, ad, [] -> []
    | i, m, ad, Inl x :: xs ->
        (if member (_A1, _A2) x ad
          then Inl x :: fo_nmlz_rec (_A1, _A2, _A3) i m ad xs
          else (match m (Inl x)
                 with None ->
                   Inr i ::
                     fo_nmlz_rec (_A1, _A2, _A3) (suc i)
                       (fun_upd (equal_sum _A3 equal_nat) m (Inl x) (Some i)) ad
                       xs
                 | Some j -> Inr j :: fo_nmlz_rec (_A1, _A2, _A3) i m ad xs))
    | i, m, ad, Inr n :: xs ->
        (match m (Inr n)
          with None ->
            Inr i ::
              fo_nmlz_rec (_A1, _A2, _A3) (suc i)
                (fun_upd (equal_sum _A3 equal_nat) m (Inr n) (Some i)) ad xs
          | Some j -> Inr j :: fo_nmlz_rec (_A1, _A2, _A3) i m ad xs);;

let rec fo_nmlz (_A1, _A2, _A3)
  = fo_nmlz_rec (_A1, _A2, _A3) zero_nat (fun _ -> None);;

let rec list_remdups
  equal x1 = match equal, x1 with
    equal, x :: xs ->
      (if list_member equal xs x then list_remdups equal xs
        else x :: list_remdups equal xs)
    | equal, [] -> [];;

let rec length _A xa = size_list (list_of_dlist _A xa);;

let rec card (_A1, _A2, _A3)
  = function
    Complement a ->
      (let aa = card (_A1, _A2, _A3) a in
       let s = of_phantom (card_UNIV _A1) in
        (if less_nat zero_nat s then minus_nat s aa
          else (if finite (_A1.finite_UNIV_card_UNIV, _A2, _A3) a then zero_nat
                 else failwith "card Complement: infinite"
                        (fun _ -> card (_A1, _A2, _A3) (Complement a)))))
    | Set_Monad xs ->
        (match ceq _A2
          with None ->
            failwith "card Set_Monad: ceq = None"
              (fun _ -> card (_A1, _A2, _A3) (Set_Monad xs))
          | Some eq -> size_list (list_remdups eq xs))
    | RBT_set rbt ->
        (match ccompare _A3
          with None ->
            failwith "card RBT_set: ccompare = None"
              (fun _ -> card (_A1, _A2, _A3) (RBT_set rbt))
          | Some _ -> size_list (keysb _A3 rbt))
    | DList_set dxs ->
        (match ceq _A2
          with None ->
            failwith "card DList_set: ceq = None"
              (fun _ -> card (_A1, _A2, _A3) (DList_set dxs))
          | Some _ -> length _A2 dxs);;

let rec fun_upda equal f aa b a = (if equal aa a then b else f a);;

let rec balance_right
  a k x xa3 = match a, k, x, xa3 with
    a, k, x, Branch (R, b, s, y, c) ->
      Branch (R, a, k, x, Branch (B, b, s, y, c))
    | Branch (B, a, k, x, b), s, y, Empty ->
        balance (Branch (R, a, k, x, b)) s y Empty
    | Branch (B, a, k, x, b), s, y, Branch (B, va, vb, vc, vd) ->
        balance (Branch (R, a, k, x, b)) s y (Branch (B, va, vb, vc, vd))
    | Branch (R, a, k, x, Branch (B, b, s, y, c)), t, z, Empty ->
        Branch (R, balance (paint R a) k x b, s, y, Branch (B, c, t, z, Empty))
    | Branch (R, a, k, x, Branch (B, b, s, y, c)), t, z,
        Branch (B, va, vb, vc, vd)
        -> Branch
             (R, balance (paint R a) k x b, s, y,
               Branch (B, c, t, z, Branch (B, va, vb, vc, vd)))
    | Empty, k, x, Empty -> Empty
    | Branch (R, va, vb, vc, Empty), k, x, Empty -> Empty
    | Branch (R, va, vb, vc, Branch (R, ve, vf, vg, vh)), k, x, Empty -> Empty
    | Empty, k, x, Branch (B, va, vb, vc, vd) -> Empty
    | Branch (R, ve, vf, vg, Empty), k, x, Branch (B, va, vb, vc, vd) -> Empty
    | Branch (R, ve, vf, vg, Branch (R, vi, vj, vk, vl)), k, x,
        Branch (B, va, vb, vc, vd)
        -> Empty;;

let rec balance_left
  x0 s y c = match x0, s, y, c with
    Branch (R, a, k, x, b), s, y, c ->
      Branch (R, Branch (B, a, k, x, b), s, y, c)
    | Empty, k, x, Branch (B, a, s, y, b) ->
        balance Empty k x (Branch (R, a, s, y, b))
    | Branch (B, va, vb, vc, vd), k, x, Branch (B, a, s, y, b) ->
        balance (Branch (B, va, vb, vc, vd)) k x (Branch (R, a, s, y, b))
    | Empty, k, x, Branch (R, Branch (B, a, s, y, b), t, z, c) ->
        Branch (R, Branch (B, Empty, k, x, a), s, y, balance b t z (paint R c))
    | Branch (B, va, vb, vc, vd), k, x,
        Branch (R, Branch (B, a, s, y, b), t, z, c)
        -> Branch
             (R, Branch (B, Branch (B, va, vb, vc, vd), k, x, a), s, y,
               balance b t z (paint R c))
    | Empty, k, x, Empty -> Empty
    | Empty, k, x, Branch (R, Empty, vb, vc, vd) -> Empty
    | Empty, k, x, Branch (R, Branch (R, ve, vf, vg, vh), vb, vc, vd) -> Empty
    | Branch (B, va, vb, vc, vd), k, x, Empty -> Empty
    | Branch (B, va, vb, vc, vd), k, x, Branch (R, Empty, vf, vg, vh) -> Empty
    | Branch (B, va, vb, vc, vd), k, x,
        Branch (R, Branch (R, vi, vj, vk, vl), vf, vg, vh)
        -> Empty;;

let rec combine
  xa0 x = match xa0, x with Empty, x -> x
    | Branch (v, va, vb, vc, vd), Empty -> Branch (v, va, vb, vc, vd)
    | Branch (R, a, k, x, b), Branch (R, c, s, y, d) ->
        (match combine b c
          with Empty -> Branch (R, a, k, x, Branch (R, Empty, s, y, d))
          | Branch (R, b2, t, z, c2) ->
            Branch (R, Branch (R, a, k, x, b2), t, z, Branch (R, c2, s, y, d))
          | Branch (B, b2, t, z, c2) ->
            Branch (R, a, k, x, Branch (R, Branch (B, b2, t, z, c2), s, y, d)))
    | Branch (B, a, k, x, b), Branch (B, c, s, y, d) ->
        (match combine b c
          with Empty -> balance_left a k x (Branch (B, Empty, s, y, d))
          | Branch (R, b2, t, z, c2) ->
            Branch (R, Branch (B, a, k, x, b2), t, z, Branch (B, c2, s, y, d))
          | Branch (B, b2, t, z, c2) ->
            balance_left a k x (Branch (B, Branch (B, b2, t, z, c2), s, y, d)))
    | Branch (B, va, vb, vc, vd), Branch (R, b, k, x, c) ->
        Branch (R, combine (Branch (B, va, vb, vc, vd)) b, k, x, c)
    | Branch (R, a, k, x, b), Branch (B, va, vb, vc, vd) ->
        Branch (R, a, k, x, combine b (Branch (B, va, vb, vc, vd)));;

let rec rbt_comp_del
  c x xa2 = match c, x, xa2 with c, x, Empty -> Empty
    | c, x, Branch (uu, a, y, s, b) ->
        (match c x y with Eq -> combine a b
          | Lt -> rbt_comp_del_from_left c x a y s b
          | Gt -> rbt_comp_del_from_right c x a y s b)
and rbt_comp_del_from_left
  c x xa2 y s b = match c, x, xa2, y, s, b with
    c, x, Branch (B, lt, z, v, rt), y, s, b ->
      balance_left (rbt_comp_del c x (Branch (B, lt, z, v, rt))) y s b
    | c, x, Empty, y, s, b -> Branch (R, rbt_comp_del c x Empty, y, s, b)
    | c, x, Branch (R, va, vb, vc, vd), y, s, b ->
        Branch (R, rbt_comp_del c x (Branch (R, va, vb, vc, vd)), y, s, b)
and rbt_comp_del_from_right
  c x a y s xa5 = match c, x, a, y, s, xa5 with
    c, x, a, y, s, Branch (B, lt, z, v, rt) ->
      balance_right a y s (rbt_comp_del c x (Branch (B, lt, z, v, rt)))
    | c, x, a, y, s, Empty -> Branch (R, a, y, s, rbt_comp_del c x Empty)
    | c, x, a, y, s, Branch (R, va, vb, vc, vd) ->
        Branch (R, a, y, s, rbt_comp_del c x (Branch (R, va, vb, vc, vd)));;

let rec rbt_comp_delete c k t = paint B (rbt_comp_del c k t);;

let rec delete _A
  xb xc =
    Mapping_RBTa (rbt_comp_delete (the (ccompare _A)) xb (impl_ofa _A xc));;

let rec list_remove1
  equal x xa2 = match equal, x, xa2 with
    equal, x, y :: xs ->
      (if equal x y then xs else y :: list_remove1 equal x xs)
    | equal, x, [] -> [];;

let rec removea _A
  xb xc = Abs_dlist (list_remove1 (the (ceq _A)) xb (list_of_dlist _A xc));;

let rec insert (_A1, _A2)
  xa x1 = match xa, x1 with
    xa, Complement x -> Complement (remove (_A1, _A2) xa x)
    | x, RBT_set rbt ->
        (match ccompare _A2
          with None ->
            failwith "insert RBT_set: ccompare = None"
              (fun _ -> insert (_A1, _A2) x (RBT_set rbt))
          | Some _ -> RBT_set (insertb _A2 x () rbt))
    | x, DList_set dxs ->
        (match ceq _A1
          with None ->
            failwith "insert DList_set: ceq = None"
              (fun _ -> insert (_A1, _A2) x (DList_set dxs))
          | Some _ -> DList_set (inserta _A1 x dxs))
    | x, Set_Monad xs -> Set_Monad (x :: xs)
    | x, Collect_set a ->
        (match ceq _A1
          with None ->
            failwith "insert Collect_set: ceq = None"
              (fun _ -> insert (_A1, _A2) x (Collect_set a))
          | Some eq -> Collect_set (fun_upda eq a x true))
and remove (_A1, _A2)
  x xa1 = match x, xa1 with
    x, Complement a -> Complement (insert (_A1, _A2) x a)
    | x, RBT_set rbt ->
        (match ccompare _A2
          with None ->
            failwith "remove RBT_set: ccompare = None"
              (fun _ -> remove (_A1, _A2) x (RBT_set rbt))
          | Some _ -> RBT_set (delete _A2 x rbt))
    | x, DList_set dxs ->
        (match ceq _A1
          with None ->
            failwith "remove DList_set: ceq = None"
              (fun _ -> remove (_A1, _A2) x (DList_set dxs))
          | Some _ -> DList_set (removea _A1 x dxs))
    | x, Collect_set a ->
        (match ceq _A1
          with None ->
            failwith "remove Collect: ceq = None"
              (fun _ -> remove (_A1, _A2) x (Collect_set a))
          | Some eq -> Collect_set (fun_upda eq a x false));;

let rec add_to_rbt_comp (_B1, _B2, _B3)
  c = (fun (a, b) t ->
        (match rbt_comp_lookup c t a
          with None ->
            rbt_comp_insert c a (insert (_B1, _B2) b (bot_set (_B1, _B2, _B3)))
              t
          | Some x -> rbt_comp_insert c a (insert (_B1, _B2) b x) t));;

let rec cluster_rbt_comp (_B1, _B2, _B3)
  c f t =
    folda (fun b _ ta ->
            (match f b with None -> ta
              | Some a -> add_to_rbt_comp (_B1, _B2, _B3) c (a, b) ta))
      t Empty;;

let rec mapping_of_cluster (_B1, _B2, _B3) _A
  xb xc =
    Mapping_RBTa (cluster_rbt_comp (_B1, _B2, _B3) (the (ccompare _A)) xb xc);;

let rec cluster (_A1, _A2, _A3) _B
  f (RBT_set t) =
    (match ccompare _B
      with None ->
        failwith "cluster: ccompare = None"
          (fun _ -> cluster (_A1, _A2, _A3) _B f (RBT_set t))
      | Some _ ->
        (match ccompare _A2
          with None ->
            failwith "cluster: ccompare = None"
              (fun _ -> cluster (_A1, _A2, _A3) _B f (RBT_set t))
          | Some _ ->
            RBT_Mapping
              (mapping_of_cluster (_A1, _A2, _A3) _B f (impl_ofa _A2 t))));;

let rec impl_of (Alist x) = x;;

let rec filtera xb xc = Alist (filter xb (impl_of xc));;

let rec filterb _A
  p x1 = match p, x1 with
    p, RBT_Mapping t ->
      (match ccompare _A
        with None ->
          failwith "filter RBT_Mapping: ccompare = None"
            (fun _ -> filterb _A p (RBT_Mapping t))
        | Some _ -> RBT_Mapping (filterd _A (fun (a, b) -> p a b) t))
    | p, Assoc_List_Mapping al ->
        Assoc_List_Mapping (filtera (fun (a, b) -> p a b) al)
    | p, Mapping m ->
        Mapping
          (fun k ->
            (match m k with None -> None
              | Some v -> (if p k v then Some v else None)));;

let rec pos _A
  a x1 = match a, x1 with a, [] -> None
    | a, x :: xs ->
        (if eq _A a x then Some zero_nat
          else (match pos _A a xs with None -> None | Some n -> Some (suc n)));;

let rec foldl f a x2 = match f, a, x2 with f, a, [] -> a
                | f, a, x :: xs -> foldl f (f a x) xs;;

let rec set_aux (_A1, _A2)
  = function Set_Monada -> (fun a -> Set_Monad a)
    | Set_Choose ->
        (match ccompare _A2
          with None ->
            (match ceq _A1 with None -> (fun a -> Set_Monad a)
              | Some _ ->
                foldl (fun s x -> insert (_A1, _A2) x s)
                  (DList_set (emptyb _A1)))
          | Some _ ->
            foldl (fun s x -> insert (_A1, _A2) x s) (RBT_set (emptyc _A2)))
    | impl ->
        foldl (fun s x -> insert (_A1, _A2) x s) (set_empty (_A1, _A2) impl);;

let rec set (_A1, _A2, _A3)
  xs = set_aux (_A1, _A2) (of_phantom (set_impl _A3)) xs;;

let rec map
  f x1 = match f, x1 with f, Empty -> Empty
    | f, Branch (c, lt, k, v, rt) -> Branch (c, map f lt, k, f k v, map f rt);;

let rec mapb _A xb xc = Mapping_RBTa (map xb (impl_ofa _A xc));;

let rec keysc (_A1, _A2, _A3) xa = set (_A1, _A2, _A3) (mapa fst (impl_of xa));;

let rec keys (_A1, _A2, _A3, _A4)
  = function RBT_Mapping t -> RBT_set (mapb _A3 (fun _ _ -> ()) t)
    | Assoc_List_Mapping al -> keysc (_A2, _A3, _A4) al
    | Mapping m -> collect _A1 (fun k -> not (is_none (m k)));;

let rec eval_forall (_A1, _A2, _A3, _A4)
  i ns (ad, (uu, x)) =
    (match pos equal_nat i ns with None -> (ad, (size_list ns, x))
      | Some j ->
        (let n = card (_A1, _A2, _A3) ad in
          (ad, (minus_nat (size_list ns) one_nat,
                 keys (cenum_list, (ceq_list (ceq_sum _A2 ceq_nat)),
                        (ccompare_list (ccompare_sum _A3 ccompare_nat)),
                        set_impl_list)
                   (filterb (ccompare_list (ccompare_sum _A3 ccompare_nat))
                     (fun t z ->
                       less_eq_nat
                         (plus_nat
                           (plus_nat n
                             (card (card_UNIV_nat, ceq_nat, ccompare_nat)
                               (set (ceq_nat, ccompare_nat, set_impl_nat)
                                 (map_filter
                                   (fun a ->
                                     (match a with Inl _ -> None
                                       | Inr aa -> Some aa))
                                   t))))
                           one_nat)
                         (card (card_UNIV_list,
                                 (ceq_list (ceq_sum _A2 ceq_nat)),
                                 (ccompare_list
                                   (ccompare_sum _A3 ccompare_nat)))
                           z))
                     (cluster
                       ((ceq_list (ceq_sum _A2 ceq_nat)),
                         (ccompare_list (ccompare_sum _A3 ccompare_nat)),
                         set_impl_list)
                       (ccompare_list (ccompare_sum _A3 ccompare_nat))
                       (comp (fun a -> Some a)
                         (fun ts -> fo_nmlz (_A2, _A3, _A4) ad (rem_nth j ts)))
                       x))))));;

let rec foldb _A x xc = folda (fun a _ -> x a) (impl_ofa _A xc);;

let rec image (_A1, _A2) (_B1, _B2, _B3)
  h x1 = match h, x1 with
    h, RBT_set rbt ->
      (match ccompare _A2
        with None ->
          failwith "image RBT_set: ccompare = None"
            (fun _ -> image (_A1, _A2) (_B1, _B2, _B3) h (RBT_set rbt))
        | Some _ ->
          foldb _A2 (comp (insert (_B1, _B2)) h) rbt (bot_set (_B1, _B2, _B3)))
    | g, DList_set dxs ->
        (match ceq _A1
          with None ->
            failwith "image DList_set: ceq = None"
              (fun _ -> image (_A1, _A2) (_B1, _B2, _B3) g (DList_set dxs))
          | Some _ ->
            foldc _A1 (comp (insert (_B1, _B2)) g) dxs
              (bot_set (_B1, _B2, _B3)))
    | f, Complement (Complement b) -> image (_A1, _A2) (_B1, _B2, _B3) f b
    | f, Collect_set a ->
        failwith "image Collect_set"
          (fun _ -> image (_A1, _A2) (_B1, _B2, _B3) f (Collect_set a))
    | f, Set_Monad xs -> Set_Monad (mapa f xs);;

let rec eval_exists (_A1, _A2, _A3)
  i ns (ad, (uu, x)) =
    (match pos equal_nat i ns with None -> (ad, (size_list ns, x))
      | Some j ->
        (ad, (minus_nat (size_list ns) one_nat,
               image ((ceq_list (ceq_sum _A1 ceq_nat)),
                       (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                 ((ceq_list (ceq_sum _A1 ceq_nat)),
                   (ccompare_list (ccompare_sum _A2 ccompare_nat)),
                   set_impl_list)
                 (fo_nmlz (_A1, _A2, _A3) ad)
                 (image
                   ((ceq_list (ceq_sum _A1 ceq_nat)),
                     (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                   ((ceq_list (ceq_sum _A1 ceq_nat)),
                     (ccompare_list (ccompare_sum _A2 ccompare_nat)),
                     set_impl_list)
                   (rem_nth j) x))));;

let rec filter_comp_minus
  c t1 t2 =
    filter (fun (k, _) -> is_none (rbt_comp_lookup c t2 k)) (entries t1);;

let rec small_rbt t = less_nat (bheight t) (nat_of_integer (Z.of_int 4));;

let rec comp_minus
  c t1 t2 =
    (if small_rbt t2 then folda (fun k _ -> rbt_comp_delete c k) t2 t1
      else (if small_rbt t1 then rbtreeify (filter_comp_minus c t1 t2)
             else (match t2 with Empty -> t1
                    | Branch (_, l2, a, _, r2) ->
                      (let (l1, (_, r1)) = rbt_split_comp c t1 a in
                        rbt_join2 (comp_minus c l1 l2)
                          (comp_minus c r1 r2)))));;

let rec rbt_comp_minus c t1 t2 = paint B (comp_minus c t1 t2);;

let rec minus _A
  xb xc =
    Mapping_RBTa
      (rbt_comp_minus (the (ccompare _A)) (impl_ofa _A xb) (impl_ofa _A xc));;

let rec minus_set (_A1, _A2)
  a b = match a, b with
    RBT_set rbt1, RBT_set rbt2 ->
      (match ccompare _A2
        with None ->
          failwith "minus RBT_set RBT_set: ccompare = None"
            (fun _ -> minus_set (_A1, _A2) (RBT_set rbt1) (RBT_set rbt2))
        | Some _ -> RBT_set (minus _A2 rbt1 rbt2))
    | a, b -> inf_seta (_A1, _A2) a (uminus_set b);;

let rec comp_fun_idem_apply (Abs_comp_fun_idem x) = x;;

let rec set_fold_cfi (_A1, _A2)
  f b x2 = match f, b, x2 with
    f, b, RBT_set rbt ->
      (match ccompare _A2
        with None ->
          failwith "set_fold_cfi RBT_set: ccompare = None"
            (fun _ -> set_fold_cfi (_A1, _A2) f b (RBT_set rbt))
        | Some _ -> foldb _A2 (comp_fun_idem_apply f) rbt b)
    | f, b, DList_set dxs ->
        (match ceq _A1
          with None ->
            failwith "set_fold_cfi DList_set: ceq = None"
              (fun _ -> set_fold_cfi (_A1, _A2) f b (DList_set dxs))
          | Some _ -> foldc _A1 (comp_fun_idem_apply f) dxs b)
    | f, b, Set_Monad xs -> fold (comp_fun_idem_apply f) xs b
    | f, b, Collect_set p ->
        failwith "set_fold_cfi not supported on Collect_set"
          (fun _ -> set_fold_cfi (_A1, _A2) f b (Collect_set p))
    | f, b, Complement a ->
        failwith "set_fold_cfi not supported on Complement"
          (fun _ -> set_fold_cfi (_A1, _A2) f b (Complement a));;

let rec sup_cfi _A
  = Abs_comp_fun_idem (sup _A.semilattice_sup_lattice.sup_semilattice_sup);;

let rec sup_setb (_A1, _A2, _A3, _A4, _A5)
  a = (if finite
            ((finite_UNIV_set _A1),
              (ceq_set (_A2, _A3, _A4.ccompare_cproper_interval)),
              (ccompare_set (_A1, _A3, _A4, _A5)))
            a
        then set_fold_cfi
               ((ceq_set (_A2, _A3, _A4.ccompare_cproper_interval)),
                 (ccompare_set (_A1, _A3, _A4, _A5)))
               (sup_cfi (lattice_set (_A2, _A3, _A4.ccompare_cproper_interval)))
               (bot_set (_A3, _A4.ccompare_cproper_interval, _A5)) a
        else failwith "Sup: infinite"
               (fun _ -> sup_setb (_A1, _A2, _A3, _A4, _A5) a));;

let rec ad_agr_close_rec (_A1, _A2)
  i m ad x3 = match i, m, ad, x3 with
    i, m, ad, [] ->
      insert
        ((ceq_list (ceq_sum _A1 ceq_nat)),
          (ccompare_list (ccompare_sum _A2 ccompare_nat)))
        [] (set_empty
             ((ceq_list (ceq_sum _A1 ceq_nat)),
               (ccompare_list (ccompare_sum _A2 ccompare_nat)))
             (of_phantom set_impl_lista))
    | i, m, ad, Inl x :: xs ->
        image ((ceq_list (ceq_sum _A1 ceq_nat)),
                (ccompare_list (ccompare_sum _A2 ccompare_nat)))
          ((ceq_list (ceq_sum _A1 ceq_nat)),
            (ccompare_list (ccompare_sum _A2 ccompare_nat)), set_impl_list)
          (fun a -> Inl x :: a) (ad_agr_close_rec (_A1, _A2) i m ad xs)
    | i, m, ad, Inr n :: xs ->
        (match m n
          with None ->
            sup_seta
              ((ceq_list (ceq_sum _A1 ceq_nat)),
                (ccompare_list (ccompare_sum _A2 ccompare_nat)))
              (sup_setb
                (finite_UNIV_list, cenum_list, (ceq_list (ceq_sum _A1 ceq_nat)),
                  (cproper_interval_list (ccompare_sum _A2 ccompare_nat)),
                  set_impl_list)
                (image (_A1, _A2)
                  ((ceq_set
                     (cenum_list, (ceq_list (ceq_sum _A1 ceq_nat)),
                       (cproper_interval_list
                         (ccompare_sum _A2
                           ccompare_nat)).ccompare_cproper_interval)),
                    (ccompare_set
                      (finite_UNIV_list, (ceq_list (ceq_sum _A1 ceq_nat)),
                        (cproper_interval_list (ccompare_sum _A2 ccompare_nat)),
                        set_impl_list)),
                    set_impl_set)
                  (fun x ->
                    image ((ceq_list (ceq_sum _A1 ceq_nat)),
                            (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                      ((ceq_list (ceq_sum _A1 ceq_nat)),
                        (ccompare_list (ccompare_sum _A2 ccompare_nat)),
                        set_impl_list)
                      (fun a -> Inl x :: a)
                      (ad_agr_close_rec (_A1, _A2) i
                        (fun_upd equal_nat m n (Some (Inl x)))
                        (remove (_A1, _A2) x ad) xs))
                  ad))
              (image
                ((ceq_list (ceq_sum _A1 ceq_nat)),
                  (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                ((ceq_list (ceq_sum _A1 ceq_nat)),
                  (ccompare_list (ccompare_sum _A2 ccompare_nat)),
                  set_impl_list)
                (fun a -> Inr i :: a)
                (ad_agr_close_rec (_A1, _A2) (suc i)
                  (fun_upd equal_nat m n (Some (Inr i))) ad xs))
          | Some v ->
            image ((ceq_list (ceq_sum _A1 ceq_nat)),
                    (ccompare_list (ccompare_sum _A2 ccompare_nat)))
              ((ceq_list (ceq_sum _A1 ceq_nat)),
                (ccompare_list (ccompare_sum _A2 ccompare_nat)), set_impl_list)
              (fun a -> v :: a) (ad_agr_close_rec (_A1, _A2) i m ad xs));;

let rec ad_agr_close (_A1, _A2)
  ad xs = ad_agr_close_rec (_A1, _A2) zero_nat (fun _ -> None) ad xs;;

let rec is_emptya _A
  xa = (match impl_ofa _A xa with Empty -> true
         | Branch (_, _, _, _, _) -> false);;

let rec exhaustive_above_fusion
  proper_interval g y s =
    (if has_next g s
      then (let (x, sa) = next g s in
             not (proper_interval (Some y) (Some x)) &&
               exhaustive_above_fusion proper_interval g x sa)
      else not (proper_interval (Some y) None));;

let rec exhaustive_fusion
  proper_interval g s =
    has_next g s &&
      (let (x, sa) = next g s in
        not (proper_interval None (Some x)) &&
          exhaustive_above_fusion proper_interval g x sa);;

let rec is_UNIV (_A1, _A2, _A3)
  = function
    RBT_set rbt ->
      (match ccompare _A3.ccompare_cproper_interval
        with None ->
          failwith "is_UNIV RBT_set: ccompare = None"
            (fun _ -> is_UNIV (_A1, _A2, _A3) (RBT_set rbt))
        | Some _ ->
          of_phantom (finite_UNIV _A1.finite_UNIV_card_UNIV) &&
            exhaustive_fusion (cproper_interval _A3) rbt_keys_generator
              (init _A3.ccompare_cproper_interval rbt))
    | a -> (let aa = of_phantom (card_UNIV _A1) in
            let b = card (_A1, _A2, _A3.ccompare_cproper_interval) a in
             (if less_nat zero_nat aa then equal_nata aa b
               else (if less_nat zero_nat b then false
                      else failwith "is_UNIV called on infinite type and set"
                             (fun _ -> is_UNIV (_A1, _A2, _A3) a))));;

let rec nulla _A xa = null (list_of_dlist _A xa);;

let rec is_empty (_A1, _A2, _A3)
  = function Complement a -> is_UNIV (_A1, _A2, _A3) a
    | RBT_set rbt ->
        (match ccompare _A3.ccompare_cproper_interval
          with None ->
            failwith "is_empty RBT_set: ccompare = None"
              (fun _ -> is_empty (_A1, _A2, _A3) (RBT_set rbt))
          | Some _ -> is_emptya _A3.ccompare_cproper_interval rbt)
    | DList_set dxs ->
        (match ceq _A2
          with None ->
            failwith "is_empty DList_set: ceq = None"
              (fun _ -> is_empty (_A1, _A2, _A3) (DList_set dxs))
          | Some _ -> nulla _A2 dxs)
    | Set_Monad xs -> null xs;;

let rec ad_agr_close_set (_A1, _A2, _A3)
  ad x =
    (if is_empty (_A1, _A2, _A3) ad then x
      else sup_setb
             (finite_UNIV_list, cenum_list, (ceq_list (ceq_sum _A2 ceq_nat)),
               (cproper_interval_list
                 (ccompare_sum _A3.ccompare_cproper_interval ccompare_nat)),
               set_impl_list)
             (image
               ((ceq_list (ceq_sum _A2 ceq_nat)),
                 (ccompare_list
                   (ccompare_sum _A3.ccompare_cproper_interval ccompare_nat)))
               ((ceq_set
                  (cenum_list, (ceq_list (ceq_sum _A2 ceq_nat)),
                    (cproper_interval_list
                      (ccompare_sum _A3.ccompare_cproper_interval
                        ccompare_nat)).ccompare_cproper_interval)),
                 (ccompare_set
                   (finite_UNIV_list, (ceq_list (ceq_sum _A2 ceq_nat)),
                     (cproper_interval_list
                       (ccompare_sum _A3.ccompare_cproper_interval
                         ccompare_nat)),
                     set_impl_list)),
                 set_impl_set)
               (ad_agr_close (_A2, _A3.ccompare_cproper_interval) ad) x));;

let rec upt i j = (if less_nat i j then i :: upt (suc i) j else []);;

let rec nall_tuples_rec (_A1, _A2, _A3)
  ad i n =
    (if equal_nata n zero_nat
      then insert
             ((ceq_list (ceq_sum _A1 ceq_nat)),
               (ccompare_list (ccompare_sum _A2 ccompare_nat)))
             [] (set_empty
                  ((ceq_list (ceq_sum _A1 ceq_nat)),
                    (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                  (of_phantom set_impl_lista))
      else sup_seta
             ((ceq_list (ceq_sum _A1 ceq_nat)),
               (ccompare_list (ccompare_sum _A2 ccompare_nat)))
             (sup_setb
               (finite_UNIV_list, cenum_list, (ceq_list (ceq_sum _A1 ceq_nat)),
                 (cproper_interval_list (ccompare_sum _A2 ccompare_nat)),
                 set_impl_list)
               (image
                 ((ceq_list (ceq_sum _A1 ceq_nat)),
                   (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                 ((ceq_set
                    (cenum_list, (ceq_list (ceq_sum _A1 ceq_nat)),
                      (cproper_interval_list
                        (ccompare_sum _A2
                          ccompare_nat)).ccompare_cproper_interval)),
                   (ccompare_set
                     (finite_UNIV_list, (ceq_list (ceq_sum _A1 ceq_nat)),
                       (cproper_interval_list (ccompare_sum _A2 ccompare_nat)),
                       set_impl_list)),
                   set_impl_set)
                 (fun asa ->
                   image ((ceq_sum _A1 ceq_nat),
                           (ccompare_sum _A2 ccompare_nat))
                     ((ceq_list (ceq_sum _A1 ceq_nat)),
                       (ccompare_list (ccompare_sum _A2 ccompare_nat)),
                       set_impl_list)
                     (fun x -> x :: asa)
                     (sup_seta
                       ((ceq_sum _A1 ceq_nat), (ccompare_sum _A2 ccompare_nat))
                       (image (_A1, _A2)
                         ((ceq_sum _A1 ceq_nat),
                           (ccompare_sum _A2 ccompare_nat),
                           (set_impl_sum _A3 set_impl_nat))
                         (fun a -> Inl a) ad)
                       (image (ceq_nat, ccompare_nat)
                         ((ceq_sum _A1 ceq_nat),
                           (ccompare_sum _A2 ccompare_nat),
                           (set_impl_sum _A3 set_impl_nat))
                         (fun a -> Inr a)
                         (set (ceq_nat, ccompare_nat, set_impl_nat)
                           (upt zero_nat i)))))
                 (nall_tuples_rec (_A1, _A2, _A3) ad i (minus_nat n one_nat))))
             (image
               ((ceq_list (ceq_sum _A1 ceq_nat)),
                 (ccompare_list (ccompare_sum _A2 ccompare_nat)))
               ((ceq_list (ceq_sum _A1 ceq_nat)),
                 (ccompare_list (ccompare_sum _A2 ccompare_nat)), set_impl_list)
               (fun a -> Inr i :: a)
               (nall_tuples_rec (_A1, _A2, _A3) ad (suc i)
                 (minus_nat n one_nat))));;

let rec merge
  x0 mys = match x0, mys with [], mys -> mys
    | v :: va, [] -> v :: va
    | (n, x) :: nxs, (m, y) :: mys ->
        (if less_eq_nat n m then (n, x) :: merge nxs ((m, y) :: mys)
          else (m, y) :: merge ((n, x) :: nxs) mys);;

let rec zip xs ys = match xs, ys with x :: xs, y :: ys -> (x, y) :: zip xs ys
              | xs, [] -> []
              | [], ys -> [];;

let rec ext_tuple (_A1, _A2, _A3)
  ad fv_sub fv_sub_comp asa =
    (if null fv_sub_comp
      then insert
             ((ceq_list (ceq_sum _A1 ceq_nat)),
               (ccompare_list (ccompare_sum _A2 ccompare_nat)))
             asa (set_empty
                   ((ceq_list (ceq_sum _A1 ceq_nat)),
                     (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                   (of_phantom set_impl_lista))
      else image ((ceq_list (ceq_sum _A1 ceq_nat)),
                   (ccompare_list (ccompare_sum _A2 ccompare_nat)))
             ((ceq_list (ceq_sum _A1 ceq_nat)),
               (ccompare_list (ccompare_sum _A2 ccompare_nat)), set_impl_list)
             (fun fs -> mapa snd (merge (zip fv_sub asa) (zip fv_sub_comp fs)))
             (nall_tuples_rec (_A1, _A2, _A3) ad
               (card (card_UNIV_nat, ceq_nat, ccompare_nat)
                 (set (ceq_nat, ccompare_nat, set_impl_nat)
                   (map_filter
                     (fun a -> (match a with Inl _ -> None | Inr aa -> Some aa))
                     asa)))
               (size_list fv_sub_comp)));;

let rec ext_tuple_set (_A1, _A2, _A3, _A4)
  ad nsa ns x =
    (if null ns then x
      else image ((ceq_list (ceq_sum _A1 ceq_nat)),
                   (ccompare_list (ccompare_sum _A2 ccompare_nat)))
             ((ceq_list (ceq_sum _A1 ceq_nat)),
               (ccompare_list (ccompare_sum _A2 ccompare_nat)), set_impl_list)
             (fo_nmlz (_A1, _A2, _A3) ad)
             (sup_setb
               (finite_UNIV_list, cenum_list, (ceq_list (ceq_sum _A1 ceq_nat)),
                 (cproper_interval_list (ccompare_sum _A2 ccompare_nat)),
                 set_impl_list)
               (image
                 ((ceq_list (ceq_sum _A1 ceq_nat)),
                   (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                 ((ceq_set
                    (cenum_list, (ceq_list (ceq_sum _A1 ceq_nat)),
                      (cproper_interval_list
                        (ccompare_sum _A2
                          ccompare_nat)).ccompare_cproper_interval)),
                   (ccompare_set
                     (finite_UNIV_list, (ceq_list (ceq_sum _A1 ceq_nat)),
                       (cproper_interval_list (ccompare_sum _A2 ccompare_nat)),
                       set_impl_list)),
                   set_impl_set)
                 (ext_tuple (_A1, _A2, _A4) ad nsa ns) x)));;

let rec entriesa _A xa = entries (impl_ofa _A xa);;

let rec set_of_idx (_A1, _A2, _A3) (_B1, _B2, _B3, _B4, _B5)
  (RBT_Mapping t) =
    (match ccompare _A2
      with None ->
        failwith "set_of_idx RBT_Mapping: ccompare = None"
          (fun _ ->
            set_of_idx (_A1, _A2, _A3) (_B1, _B2, _B3, _B4, _B5)
              (RBT_Mapping t))
      | Some _ ->
        sup_setb (_B1, _B2, _B3, _B4, _B5)
          (image
            ((ceq_prod _A1 (ceq_set (_B2, _B3, _B4.ccompare_cproper_interval))),
              (ccompare_prod _A2 (ccompare_set (_B1, _B3, _B4, _B5))))
            ((ceq_set (_B2, _B3, _B4.ccompare_cproper_interval)),
              (ccompare_set (_B1, _B3, _B4, _B5)), set_impl_set)
            snd (set ((ceq_prod _A1
                        (ceq_set (_B2, _B3, _B4.ccompare_cproper_interval))),
                       (ccompare_prod _A2 (ccompare_set (_B1, _B3, _B4, _B5))),
                       (set_impl_prod _A3 set_impl_set))
                  (entriesa _A2 t))));;

let rec proj_tuple
  x0 mys = match x0, mys with [], mys -> []
    | v :: va, [] -> []
    | n :: ns, (m, y) :: mys ->
        (if less_nat m n then proj_tuple (n :: ns) mys
          else (if equal_nata m n then y :: proj_tuple ns mys
                 else proj_tuple ns ((m, y) :: mys)));;

let rec mapping_join _B
  f (RBT_Mapping t) (RBT_Mapping u) =
    (match ccompare _B
      with None ->
        failwith "mapping_join RBT_Mapping: ccompare = None"
          (fun _ -> mapping_join _B f (RBT_Mapping t) (RBT_Mapping u))
      | Some _ -> RBT_Mapping (meet _B (fun _ -> f) t u));;

let rec insort_key _B
  f x xa2 = match f, x, xa2 with f, x, [] -> [x]
    | f, x, y :: ys ->
        (if less_eq _B.order_linorder.preorder_order.ord_preorder (f x) (f y)
          then x :: y :: ys else y :: insort_key _B f x ys);;

let rec foldr f x1 = match f, x1 with f, [] -> id
                | f, x :: xs -> comp (f x) (foldr f xs);;

let rec sort_key _B f xs = foldr (insort_key _B f) xs [];;

let rec isl = function Inl x1 -> true
              | Inr x2 -> false;;

let rec membera _A x0 y = match x0, y with [], y -> false
                     | x :: xs, y -> eq _A x y || membera _A xs y;;

let rec eval_conj_tuple (_A1, _A2, _A3, _A4)
  ad ns_phi ns_psi xs ys =
    (let cxs =
       filter (fun (n, x) -> not (membera equal_nat ns_psi n) && isl x)
         (zip ns_phi xs)
       in
     let nxs =
       map_filter
         (fun x ->
           (if (let (n, xa) = x in
                 not (membera equal_nat ns_psi n) && not (isl xa))
             then Some (fst x) else None))
         (zip ns_phi xs)
       in
     let cys =
       filter (fun (n, y) -> not (membera equal_nat ns_phi n) && isl y)
         (zip ns_psi ys)
       in
     let nys =
       map_filter
         (fun x ->
           (if (let (n, y) = x in
                 not (membera equal_nat ns_phi n) && not (isl y))
             then Some (fst x) else None))
         (zip ns_psi ys)
       in
      inf_seta
        ((ceq_list (ceq_sum _A1 ceq_nat)),
          (ccompare_list (ccompare_sum _A2 ccompare_nat)))
        (image
          ((ceq_list (ceq_sum _A1 ceq_nat)),
            (ccompare_list (ccompare_sum _A2 ccompare_nat)))
          ((ceq_list (ceq_sum _A1 ceq_nat)),
            (ccompare_list (ccompare_sum _A2 ccompare_nat)), set_impl_list)
          (fo_nmlz (_A1, _A2, _A3) ad)
          (ext_tuple (_A1, _A2, _A4) (bot_set (_A1, _A2, _A4))
            (sort_key linorder_nat (fun x -> x) (ns_phi @ mapa fst cys)) nys
            (mapa snd (merge (zip ns_phi xs) cys))))
        (image
          ((ceq_list (ceq_sum _A1 ceq_nat)),
            (ccompare_list (ccompare_sum _A2 ccompare_nat)))
          ((ceq_list (ceq_sum _A1 ceq_nat)),
            (ccompare_list (ccompare_sum _A2 ccompare_nat)), set_impl_list)
          (fo_nmlz (_A1, _A2, _A3) ad)
          (ext_tuple (_A1, _A2, _A4) (bot_set (_A1, _A2, _A4))
            (sort_key linorder_nat (fun x -> x) (ns_psi @ mapa fst cxs)) nxs
            (mapa snd (merge (zip ns_psi ys) cxs)))));;

let rec eval_conj_set (_A1, _A2, _A3, _A4)
  ad ns_phi x_phi ns_psi x_psi =
    sup_setb
      (finite_UNIV_list, cenum_list, (ceq_list (ceq_sum _A1 ceq_nat)),
        (cproper_interval_list (ccompare_sum _A2 ccompare_nat)), set_impl_list)
      (image
        ((ceq_list (ceq_sum _A1 ceq_nat)),
          (ccompare_list (ccompare_sum _A2 ccompare_nat)))
        ((ceq_set
           (cenum_list, (ceq_list (ceq_sum _A1 ceq_nat)),
             (cproper_interval_list
               (ccompare_sum _A2 ccompare_nat)).ccompare_cproper_interval)),
          (ccompare_set
            (finite_UNIV_list, (ceq_list (ceq_sum _A1 ceq_nat)),
              (cproper_interval_list (ccompare_sum _A2 ccompare_nat)),
              set_impl_list)),
          set_impl_set)
        (fun xs ->
          sup_setb
            (finite_UNIV_list, cenum_list, (ceq_list (ceq_sum _A1 ceq_nat)),
              (cproper_interval_list (ccompare_sum _A2 ccompare_nat)),
              set_impl_list)
            (image
              ((ceq_list (ceq_sum _A1 ceq_nat)),
                (ccompare_list (ccompare_sum _A2 ccompare_nat)))
              ((ceq_set
                 (cenum_list, (ceq_list (ceq_sum _A1 ceq_nat)),
                   (cproper_interval_list
                     (ccompare_sum _A2
                       ccompare_nat)).ccompare_cproper_interval)),
                (ccompare_set
                  (finite_UNIV_list, (ceq_list (ceq_sum _A1 ceq_nat)),
                    (cproper_interval_list (ccompare_sum _A2 ccompare_nat)),
                    set_impl_list)),
                set_impl_set)
              (eval_conj_tuple (_A1, _A2, _A3, _A4) ad ns_phi ns_psi xs) x_psi))
        x_phi);;

let rec idx_join (_A1, _A2, _A3, _A4)
  ad ns ns_phi x_phi ns_psi x_psi =
    (let idx_phi =
       cluster
         ((ceq_list (ceq_sum _A1 ceq_nat)),
           (ccompare_list (ccompare_sum _A2 ccompare_nat)), set_impl_list)
         (ccompare_list (ccompare_sum _A2 ccompare_nat))
         (comp (fun a -> Some a)
           (fun xs ->
             fo_nmlz (_A1, _A2, _A3) ad (proj_tuple ns (zip ns_phi xs))))
         x_phi
       in
     let idx_psi =
       cluster
         ((ceq_list (ceq_sum _A1 ceq_nat)),
           (ccompare_list (ccompare_sum _A2 ccompare_nat)), set_impl_list)
         (ccompare_list (ccompare_sum _A2 ccompare_nat))
         (comp (fun a -> Some a)
           (fun ys ->
             fo_nmlz (_A1, _A2, _A3) ad (proj_tuple ns (zip ns_psi ys))))
         x_psi
       in
      set_of_idx
        ((ceq_list (ceq_sum _A1 ceq_nat)),
          (ccompare_list (ccompare_sum _A2 ccompare_nat)), set_impl_list)
        (finite_UNIV_list, cenum_list, (ceq_list (ceq_sum _A1 ceq_nat)),
          (cproper_interval_list (ccompare_sum _A2 ccompare_nat)),
          set_impl_list)
        (mapping_join (ccompare_list (ccompare_sum _A2 ccompare_nat))
          (fun x_phia ->
            eval_conj_set (_A1, _A2, _A3, _A4) ad ns_phi x_phia ns_psi)
          idx_phi idx_psi));;

let rec map_option f x1 = match f, x1 with f, None -> None
                     | f, Some x2 -> Some (f x2);;

let rec map_valuesa
  xb xc = Alist (mapa (fun (x, y) -> (x, xb x y)) (impl_of xc));;

let rec map_values _A
  f x1 = match f, x1 with
    f, RBT_Mapping t ->
      (match ccompare _A
        with None ->
          failwith "map_values RBT_Mapping: ccompare = None"
            (fun _ -> map_values _A f (RBT_Mapping t))
        | Some _ -> RBT_Mapping (mapb _A f t))
    | f, Assoc_List_Mapping al -> Assoc_List_Mapping (map_valuesa f al)
    | f, Mapping m -> Mapping (fun k -> map_option (f k) (m k));;

let rec map_of _A
  x0 k = match x0, k with
    (l, v) :: ps, k -> (if eq _A l k then Some v else map_of _A ps k)
    | [], k -> None;;

let rec lookup _A xa = map_of _A (impl_of xa);;

let rec lookupa (_A1, _A2) = function RBT_Mapping t -> lookupb _A1 t
                             | Assoc_List_Mapping al -> lookup _A2 al;;

let rec eval_ajoin (_A1, _A2, _A3, _A4, _A5)
  ns_phi (aD_phi, (uu, x_phi)) ns_psi (aD_psi, (uv, x_psi)) =
    (let ad = sup_seta (_A2, _A3.ccompare_cproper_interval) aD_phi aD_psi in
     let aD_Delta_phi = minus_set (_A2, _A3.ccompare_cproper_interval) ad aD_phi
       in
     let aD_Delta_psi = minus_set (_A2, _A3.ccompare_cproper_interval) ad aD_psi
       in
     let ns = filter (membera equal_nat ns_psi) ns_phi in
     let ns_phia = filter (fun n -> not (membera equal_nat ns_phi n)) ns_psi in
     let idx_phi =
       cluster
         ((ceq_list (ceq_sum _A2 ceq_nat)),
           (ccompare_list
             (ccompare_sum _A3.ccompare_cproper_interval ccompare_nat)),
           set_impl_list)
         (ccompare_list
           (ccompare_sum _A3.ccompare_cproper_interval ccompare_nat))
         (comp (fun a -> Some a)
           (fun xs ->
             fo_nmlz (_A2, _A3.ccompare_cproper_interval, _A4) aD_psi
               (proj_tuple ns (zip ns_phi xs))))
         (ad_agr_close_set (_A1, _A2, _A3) aD_Delta_phi x_phi)
       in
     let idx_psi =
       cluster
         ((ceq_list (ceq_sum _A2 ceq_nat)),
           (ccompare_list
             (ccompare_sum _A3.ccompare_cproper_interval ccompare_nat)),
           set_impl_list)
         (ccompare_list
           (ccompare_sum _A3.ccompare_cproper_interval ccompare_nat))
         (comp (fun a -> Some a)
           (fun ys ->
             fo_nmlz (_A2, _A3.ccompare_cproper_interval, _A4) aD_psi
               (proj_tuple ns (zip ns_psi ys))))
         x_psi
       in
      (ad, (card (card_UNIV_nat, ceq_nat, ccompare_nat)
              (sup_seta (ceq_nat, ccompare_nat)
                (set (ceq_nat, ccompare_nat, set_impl_nat) ns_phi)
                (set (ceq_nat, ccompare_nat, set_impl_nat) ns_psi)),
             set_of_idx
               ((ceq_list (ceq_sum _A2 ceq_nat)),
                 (ccompare_list
                   (ccompare_sum _A3.ccompare_cproper_interval ccompare_nat)),
                 set_impl_list)
               (finite_UNIV_list, cenum_list, (ceq_list (ceq_sum _A2 ceq_nat)),
                 (cproper_interval_list
                   (ccompare_sum _A3.ccompare_cproper_interval ccompare_nat)),
                 set_impl_list)
               (map_values
                 (ccompare_list
                   (ccompare_sum _A3.ccompare_cproper_interval ccompare_nat))
                 (fun xs x ->
                   (match
                     lookupa
                       ((ccompare_list
                          (ccompare_sum _A3.ccompare_cproper_interval
                            ccompare_nat)),
                         (equal_list (equal_sum _A4 equal_nat)))
                       idx_psi xs
                     with None ->
                       ext_tuple_set
                         (_A2, _A3.ccompare_cproper_interval, _A4, _A5) ad
                         ns_phi ns_phia x
                     | Some y ->
                       idx_join (_A2, _A3.ccompare_cproper_interval, _A4, _A5)
                         ad ns ns_phi x ns_psi
                         (ad_agr_close_set (_A1, _A2, _A3) aD_Delta_psi
                           (minus_set
                             ((ceq_list (ceq_sum _A2 ceq_nat)),
                               (ccompare_list
                                 (ccompare_sum _A3.ccompare_cproper_interval
                                   ccompare_nat)))
                             (ext_tuple_set
                               (_A2, _A3.ccompare_cproper_interval, _A4, _A5)
                               aD_psi ns ns_phia
                               (insert
                                 ((ceq_list (ceq_sum _A2 ceq_nat)),
                                   (ccompare_list
                                     (ccompare_sum _A3.ccompare_cproper_interval
                                       ccompare_nat)))
                                 xs (set_empty
                                      ((ceq_list (ceq_sum _A2 ceq_nat)),
(ccompare_list (ccompare_sum _A3.ccompare_cproper_interval ccompare_nat)))
                                      (of_phantom set_impl_lista))))
                             y))))
                 idx_phi))));;

let rec set_fo_term (_A1, _A2, _A3)
  = function Const x1 -> insert (_A1, _A2) x1 (bot_set (_A1, _A2, _A3))
    | Var x2 -> bot_set (_A1, _A2, _A3);;

let rec unify_vals_terms _A _B
  x0 x1 sigma = match x0, x1, sigma with [], [], sigma -> Some sigma
    | v :: vs, Const c :: ts, sigma ->
        (if equal_suma _A _B v (Inl c) then unify_vals_terms _A _B vs ts sigma
          else None)
    | v :: vs, Var n :: ts, sigma ->
        (match sigma n
          with None ->
            unify_vals_terms _A _B vs ts (fun_upd equal_nat sigma n (Some v))
          | Some x ->
            (if equal_suma _A _B v x then unify_vals_terms _A _B vs ts sigma
              else None))
    | v :: va, [], uw -> None
    | [], v :: va, uw -> None;;

let rec fv_fo_term_list = function Var n -> [n]
                          | Const v -> [];;

let rec fv_fo_terms_list_rec
  = function [] -> []
    | t :: ts -> fv_fo_term_list t @ fv_fo_terms_list_rec ts;;

let rec remdups_adj _A
  = function [] -> []
    | [x] -> [x]
    | x :: y :: xs ->
        (if eq _A x y then remdups_adj _A (x :: xs)
          else x :: remdups_adj _A (y :: xs));;

let rec fv_fo_terms_list
  ts = remdups_adj equal_nat
         (sort_key linorder_nat (fun x -> x) (fv_fo_terms_list_rec ts));;

let rec eval_table (_A1, _A2, _A3) (_B1, _B2, _B3)
  ts x =
    (let fvs = fv_fo_terms_list ts in
      sup_setb
        (finite_UNIV_list, cenum_list, (ceq_list (ceq_sum _A1 _B1)),
          (cproper_interval_list (ccompare_sum _A2 _B2)), set_impl_list)
        (image
          ((ceq_list (ceq_sum _A1 _B1)), (ccompare_list (ccompare_sum _A2 _B2)))
          ((ceq_set
             (cenum_list, (ceq_list (ceq_sum _A1 _B1)),
               (cproper_interval_list
                 (ccompare_sum _A2 _B2)).ccompare_cproper_interval)),
            (ccompare_set
              (finite_UNIV_list, (ceq_list (ceq_sum _A1 _B1)),
                (cproper_interval_list (ccompare_sum _A2 _B2)), set_impl_list)),
            set_impl_set)
          (fun vs ->
            (match unify_vals_terms _A3 _B3 vs ts (fun _ -> None)
              with None ->
                set_empty
                  ((ceq_list (ceq_sum _A1 _B1)),
                    (ccompare_list (ccompare_sum _A2 _B2)))
                  (of_phantom set_impl_lista)
              | Some sigma ->
                insert
                  ((ceq_list (ceq_sum _A1 _B1)),
                    (ccompare_list (ccompare_sum _A2 _B2)))
                  (mapa (comp the sigma) fvs)
                  (set_empty
                    ((ceq_list (ceq_sum _A1 _B1)),
                      (ccompare_list (ccompare_sum _A2 _B2)))
                    (of_phantom set_impl_lista))))
          x));;

let rec eval_pred (_A1, _A2, _A3, _A4, _A5, _A6) (_B1, _B2, _B3)
  ts x =
    (let ad =
       sup_seta (_A3, _A4.ccompare_cproper_interval)
         (sup_setb (_A1, _A2, _A3, _A4, _A6)
           (set ((ceq_set (_A2, _A3, _A4.ccompare_cproper_interval)),
                  (ccompare_set (_A1, _A3, _A4, _A6)), set_impl_set)
             (mapa (set_fo_term (_A3, _A4.ccompare_cproper_interval, _A6)) ts)))
         (sup_setb (_A1, _A2, _A3, _A4, _A6)
           (image
             ((ceq_list _A3), (ccompare_list _A4.ccompare_cproper_interval))
             ((ceq_set (_A2, _A3, _A4.ccompare_cproper_interval)),
               (ccompare_set (_A1, _A3, _A4, _A6)), set_impl_set)
             (set (_A3, _A4.ccompare_cproper_interval, _A6)) x))
       in
      (ad, (size_list (fv_fo_terms_list ts),
             eval_table (_A3, _A4.ccompare_cproper_interval, _A5)
               (_B1, _B2, _B3) ts
               (image
                 ((ceq_list _A3), (ccompare_list _A4.ccompare_cproper_interval))
                 ((ceq_list (ceq_sum _A3 _B1)),
                   (ccompare_list
                     (ccompare_sum _A4.ccompare_cproper_interval _B2)),
                   set_impl_list)
                 (mapa (fun a -> Inl a)) x))));;

let rec eval_disj (_A1, _A2, _A3, _A4, _A5)
  ns_phi (aD_phi, (uu, x_phi)) ns_psi (aD_psi, (uv, x_psi)) =
    (let ad = sup_seta (_A2, _A3.ccompare_cproper_interval) aD_phi aD_psi in
     let ns_phia = filter (fun n -> not (membera equal_nat ns_phi n)) ns_psi in
     let ns_psia = filter (fun n -> not (membera equal_nat ns_psi n)) ns_phi in
     let aD_Delta_phi = minus_set (_A2, _A3.ccompare_cproper_interval) ad aD_phi
       in
     let aD_Delta_psi = minus_set (_A2, _A3.ccompare_cproper_interval) ad aD_psi
       in
      (ad, (card (card_UNIV_nat, ceq_nat, ccompare_nat)
              (sup_seta (ceq_nat, ccompare_nat)
                (set (ceq_nat, ccompare_nat, set_impl_nat) ns_phi)
                (set (ceq_nat, ccompare_nat, set_impl_nat) ns_psi)),
             sup_seta
               ((ceq_list (ceq_sum _A2 ceq_nat)),
                 (ccompare_list
                   (ccompare_sum _A3.ccompare_cproper_interval ccompare_nat)))
               (ext_tuple_set (_A2, _A3.ccompare_cproper_interval, _A4, _A5) ad
                 ns_phi ns_phia
                 (ad_agr_close_set (_A1, _A2, _A3) aD_Delta_phi x_phi))
               (ext_tuple_set (_A2, _A3.ccompare_cproper_interval, _A4, _A5) ad
                 ns_psi ns_psia
                 (ad_agr_close_set (_A1, _A2, _A3) aD_Delta_psi x_psi)))));;

let rec eval_conj (_A1, _A2, _A3, _A4, _A5)
  ns_phi (aD_phi, (uu, x_phi)) ns_psi (aD_psi, (uv, x_psi)) =
    (let ad = sup_seta (_A2, _A3.ccompare_cproper_interval) aD_phi aD_psi in
     let aD_Delta_phi = minus_set (_A2, _A3.ccompare_cproper_interval) ad aD_phi
       in
     let aD_Delta_psi = minus_set (_A2, _A3.ccompare_cproper_interval) ad aD_psi
       in
     let ns = filter (membera equal_nat ns_psi) ns_phi in
      (ad, (card (card_UNIV_nat, ceq_nat, ccompare_nat)
              (sup_seta (ceq_nat, ccompare_nat)
                (set (ceq_nat, ccompare_nat, set_impl_nat) ns_phi)
                (set (ceq_nat, ccompare_nat, set_impl_nat) ns_psi)),
             idx_join (_A2, _A3.ccompare_cproper_interval, _A4, _A5) ad ns
               ns_phi (ad_agr_close_set (_A1, _A2, _A3) aD_Delta_phi x_phi)
               ns_psi (ad_agr_close_set (_A1, _A2, _A3) aD_Delta_psi x_psi))));;

let rec eval_bool (_A1, _A2, _A3) (_B1, _B2)
  b = (if b then (bot_set (_A1, _A2, _A3),
                   (zero_nat,
                     insert
                       ((ceq_list (ceq_sum _A1 _B1)),
                         (ccompare_list (ccompare_sum _A2 _B2)))
                       [] (set_empty
                            ((ceq_list (ceq_sum _A1 _B1)),
                              (ccompare_list (ccompare_sum _A2 _B2)))
                            (of_phantom set_impl_lista))))
        else (bot_set (_A1, _A2, _A3),
               (zero_nat,
                 set_empty
                   ((ceq_list (ceq_sum _A1 _B1)),
                     (ccompare_list (ccompare_sum _A2 _B2)))
                   (of_phantom set_impl_lista))));;

let rec nall_tuples (_A1, _A2, _A3)
  ad n = nall_tuples_rec (_A1, _A2, _A3) ad zero_nat n;;

let rec eval_neg (_A1, _A2, _A3)
  ns (ad, (uu, x)) =
    (ad, (size_list ns,
           minus_set
             ((ceq_list (ceq_sum _A1 ceq_nat)),
               (ccompare_list (ccompare_sum _A2 ccompare_nat)))
             (nall_tuples (_A1, _A2, _A3) ad (size_list ns)) x));;

let rec fv_fo_fmla_list_rec
  = function Pred (uu, ts) -> fv_fo_terms_list ts
    | Bool b -> []
    | Eqa (ta, t) -> fv_fo_term_list ta @ fv_fo_term_list t
    | Neg phi -> fv_fo_fmla_list_rec phi
    | Conj (phi, psi) -> fv_fo_fmla_list_rec phi @ fv_fo_fmla_list_rec psi
    | Disj (phi, psi) -> fv_fo_fmla_list_rec phi @ fv_fo_fmla_list_rec psi
    | Exists (n, phi) ->
        filter (fun m -> not (equal_nata n m)) (fv_fo_fmla_list_rec phi)
    | Forall (n, phi) ->
        filter (fun m -> not (equal_nata n m)) (fv_fo_fmla_list_rec phi);;

let rec fv_fo_fmla_list
  phi = remdups_adj equal_nat
          (sort_key linorder_nat (fun x -> x) (fv_fo_fmla_list_rec phi));;

let rec eval_eq (_A1, _A2, _A3, _A4)
  ta t =
    (match ta
      with Const c ->
        (match t
          with Const ca ->
            (if eq _A3 c ca
              then (insert (_A1, _A2) c (bot_set (_A1, _A2, _A4)),
                     (zero_nat,
                       insert
                         ((ceq_list (ceq_sum _A1 ceq_nat)),
                           (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                         [] (set_empty
                              ((ceq_list (ceq_sum _A1 ceq_nat)),
                                (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                              (of_phantom set_impl_lista))))
              else (insert (_A1, _A2) c
                      (insert (_A1, _A2) ca (bot_set (_A1, _A2, _A4))),
                     (zero_nat,
                       set_empty
                         ((ceq_list (ceq_sum _A1 ceq_nat)),
                           (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                         (of_phantom set_impl_lista))))
          | Var _ ->
            (insert (_A1, _A2) c (bot_set (_A1, _A2, _A4)),
              (one_nat,
                insert
                  ((ceq_list (ceq_sum _A1 ceq_nat)),
                    (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                  [Inl c]
                  (set_empty
                    ((ceq_list (ceq_sum _A1 ceq_nat)),
                      (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                    (of_phantom set_impl_lista)))))
      | Var n ->
        (match t
          with Const c ->
            (insert (_A1, _A2) c (bot_set (_A1, _A2, _A4)),
              (one_nat,
                insert
                  ((ceq_list (ceq_sum _A1 ceq_nat)),
                    (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                  [Inl c]
                  (set_empty
                    ((ceq_list (ceq_sum _A1 ceq_nat)),
                      (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                    (of_phantom set_impl_lista))))
          | Var na ->
            (if equal_nata n na
              then (bot_set (_A1, _A2, _A4),
                     (one_nat,
                       insert
                         ((ceq_list (ceq_sum _A1 ceq_nat)),
                           (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                         [Inr zero_nat]
                         (set_empty
                           ((ceq_list (ceq_sum _A1 ceq_nat)),
                             (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                           (of_phantom set_impl_lista))))
              else (bot_set (_A1, _A2, _A4),
                     (nat_of_integer (Z.of_int 2),
                       insert
                         ((ceq_list (ceq_sum _A1 ceq_nat)),
                           (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                         [Inr zero_nat; Inr zero_nat]
                         (set_empty
                           ((ceq_list (ceq_sum _A1 ceq_nat)),
                             (ccompare_list (ccompare_sum _A2 ccompare_nat)))
                           (of_phantom set_impl_lista)))))));;

let rec eval_fmla (_A1, _A2, _A3, _A4, _A5, _A6, _A7)
  x0 i = match x0, i with
    Pred (r, ts), i ->
      eval_pred (_A1.finite_UNIV_card_UNIV, _A2, _A3, _A4, _A5, _A7)
        (ceq_nat, ccompare_nat, equal_nat) ts (i (r, size_list ts))
    | Bool b, i ->
        eval_bool (_A3, _A4.ccompare_cproper_interval, _A7)
          (ceq_nat, ccompare_nat) b
    | Eqa (ta, t), i ->
        eval_eq (_A3, _A4.ccompare_cproper_interval, _A5, _A7) ta t
    | Neg phi, i ->
        eval_neg (_A3, _A4.ccompare_cproper_interval, _A7) (fv_fo_fmla_list phi)
          (eval_fmla (_A1, _A2, _A3, _A4, _A5, _A6, _A7) phi i)
    | Conj (phi, psi), i ->
        (let ns_phi = fv_fo_fmla_list phi in
         let ns_psi = fv_fo_fmla_list psi in
         let x_phi = eval_fmla (_A1, _A2, _A3, _A4, _A5, _A6, _A7) phi i in
          (match psi
            with Pred (_, _) ->
              eval_conj (_A1, _A3, _A4, _A5, _A7) ns_phi x_phi ns_psi
                (eval_fmla (_A1, _A2, _A3, _A4, _A5, _A6, _A7) psi i)
            | Bool _ ->
              eval_conj (_A1, _A3, _A4, _A5, _A7) ns_phi x_phi ns_psi
                (eval_fmla (_A1, _A2, _A3, _A4, _A5, _A6, _A7) psi i)
            | Eqa (_, _) ->
              eval_conj (_A1, _A3, _A4, _A5, _A7) ns_phi x_phi ns_psi
                (eval_fmla (_A1, _A2, _A3, _A4, _A5, _A6, _A7) psi i)
            | Neg psia ->
              (let a = eval_fmla (_A1, _A2, _A3, _A4, _A5, _A6, _A7) psia i in
                eval_ajoin (_A1, _A3, _A4, _A5, _A7) ns_phi x_phi
                  (fv_fo_fmla_list psia) a)
            | Conj (_, _) ->
              eval_conj (_A1, _A3, _A4, _A5, _A7) ns_phi x_phi ns_psi
                (eval_fmla (_A1, _A2, _A3, _A4, _A5, _A6, _A7) psi i)
            | Disj (_, _) ->
              eval_conj (_A1, _A3, _A4, _A5, _A7) ns_phi x_phi ns_psi
                (eval_fmla (_A1, _A2, _A3, _A4, _A5, _A6, _A7) psi i)
            | Exists (_, _) ->
              eval_conj (_A1, _A3, _A4, _A5, _A7) ns_phi x_phi ns_psi
                (eval_fmla (_A1, _A2, _A3, _A4, _A5, _A6, _A7) psi i)
            | Forall (_, _) ->
              eval_conj (_A1, _A3, _A4, _A5, _A7) ns_phi x_phi ns_psi
                (eval_fmla (_A1, _A2, _A3, _A4, _A5, _A6, _A7) psi i)))
    | Disj (phi, psi), i ->
        eval_disj (_A1, _A3, _A4, _A5, _A7) (fv_fo_fmla_list phi)
          (eval_fmla (_A1, _A2, _A3, _A4, _A5, _A6, _A7) phi i)
          (fv_fo_fmla_list psi)
          (eval_fmla (_A1, _A2, _A3, _A4, _A5, _A6, _A7) psi i)
    | Exists (ia, phi), i ->
        eval_exists (_A3, _A4.ccompare_cproper_interval, _A5) ia
          (fv_fo_fmla_list phi)
          (eval_fmla (_A1, _A2, _A3, _A4, _A5, _A6, _A7) phi i)
    | Forall (ia, phi), i ->
        eval_forall (_A1, _A3, _A4.ccompare_cproper_interval, _A5) ia
          (fv_fo_fmla_list phi)
          (eval_fmla (_A1, _A2, _A3, _A4, _A5, _A6, _A7) phi i);;

let rec ad_terms (_A1, _A2, _A3, _A4, _A5)
  ts = sup_setb (_A1, _A2, _A3, _A4, _A5)
         (set ((ceq_set (_A2, _A3, _A4.ccompare_cproper_interval)),
                (ccompare_set (_A1, _A3, _A4, _A5)), set_impl_set)
           (mapa (set_fo_term (_A3, _A4.ccompare_cproper_interval, _A5)) ts));;

let rec act_edom (_A1, _A2, _A3, _A4, _A5)
  x0 i = match x0, i with
    Pred (r, ts), i ->
      sup_seta (_A3, _A4.ccompare_cproper_interval)
        (ad_terms (_A1, _A2, _A3, _A4, _A5) ts)
        (sup_setb (_A1, _A2, _A3, _A4, _A5)
          (image ((ceq_list _A3), (ccompare_list _A4.ccompare_cproper_interval))
            ((ceq_set (_A2, _A3, _A4.ccompare_cproper_interval)),
              (ccompare_set (_A1, _A3, _A4, _A5)), set_impl_set)
            (set (_A3, _A4.ccompare_cproper_interval, _A5))
            (i (r, size_list ts))))
    | Bool b, i -> bot_set (_A3, _A4.ccompare_cproper_interval, _A5)
    | Eqa (ta, t), i ->
        sup_seta (_A3, _A4.ccompare_cproper_interval)
          (set_fo_term (_A3, _A4.ccompare_cproper_interval, _A5) ta)
          (set_fo_term (_A3, _A4.ccompare_cproper_interval, _A5) t)
    | Neg phi, i -> act_edom (_A1, _A2, _A3, _A4, _A5) phi i
    | Conj (phi, psi), i ->
        sup_seta (_A3, _A4.ccompare_cproper_interval)
          (act_edom (_A1, _A2, _A3, _A4, _A5) phi i)
          (act_edom (_A1, _A2, _A3, _A4, _A5) psi i)
    | Disj (phi, psi), i ->
        sup_seta (_A3, _A4.ccompare_cproper_interval)
          (act_edom (_A1, _A2, _A3, _A4, _A5) phi i)
          (act_edom (_A1, _A2, _A3, _A4, _A5) psi i)
    | Exists (n, phi), i -> act_edom (_A1, _A2, _A3, _A4, _A5) phi i
    | Forall (n, phi), i -> act_edom (_A1, _A2, _A3, _A4, _A5) phi i;;

let rec wf_fo_intp (_A1, _A2)
  x0 i = match x0, i with
    Pred (r, ts), i ->
      finite (finite_UNIV_list, (ceq_list _A1), (ccompare_list _A2))
        (i (r, size_list ts))
    | Bool b, i -> true
    | Eqa (ta, t), i -> true
    | Neg phi, i -> wf_fo_intp (_A1, _A2) phi i
    | Conj (phi, psi), i ->
        wf_fo_intp (_A1, _A2) phi i && wf_fo_intp (_A1, _A2) psi i
    | Disj (phi, psi), i ->
        wf_fo_intp (_A1, _A2) phi i && wf_fo_intp (_A1, _A2) psi i
    | Exists (n, phi), i -> wf_fo_intp (_A1, _A2) phi i
    | Forall (n, phi), i -> wf_fo_intp (_A1, _A2) phi i;;

let rec sat (_A1, _A2, _A3, _A4, _A5, _A6, _A7)
  phi i sigma =
    (if wf_fo_intp (_A3, _A4.ccompare_cproper_interval) phi i
      then (let (_, a) = eval_fmla (_A1, _A2, _A3, _A4, _A5, _A6, _A7) phi i in
            let (_, aa) = a in
             member
               ((ceq_list (ceq_sum _A3 ceq_nat)),
                 (ccompare_list
                   (ccompare_sum _A4.ccompare_cproper_interval ccompare_nat)))
               (fo_nmlz (_A3, _A4.ccompare_cproper_interval, _A5)
                 (act_edom (_A1.finite_UNIV_card_UNIV, _A2, _A3, _A4, _A7) phi
                   i)
                 (mapa (comp (fun ab -> Inl ab) sigma) (fv_fo_fmla_list phi)))
               aa)
      else sat (_A1, _A2, _A3, _A4, _A5, _A6, _A7) phi i sigma);;

let rec rBT_Impl_rbt_all
  p x1 = match p, x1 with
    p, Branch (c, l, k, v, r) ->
      p k v && (rBT_Impl_rbt_all p l && rBT_Impl_rbt_all p r)
    | p, Empty -> true;;

let rec all _A xb xc = rBT_Impl_rbt_all xb (impl_ofa _A xc);;

let rec ball (_A1, _A2)
  x0 p = match x0, p with
    RBT_set rbt, p ->
      (match ccompare _A2
        with None ->
          failwith "Ball RBT_set: ccompare = None"
            (fun _ -> ball (_A1, _A2) (RBT_set rbt) p)
        | Some _ -> all _A2 (fun k _ -> p k) rbt)
    | DList_set dxs, p ->
        (match ceq _A1
          with None ->
            failwith "Ball DList_set: ceq = None"
              (fun _ -> ball (_A1, _A2) (DList_set dxs) p)
          | Some _ -> dlist_all _A1 p dxs)
    | Set_Monad xs, p -> list_all p xs;;

let rec update _A
  k v x2 = match k, v, x2 with k, v, [] -> [(k, v)]
    | k, v, p :: ps ->
        (if eq _A (fst p) k then (k, v) :: ps else p :: update _A k v ps);;

let empty : ('a, 'b) alist = Alist [];;

let rec updatea _A xc xd xe = Alist (update _A xc xd (impl_of xe));;

let rec mapping_empty_choose _A
  = (match ccompare _A with None -> Assoc_List_Mapping empty
      | Some _ -> RBT_Mapping (emptyc _A));;

let rec mapping_empty _A = function Mapping_RBT -> RBT_Mapping (emptyc _A)
                           | Mapping_Assoc_List -> Assoc_List_Mapping empty
                           | Mapping_Mapping -> Mapping (fun _ -> None)
                           | Mapping_Choose -> mapping_empty_choose _A;;

let rec emptya (_A1, _A2) = mapping_empty _A1 (of_phantom (mapping_impl _A2));;

let rec updateb (_A1, _A2)
  k v x2 = match k, v, x2 with
    k, v, RBT_Mapping t ->
      (match ccompare _A1
        with None ->
          failwith "update RBT_Mapping: ccompare = None"
            (fun _ -> updateb (_A1, _A2) k v (RBT_Mapping t))
        | Some _ -> RBT_Mapping (insertb _A1 k v t))
    | k, v, Assoc_List_Mapping al -> Assoc_List_Mapping (updatea _A2 k v al)
    | k, v, Mapping m -> Mapping (fun_upd _A2 m k (Some v));;

let rec projl (Inl x1) = x1;;

let rec fo_fin (_A1, _A2, _A3, _A4, _A5)
  (ad, (n, x)) =
    ball ((ceq_sum _A3 ceq_nat),
           (ccompare_sum _A4.ccompare_cproper_interval ccompare_nat))
      (sup_setb
        ((finite_UNIV_sum _A1 finite_UNIV_nat), (cenum_sum _A2 cenum_nat),
          (ceq_sum _A3 ceq_nat),
          (cproper_interval_sum _A4 cproper_interval_nat),
          (set_impl_sum _A5 set_impl_nat))
        (image
          ((ceq_list (ceq_sum _A3 ceq_nat)),
            (ccompare_list
              (ccompare_sum _A4.ccompare_cproper_interval ccompare_nat)))
          ((ceq_set
             ((cenum_sum _A2 cenum_nat), (ceq_sum _A3 ceq_nat),
               (cproper_interval_sum _A4
                 cproper_interval_nat).ccompare_cproper_interval)),
            (ccompare_set
              ((finite_UNIV_sum _A1 finite_UNIV_nat), (ceq_sum _A3 ceq_nat),
                (cproper_interval_sum _A4 cproper_interval_nat),
                (set_impl_sum _A5 set_impl_nat))),
            set_impl_set)
          (set ((ceq_sum _A3 ceq_nat),
                 (ccompare_sum _A4.ccompare_cproper_interval ccompare_nat),
                 (set_impl_sum _A5 set_impl_nat)))
          x))
      isl;;

let rec fo_res (_A1, _A2, _A3, _A4, _A5)
  (ad, (n, x)) =
    (if fo_fin (_A1, _A2, _A3, _A4, _A5) (ad, (n, x))
      then Fin (image
                 ((ceq_list (ceq_sum _A3 ceq_nat)),
                   (ccompare_list
                     (ccompare_sum _A4.ccompare_cproper_interval ccompare_nat)))
                 ((ceq_list _A3), (ccompare_list _A4.ccompare_cproper_interval),
                   set_impl_list)
                 (mapa projl) x)
      else Infin);;

let rec eval (_A1, _A2, _A3, _A4, _A5, _A6, _A7)
  phi i =
    (if wf_fo_intp (_A3, _A4.ccompare_cproper_interval) phi i
      then fo_res (_A1.finite_UNIV_card_UNIV, _A2, _A3, _A4, _A7)
             (eval_fmla (_A1, _A2, _A3, _A4, _A5, _A6, _A7) phi i)
      else Wf_error);;

let rec insert_db (_A1, _A2) (_B1, _B2, _B3)
  k v m =
    (match lookupa (_A1, _A2) m k
      with None ->
        updateb (_A1, _A2) k (insert (_B1, _B2) v (bot_set (_B1, _B2, _B3))) m
      | Some vs ->
        updateb (_A1, _A2) k
          (sup_seta (_B1, _B2) (insert (_B1, _B2) v (bot_set (_B1, _B2, _B3)))
            vs)
          m);;

let rec convert_db_rec (_A1, _A2) (_B1, _B2)
  x0 m = match x0, m with [], m -> m
    | (r, ts) :: ktss, m ->
        convert_db_rec (_A1, _A2) (_B1, _B2) ktss
          (insert_db
            ((ccompare_prod _A1 ccompare_nat), (equal_prod _A2 equal_nat))
            ((ceq_list _B1), (ccompare_list _B2), set_impl_list)
            (r, size_list ts) ts m);;

let rec convert_db (_A1, _A2, _A3) (_B1, _B2)
  ktss =
    (let m =
       convert_db_rec (_A1, _A2) (_B1, _B2) ktss
         (emptya
           ((ccompare_prod _A1 ccompare_nat),
             (mapping_impl_prod _A3 mapping_impl_nat)))
       in
      (fun x ->
        (match
          lookupa ((ccompare_prod _A1 ccompare_nat), (equal_prod _A2 equal_nat))
            m x
          with None ->
            set_empty ((ceq_list _B1), (ccompare_list _B2))
              (of_phantom set_impl_lista)
          | Some v -> v)));;

let rec sat_fin_nat
  phi i =
    sat (card_UNIV_nat, cenum_nat, ceq_nat, cproper_interval_nat, equal_nat,
          infinite_nat, set_impl_nat)
      phi i;;

let rec eval_fin_nat
  phi i =
    eval (card_UNIV_nat, cenum_nat, ceq_nat, cproper_interval_nat, equal_nat,
           infinite_nat, set_impl_nat)
      phi i;;

let rec rbt_nat_fold x = foldb ccompare_nat x;;

let rec convert_nat_db
  x = convert_db (ccompare_literal, equal_literal, mapping_impl_literal)
        (ceq_nat, ccompare_nat) x;;

let rec rbt_nat_list_fold x = foldb (ccompare_list ccompare_nat) x;;

let rec rbt_sum_list_fold
  x = foldb (ccompare_list (ccompare_sum ccompare_nat ccompare_nat)) x;;

end;; (*struct Eval_FO*)
