

import Mathlib.Order.Defs.LinearOrder
import Mathlib.Data.Nat.Basic
import Mathlib.Data.List.Basic
import Mathlib.Tactic

-- namespace DenseCheck.Ordered

universe u

-- Let's define a list where all the elements are distinct and ordered
def OrderedNodupList (α : Type u) [LinearOrder α] :=
  { l : List α // l.Pairwise (· < ·) }


-- Turns n into a list of distinct natural numbers, by looking at the binary
-- expansion of n
def nat2exps (n: Nat) (x: Nat) : List Nat :=
    if n = 0 then []
    else
        let np := n / 2
        let xp := x + 1
        let xs := nat2exps np xp
        if n % 2 = 0 then xs else x::xs

def nat2set (n: Nat): List Nat :=
    if n >= 0 then nat2exps n 0 else []

def listToSetDiff (d: Nat) (xs: List Nat): List Nat :=
    match xs with
    | [] => []
    | n::ns => (d + n)::(listToSetDiff (d + n + 1) ns)

theorem listToSetDiff_ge (d: Nat) (xs: List Nat) :
    ∀ x ∈ listToSetDiff d xs, d ≤ x := by
  induction xs generalizing d with
  | nil => simp [listToSetDiff]
  | cons n ns ih =>
    intro x hx
    simp [listToSetDiff] at hx
    rcases hx with rfl | hx
    · omega
    · have := ih (d + n + 1) x hx
      omega

theorem listToSetDiff_pairwise_lt
    (d: Nat) (xs: List Nat) :
    (listToSetDiff d xs).Pairwise (· < ·) := by
  induction xs generalizing d with
  | nil => simp [listToSetDiff]
  | cons n ns ih =>
    simp [listToSetDiff, List.pairwise_cons]
    constructor
    · intro x hx
      have := listToSetDiff_ge (d + n + 1) ns x hx
      omega
    · exact ih (d + n + 1)

-- let listToSet (ns: Nat list): Nat list = listToSetDiff 0I ns
def listToSet (ns: List Nat): List Nat :=
    listToSetDiff 0 ns

theorem listToSet_pairwise_lt
    (xs: List Nat) :
    (listToSet xs).Pairwise (· < ·) := by
    apply listToSetDiff_pairwise_lt

def setToListDiff (s: Nat) (xs: List Nat): List Nat :=
    match xs with
    | [] => []
    | n::ns => (n - s)::(setToListDiff (n + 1) ns)



def setToList (ns: OrderedNodupList Nat): List Nat :=
    setToListDiff 0 ns.val


-- Easy: no ordering needed, pure arithmetic induction
lemma setToListDiff_listToSetDiff (d : Nat) (xs : List Nat) :
    setToListDiff d (listToSetDiff d xs) = xs := by
  induction xs generalizing d with
  | nil => simp [listToSetDiff, setToListDiff]
  | cons n ns ih =>
    simp [listToSetDiff, setToListDiff, ih]


-- Let's create a theorem showing that setToList is the inverse of listToSet
theorem setToList_listToSet (xs: List Nat) :
    setToList ⟨listToSet xs, listToSet_pairwise_lt xs⟩ = xs :=
  setToListDiff_listToSetDiff 0 xs

-- Harder: needs both Pairwise and a lower bound hypothesis
lemma listToSetDiff_setToListDiff (s : Nat) (xs : List Nat)
    (hpw : xs.Pairwise (· < ·))
    (hge : ∀ x ∈ xs, s ≤ x) :
    listToSetDiff s (setToListDiff s xs) = xs := by
  induction xs generalizing s with
  | nil => simp [setToListDiff, listToSetDiff]
  | cons n ns ih =>
    rw [List.pairwise_cons] at hpw
    obtain ⟨hlt, hpw_ns⟩ := hpw
    have hn : s ≤ n := hge n (by simp)
    have hge_ns : ∀ x ∈ ns, n + 1 ≤ x := fun x hx => by
        have := hlt x hx; omega
    simp only [setToListDiff, listToSetDiff, Nat.add_sub_cancel' hn]
    exact congrArg (n :: ·) (ih (n + 1) hpw_ns hge_ns)

-- Let's create a theorem showing that listToSet is the inverse of setToList
@[simp]
theorem listToSet_setToList (xs: OrderedNodupList Nat) :
    listToSet (setToList xs) = xs.val :=
  listToSetDiff_setToListDiff 0 xs.val xs.property (fun _ _ => Nat.zero_le _)


-- Can we show that nat2expr produces a list of distinct natural numbers that's ordered?
lemma nat2exps_mem_ge (n x y : Nat) (hy : y ∈ nat2exps n x) : x ≤ y := by
  induction' n using Nat.strong_induction_on with n ih generalizing x y
  by_cases h0 : n = 0
  · rw [nat2exps, if_pos h0] at hy
    simp at hy
  · by_cases he : n % 2 = 0
    · rw [nat2exps, if_neg h0, if_pos he] at hy
      have hlt : n / 2 < n := by
        exact Nat.div_lt_self (Nat.pos_of_ne_zero h0) (by decide : 1 < 2)
      have hrec := ih (n / 2) hlt (x + 1) y hy
      omega
    · rw [nat2exps, if_neg h0, if_neg he] at hy
      simp at hy
      rcases hy with rfl | hy
      · omega
      · have hlt : n / 2 < n := by
          exact Nat.div_lt_self (Nat.pos_of_ne_zero h0) (by decide : 1 < 2)
        have hrec := ih (n / 2) hlt (x + 1) y hy
        omega

theorem nat2exps_pairwise_lt (n x : Nat) :
    (nat2exps n x).Pairwise (· < ·) := by
  induction' n using Nat.strong_induction_on with n ih generalizing x
  by_cases h0 : n = 0
  · rw [nat2exps, if_pos h0]
    simp
  · by_cases he : n % 2 = 0
    · rw [nat2exps, if_neg h0, if_pos he]
      have hlt : n / 2 < n := by
        exact Nat.div_lt_self (Nat.pos_of_ne_zero h0) (by decide : 1 < 2)
      simpa using ih (n / 2) hlt (x + 1)
    · rw [nat2exps, if_neg h0, if_neg he]
      rw [List.pairwise_cons]
      constructor
      · intro y hy
        have hxy : x + 1 ≤ y := nat2exps_mem_ge (n / 2) (x + 1) y hy
        omega
      · have hlt : n / 2 < n := by
          exact Nat.div_lt_self (Nat.pos_of_ne_zero h0) (by decide : 1 < 2)
        simpa using ih (n / 2) hlt (x + 1)

def nat2expsOrdered (n: Nat) (x: Nat) : OrderedNodupList Nat :=
  ⟨nat2exps n x, nat2exps_pairwise_lt n x⟩

def Nat.toOrdered (n: Nat) : OrderedNodupList Nat :=
  nat2expsOrdered n 0

def expsToNat : List Nat → Nat
  | [] => 0
  | n::ns => (2 ^ n) + expsToNat ns

lemma expsToNat_ne_zero_of_cons (n : Nat) (ns : List Nat) :
    expsToNat (n :: ns) ≠ 0 := by
  have hpow : 0 < 2 ^ n := by positivity
  intro h
  have hsum : 0 < (2 ^ n) + expsToNat ns := by grind
  have hpos : 0 < expsToNat (n :: ns) := by
    rw [expsToNat]
    exact hsum
  grind

lemma expsToNat_eq_zero_iff (xs : List Nat) :
    expsToNat xs = 0 ↔ xs = [] := by
  constructor
  · intro h
    cases xs with
    | nil => rfl
    | cons n ns =>
      exact False.elim ((expsToNat_ne_zero_of_cons n ns) h)
  · intro h
    simp [h, expsToNat]

def OrderedNodupList.toNat (l: OrderedNodupList Nat) : Nat :=
  expsToNat l.val

lemma expsToNat_nat2exps (n x : Nat) :
    expsToNat (nat2exps n x) = (2 ^ x) * n := by
  induction' n using Nat.strong_induction_on with n ih generalizing x
  by_cases h0 : n = 0
  · subst h0
    simp [nat2exps, expsToNat]
  · by_cases he : n % 2 = 0
    · rw [nat2exps, if_neg h0, if_pos he]
      have hlt : n / 2 < n := by
        exact Nat.div_lt_self (Nat.pos_of_ne_zero h0) (by decide : 1 < 2)
      have hrec := ih (n / 2) hlt (x + 1)
      rw [hrec]
      have hdecomp : n = 2 * (n / 2) := by
        have h := Nat.mod_add_div n 2
        rw [he, zero_add] at h
        exact h.symm
      calc
        (2 ^ (x + 1)) * (n / 2)
            = (2 ^ x) * (2 * (n / 2)) := by
                simp [Nat.pow_succ, Nat.mul_assoc, Nat.mul_comm, Nat.mul_left_comm]
        _ = (2 ^ x) * n := by
          exact congrArg ((2 ^ x) * ·) hdecomp.symm
    · rw [nat2exps, if_neg h0, if_neg he]
      simp [expsToNat]
      have hlt : n / 2 < n := by
        exact Nat.div_lt_self (Nat.pos_of_ne_zero h0) (by decide : 1 < 2)
      have hrec := ih (n / 2) hlt (x + 1)
      rw [hrec]
      have hmodlt : n % 2 < 2 := Nat.mod_lt _ (by decide : 0 < 2)
      have hmodpos : 0 < n % 2 := Nat.pos_of_ne_zero he
      have hmod1 : n % 2 = 1 := by omega
      have hdecomp : 1 + 2 * (n / 2) = n := by
        have h := Nat.mod_add_div n 2
        rw [hmod1] at h
        exact h
      calc
        2 ^ x + (2 ^ (x + 1)) * (n / 2)
            = 2 ^ x + ((2 * (n / 2)) * 2 ^ x) := by
                simp [Nat.pow_succ, Nat.mul_assoc, Nat.mul_comm, Nat.mul_left_comm]
        _ = (1 * 2 ^ x) + ((2 * (n / 2)) * 2 ^ x) := by simp
        _ = (1 + 2 * (n / 2)) * 2 ^ x := by rw [Nat.add_mul]
        _ = (2 ^ x) * (1 + 2 * (n / 2)) := by rw [Nat.mul_comm]
        _ = (2 ^ x) * n := by rw [hdecomp]

theorem fromNat_toNat (n: Nat) :
    OrderedNodupList.toNat (Nat.toOrdered n) = n := by
  simpa [OrderedNodupList.toNat, Nat.toOrdered, nat2expsOrdered] using
    expsToNat_nat2exps n 0

lemma expsToNat_mod2_eq_zero_of_pos (xs : List Nat)
    (hpos : ∀ y ∈ xs, 0 < y) :
    expsToNat xs % 2 = 0 := by
  induction xs with
  | nil => simp [expsToNat]
  | cons a as ih =>
    have ha_pos : 0 < a := hpos a (by simp)
    have hpos_as : ∀ y ∈ as, 0 < y := by
      intro y hy
      exact hpos y (by simp [hy])
    have ih' := ih hpos_as
    have ha0 : a ≠ 0 := by omega
    rcases Nat.exists_eq_succ_of_ne_zero ha0 with ⟨k, hk⟩
    subst hk
    have hpow_even : (2 ^ Nat.succ k) % 2 = 0 := by
      simp [Nat.pow_succ, Nat.mul_comm]
    rw [expsToNat, Nat.add_mod]
    simp [hpow_even, ih']

lemma expsToNat_div2_of_pos (xs : List Nat)
    (hpos : ∀ y ∈ xs, 0 < y) :
    expsToNat xs / 2 = expsToNat (xs.map Nat.pred) := by
  induction xs with
  | nil => simp [expsToNat]
  | cons a as ih =>
    have ha_pos : 0 < a := hpos a (by simp)
    have hpos_as : ∀ y ∈ as, 0 < y := by
      intro y hy
      exact hpos y (by simp [hy])
    have ih' := ih hpos_as
    have heven_as : expsToNat as % 2 = 0 := expsToNat_mod2_eq_zero_of_pos as hpos_as
    have hdecomp_as : expsToNat as = 2 * (expsToNat as / 2) := by
      have h := Nat.mod_add_div (expsToNat as) 2
      rw [heven_as, zero_add] at h
      exact h.symm
    have ha0 : a ≠ 0 := by omega
    rcases Nat.exists_eq_succ_of_ne_zero ha0 with ⟨k, hk⟩
    subst hk
    calc
      expsToNat (Nat.succ k :: as) / 2
          = (2 ^ Nat.succ k + expsToNat as) / 2 := by simp [expsToNat]
      _ = (2 * 2 ^ k + expsToNat as) / 2 := by simp [Nat.pow_succ, Nat.mul_comm]
      _ = 2 ^ k + expsToNat as / 2 := by
        omega
      _ = 2 ^ k + expsToNat (as.map Nat.pred) := by rw [ih']
      _ = expsToNat (Nat.pred (Nat.succ k) :: as.map Nat.pred) := by simp [expsToNat]
      _ = expsToNat ((Nat.succ k :: as).map Nat.pred) := by simp

lemma pairwise_pred_of_pairwise_pos (xs : List Nat)
    (hxs : xs.Pairwise (· < ·))
    (hpos : ∀ y ∈ xs, 0 < y) :
    (xs.map Nat.pred).Pairwise (· < ·) := by
  induction xs with
  | nil => simp
  | cons a as ih =>
    rw [List.pairwise_cons] at hxs
    obtain ⟨hhead, htail⟩ := hxs
    simp [List.pairwise_cons]
    constructor
    · intro y hy
      have hy_pos : 0 < y := hpos y (by simp [hy])
      have hay : a < y := hhead y hy
      have ha0 : a ≠ 0 := Nat.ne_of_gt (hpos a (by simp))
      have hy0 : y ≠ 0 := Nat.ne_of_gt hy_pos
      rcases Nat.exists_eq_succ_of_ne_zero ha0 with ⟨a', ha'⟩
      rcases Nat.exists_eq_succ_of_ne_zero hy0 with ⟨y', hy'⟩
      subst ha'
      subst hy'
      have : a' < y' := by omega
      simpa using this
    · have hpos_as : ∀ y ∈ as, 0 < y := by
        intro y hy
        exact hpos y (by simp [hy])
      exact ih htail hpos_as

lemma map_add_one_pred_of_pos (xs : List Nat)
    (hpos : ∀ y ∈ xs, 0 < y) :
    (xs.map Nat.pred).map (fun a => a + 1) = xs := by
  induction xs with
  | nil => simp
  | cons a as ih =>
    have ha_pos : 0 < a := hpos a (by simp)
    have hpos_as : ∀ y ∈ as, 0 < y := by
      intro y hy
      exact hpos y (by simp [hy])
    have hhead : a - 1 + 1 = a := by omega
    simp [Nat.pred_eq_sub_one, hhead, ih hpos_as]

lemma nat2exps_shift (n x k : Nat) :
    nat2exps n (x + k) = (nat2exps n x).map (fun a => a + k) := by
  induction' n using Nat.strong_induction_on with n ih generalizing x k
  by_cases h0 : n = 0
  · subst h0
    simp [nat2exps]
  · by_cases he : n % 2 = 0
    · have hlt : n / 2 < n := by
        exact Nat.div_lt_self (Nat.pos_of_ne_zero h0) (by decide : 1 < 2)
      have hrec := ih (n / 2) hlt (x + 1) k
      have hx : nat2exps n x = nat2exps (n / 2) (x + 1) := by
        rw [nat2exps, if_neg h0, if_pos he]
      rw [nat2exps, if_neg h0, if_pos he]
      rw [hx]
      simpa [Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using hrec
    · have hlt : n / 2 < n := by
        exact Nat.div_lt_self (Nat.pos_of_ne_zero h0) (by decide : 1 < 2)
      have hrec := ih (n / 2) hlt (x + 1) k
      have hx : nat2exps n x = x :: nat2exps (n / 2) (x + 1) := by
        rw [nat2exps, if_neg h0, if_neg he]
      rw [nat2exps, if_neg h0, if_neg he]
      rw [hx]
      have hrec'' : nat2exps (n / 2) (x + k + 1) =
          List.map (fun a => a + k) (nat2exps (n / 2) (x + 1)) := by
        simpa [Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using hrec
      exact congrArg (fun t => (x + k) :: t) hrec''

lemma nat2exps_expsToNat_pairwise (xs : List Nat)
    (hxs : xs.Pairwise (· < ·)) :
    nat2exps (expsToNat xs) 0 = xs := by
  let P : Nat → Prop := fun m =>
    ∀ ys : List Nat, ys.Pairwise (· < ·) → expsToNat ys = m → nat2exps m 0 = ys
  have hmain : ∀ m : Nat, P m := by
    intro m
    induction' m using Nat.strong_induction_on with m ih
    intro ys hys hm
    cases ys with
    | nil =>
      simp [expsToNat] at hm
      subst hm
      simp [nat2exps]
    | cons n ns =>
      rw [List.pairwise_cons] at hys
      obtain ⟨hhead, htail⟩ := hys
      by_cases hn0 : n = 0
      · subst hn0
        have hpos_ns : ∀ y ∈ ns, 0 < y := by
          intro y hy
          have hylt := hhead y hy
          omega
        have heven : expsToNat ns % 2 = 0 := expsToNat_mod2_eq_zero_of_pos ns hpos_ns
        have hhalf : (1 + expsToNat ns) / 2 = expsToNat ns / 2 := by
          have hdecomp : expsToNat ns = 2 * (expsToNat ns / 2) := by
            have h := Nat.mod_add_div (expsToNat ns) 2
            rw [heven, zero_add] at h
            exact h.symm
          have hstep : (1 + expsToNat ns) / 2 = (1 + 2 * (expsToNat ns / 2)) / 2 := by
            conv_lhs => rw [hdecomp]
          have hq : (1 + 2 * (expsToNat ns / 2)) / 2 = expsToNat ns / 2 := by
            simpa using (Nat.add_mul_div_left 1 (expsToNat ns / 2) zero_lt_two)
          calc
            (1 + expsToNat ns) / 2 = (1 + 2 * (expsToNat ns / 2)) / 2 := hstep
            _ = expsToNat ns / 2 := hq
        have hshift : nat2exps (expsToNat ns) 0 = nat2exps (expsToNat ns / 2) 1 := by
          by_cases hzero : expsToNat ns = 0
          · simp [hzero, nat2exps]
          · rw [nat2exps, if_neg hzero, if_pos heven]
        have hm_ns : expsToNat (0 :: ns) = m := hm
        have hm_m : m = 1 + expsToNat ns := by simpa [expsToNat] using hm_ns.symm
        have hodd : (1 + expsToNat ns) % 2 ≠ 0 := by omega
        have hneq : 1 + expsToNat ns ≠ 0 := by omega
        have hrec0 : nat2exps (expsToNat ns) 0 = ns := by
          have hm_lt : expsToNat ns < m := by
            omega
          exact ih (expsToNat ns) hm_lt ns htail rfl
        calc
          nat2exps m 0 = nat2exps (1 + expsToNat ns) 0 := by rw [hm_m]
          _ = 0 :: nat2exps ((1 + expsToNat ns) / 2) 1 := by
            rw [nat2exps, if_neg hneq, if_neg hodd]
          _ = 0 :: nat2exps (expsToNat ns / 2) 1 := by rw [hhalf]
          _ = 0 :: nat2exps (expsToNat ns) 0 := by rw [hshift]
          _ = 0 :: ns := by rw [hrec0]
      · have hpos_all : ∀ y ∈ (n :: ns), 0 < y := by
          intro y hy
          simp at hy
          rcases hy with rfl | hy
          · omega
          · have hylt := hhead y hy
            omega
        have heven_cons : expsToNat (n :: ns) % 2 = 0 :=
          expsToNat_mod2_eq_zero_of_pos (n :: ns) hpos_all
        have hne_cons : expsToNat (n :: ns) ≠ 0 := expsToNat_ne_zero_of_cons n ns
        have hm_cons : expsToNat (n :: ns) = m := hm
        have hm_div : expsToNat (n :: ns) / 2 = expsToNat ((n :: ns).map Nat.pred) :=
          expsToNat_div2_of_pos (n :: ns) hpos_all
        have hpred_pairwise : ((n :: ns).map Nat.pred).Pairwise (· < ·) :=
          pairwise_pred_of_pairwise_pos (n :: ns) (by exact List.pairwise_cons.2 ⟨hhead, htail⟩) hpos_all
        have hm_lt : expsToNat ((n :: ns).map Nat.pred) < m := by
          have hpos_cons : 0 < expsToNat (n :: ns) := Nat.pos_of_ne_zero hne_cons
          have hdiv_lt : expsToNat (n :: ns) / 2 < expsToNat (n :: ns) := by
            exact Nat.div_lt_self hpos_cons (by decide : 1 < 2)
          rw [hm_div] at hdiv_lt
          simpa [hm_cons] using hdiv_lt
        have hrec_pred :
            nat2exps (expsToNat ((n :: ns).map Nat.pred)) 0 = (n :: ns).map Nat.pred := by
          exact ih (expsToNat ((n :: ns).map Nat.pred)) hm_lt ((n :: ns).map Nat.pred) hpred_pairwise rfl
        have hmap_back : ((n :: ns).map Nat.pred).map (fun a => a + 1) = (n :: ns) :=
          map_add_one_pred_of_pos (n :: ns) hpos_all
        have hshift1 : nat2exps (expsToNat ((n :: ns).map Nat.pred)) 1 =
            (nat2exps (expsToNat ((n :: ns).map Nat.pred)) 0).map (fun a => a + 1) := by
          simpa using (nat2exps_shift (expsToNat ((n :: ns).map Nat.pred)) 0 1)
        have hm_m : m = expsToNat (n :: ns) := hm_cons.symm
        calc
          nat2exps m 0 = nat2exps (expsToNat (n :: ns)) 0 := by rw [hm_m]
          _ = nat2exps (expsToNat (n :: ns) / 2) 1 := by
            rw [nat2exps, if_neg hne_cons, if_pos heven_cons]
          _ = nat2exps (expsToNat ((n :: ns).map Nat.pred)) 1 := by rw [hm_div]
          _ = (nat2exps (expsToNat ((n :: ns).map Nat.pred)) 0).map (fun a => a + 1) := by rw [hshift1]
          _ = ((n :: ns).map Nat.pred).map (fun a => a + 1) := by rw [hrec_pred]
          _ = n :: ns := by rw [hmap_back]
  exact hmain (expsToNat xs) xs hxs rfl

theorem toNat_fromNat (l: OrderedNodupList Nat) :
    Nat.toOrdered (OrderedNodupList.toNat l) = l := by
  cases l with
  | mk xs hxs =>
    apply Subtype.ext
    simpa [OrderedNodupList.toNat, Nat.toOrdered, nat2expsOrdered] using
      nat2exps_expsToNat_pairwise xs hxs

def List.toOrderedNodup (xs: List Nat) : OrderedNodupList Nat :=
  ⟨listToSet xs, listToSet_pairwise_lt xs⟩

section Examples1

  def list1 : OrderedNodupList Nat := ⟨[1, 2, 3], by decide⟩
  def list2 := nat2set 10
  #eval 100313455642417 |> Nat.toOrdered |> OrderedNodupList.toNat



end Examples1

-- end DenseCheck.Ordered
