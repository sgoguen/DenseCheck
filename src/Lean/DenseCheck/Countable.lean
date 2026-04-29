import Mathlib.Logic.Denumerable
import Mathlib.Data.Fin.Basic

namespace DenseCheck

structure FinDenumerable (α : Type*) where
  card          : ℕ
  encode        : α → Fin card
  decode        : Fin card → α   -- i.e. Fin card → α
  decode_encode : ∀ i, encode (decode i) = i



end DenseCheck

namespace DenseCheck


-- instance : Fintype Bool where
--   elems := {false, true}
--   complete := by decide

-- #check Denumerable Bool

def DistinctList (α : Type*) := { l : List α // l.Nodup }

def DistinctList.toFinset {α : Type*} [DecidableEq α] (l : DistinctList α) : Finset α :=
  l.val.toFinset

def values : Finset String := {"one", "two", "three", "four"}
def values2 : Set Nat := {1,1,2,3,4}
def values3 : Multiset Nat := {1,1,2,3,4}
def values4 : List Nat := {1,1,2,3,4}

def Nat1to5 := { n : Nat // 1 ≤ n ∧ n ≤ 5 }
def values5 : DistinctList Nat := ⟨[1, 2, 3, 4, 5], by decide⟩

example : Finset.card values = 4 := by decide

example : values4[0] = 1 := by decide
example : values4[1] = 2 := by decide
example : values4[2] = 3 := by decide
example : values4[3] = 4 := by decide
example : values4.length = 4 := by decide

example : Fintype.card Bool = 2 := by decide

-- Drop FinEncodable entirely, or keep it as a separate thing
-- FinDenumerable stands alone with everything it needs
-- α is in bijection with Fin n for some n
-- i.e., there exists an indexing Fin n → α that's a bijection


#check Denumerable Bool

instance : FinDenumerable Nat1to5 where
  card          := 5
  encode        := fun n => ⟨n.val - 1, by grind⟩
  decode        := fun i => ⟨i.val + 1, by grind⟩
  decode_encode := fun i => by grind

instance : FinDenumerable Bool where
  card          := 2
  encode        := fun b => if b then ⟨1, by decide⟩ else ⟨0, by decide⟩
  decode        := fun n => n.val == 1
  decode_encode := by decide


-- Probe: find the right lemmas before wiring up fromList
example (x : Bool) (l : List Bool) (h : x ∈ l) : l.idxOf x < l.length := by
  exact List.idxOf_lt_length_of_mem h

example (l : List Bool) (hnd : l.Nodup) (i : Fin l.length) :
    l.idxOf (l.get i) = i.val := by
  exact List.get_idxOf hnd i

-- Build a FinDenumerable from an ordered, duplicate-free list covering all of α
-- @[reducible]
def FinDenumerable.fromList {α : Type*} [DecidableEq α]
    (l : DistinctList α)
    (complete : ∀ x : α, x ∈ l.val)
    : FinDenumerable α :=
    { card          := l.val.length
      encode        := fun x => ⟨l.val.idxOf x, by exact List.idxOf_lt_length_of_mem (complete x)⟩
      decode        := fun i => l.val.get i
      decode_encode := fun i => by
                                  apply Fin.ext
                                  simp only [List.get_eq_getElem]
                                  exact List.get_idxOf (l.property) i
  }

-- Test: build FinDenumerable Bool from a list
example : FinDenumerable Bool :=
  FinDenumerable.fromList ⟨[false, true], by decide⟩ (by decide)

def Nat1to4 := { n : Nat // 1 ≤ n ∧ n ≤ 4 }

def nat1to4 : FinDenumerable Nat1to4 := FinDenumerable.fromList ⟨[1, 2, 3, 4], by decide⟩ (by omega)

-- encode_decode: round-trip from α → Fin → α gives back the original
example : FinDenumerable.decode (FinDenumerable.encode (α := Bool) false) = false := by native_decide
example : FinDenumerable.decode (FinDenumerable.encode (α := Bool) true)  = true  := by native_decide

-- decode_encode: round-trip from Fin → α → Fin gives back the original
example : FinDenumerable.encode (FinDenumerable.decode (α := Bool) ⟨0, by decide⟩) = ⟨0, by decide⟩ := by native_decide
example : FinDenumerable.encode (FinDenumerable.decode (α := Bool) ⟨1, by decide⟩) = ⟨1, by decide⟩ := by native_decide

end DenseCheck
