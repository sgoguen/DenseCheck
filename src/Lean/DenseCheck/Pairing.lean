import Mathlib.Tactic
import Mathlib.Data.Nat.Pairing

/-!
# DenseCheck.Pairing

Square-based pairing and unpairing functions for natural numbers,
ported from the F# DenseCheck library.

## The pairing function

The square-based `pair`/`unpair` are provided by `Mathlib.Data.Nat.Pairing`
as `Nat.pair` / `Nat.unpair`. This file re-exports them under the `DenseCheck`
namespace and adds the bit-interleaving variant.

  pair a b = if a < b then b * b + a else a * a + a + b

  (0,0) (0,1) (1,0) (1,1) (0,2) (1,2) (2,0) (2,1) (2,2) ...
    0     1     2     3     4     5     6     7     8    ...
-/

namespace DenseCheck

-- ============================================================
-- Square-based pairing: aliases for Mathlib's Nat.pair/unpair
-- ============================================================

/-- Encode a pair of natural numbers into a single natural number.
    Alias for `Nat.pair` from `Mathlib.Data.Nat.Pairing`. -/
abbrev pair := Nat.pair

/-- Decode a natural number back into a pair of natural numbers.
    Alias for `Nat.unpair` from `Mathlib.Data.Nat.Pairing`. -/
abbrev unpair := Nat.unpair

-- ============================================================
-- Computational tests
-- ============================================================

example : pair 0 0 = 0 := by native_decide
example : pair 0 1 = 1 := by native_decide
example : pair 1 0 = 2 := by native_decide
example : pair 1 1 = 3 := by native_decide
example : pair 0 2 = 4 := by native_decide
example : pair 1 2 = 5 := by native_decide
example : pair 2 0 = 6 := by native_decide
example : pair 2 1 = 7 := by native_decide
example : pair 2 2 = 8 := by native_decide

example : unpair 0 = (0, 0) := by native_decide
example : unpair 1 = (0, 1) := by native_decide
example : unpair 2 = (1, 0) := by native_decide
example : unpair 3 = (1, 1) := by native_decide
example : unpair 4 = (0, 2) := by native_decide
example : unpair 5 = (1, 2) := by native_decide
example : unpair 6 = (2, 0) := by native_decide
example : unpair 7 = (2, 1) := by native_decide
example : unpair 8 = (2, 2) := by native_decide

-- ============================================================
-- Round-trip theorems: direct from Mathlib
-- ============================================================

/-- Unpairing a paired value returns the original pair. -/
theorem unpair_pair (a b : Nat) : unpair (pair a b) = (a, b) :=
  Nat.unpair_pair a b

/-- Pairing an unpaired value returns the original number. -/
theorem pair_unpair (n : Nat) : (let p := unpair n; pair p.1 p.2) = n :=
  Nat.pair_unpair n

/-- `pair` is injective (follows from `unpair_pair`). -/
theorem pair_injective (a₁ b₁ a₂ b₂ : Nat)
    (h : pair a₁ b₁ = pair a₂ b₂) : a₁ = a₂ ∧ b₁ = b₂ :=
  Nat.pair_eq_pair.mp h

-- ============================================================
-- Bit-interleaving pairing (from Pairing.fs)
-- Recursive definitions for clean inductive proofs.
-- ============================================================

/-- Bit-interleaving pair: even bits from `a`, odd bits from `b`.
    Defined recursively on `a + b` for inductive proofs. -/
def bitPair (a b : Nat) : Nat :=
  if a = 0 ∧ b = 0 then 0
  else (a % 2) + 2 * (b % 2) + 4 * bitPair (a / 2) (b / 2)
termination_by a + b
decreasing_by omega

/-- Inverse of `bitPair`: extract even and odd bits. -/
def bitUnpair (n : Nat) : Nat × Nat :=
  if n = 0 then (0, 0)
  else
    let (a', b') := bitUnpair (n / 4)
    (n % 2 + 2 * a', (n / 2) % 2 + 2 * b')
termination_by n
decreasing_by omega

-- ============================================================
-- Specification: bitPair interleaves testBit
-- ============================================================

-- /-- The even bits of `bitPair a b` are the bits of `a`. -/
-- theorem bitPair_testBit_even (a b k : Nat) :
--     (bitPair a b).testBit (2 * k) = a.testBit k := by
--   sorry

-- /-- The odd bits of `bitPair a b` are the bits of `b`. -/
-- theorem bitPair_testBit_odd (a b k : Nat) :
--     (bitPair a b).testBit (2 * k + 1) = b.testBit k := by
--   sorry

-- ============================================================
-- Bit-interleaving computational tests
-- ============================================================

example : bitPair 3 5 = 39 := by native_decide
example : bitUnpair 39 = (3, 5) := by native_decide
example : bitUnpair (bitPair 3 5) = (3, 5) := by native_decide
example : bitUnpair (bitPair 42 17) = (42, 17) := by native_decide
example : bitPair (bitUnpair 100).1 (bitUnpair 100).2 = 100 := by native_decide

-- ============================================================
-- Bit-interleaving round-trip theorems
-- ============================================================

-- theorem bitUnpair_bitPair (a b : Nat) : bitUnpair (bitPair a b) = (a, b) := by
--   sorry

-- theorem bitPair_bitUnpair (n : Nat) :
--     (let p := bitUnpair n; bitPair p.1 p.2) = n := by
--   sorry

end DenseCheck
