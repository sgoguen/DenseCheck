import Mathlib.Tactic

/-!
# DenseCheck.Pairing

Square-based pairing and unpairing functions for natural numbers,
ported from the F# DenseCheck library.

## The pairing function

Given two natural numbers `a` and `b`, we encode them as a single
natural number using a square-based scheme:

  pair a b = if a < b then b * b + a else a * a + a + b

This creates a bijection ℕ × ℕ → ℕ, enumerating pairs along the
L-shaped borders of growing squares:

  (0,0) (0,1) (1,0) (1,1) (0,2) (1,2) (2,0) (2,1) (2,2) ...
    0     1     2     3     4     5     6     7     8    ...

## Leveraging Mathlib

- Uses `Nat.sqrt` from core Lean (with Mathlib lemmas) instead of hand-rolled isqrt
- Uses `Nat.testBit` from core for bit-interleaving specification
- Recursive definitions for `bitPair`/`bitUnpair` to enable inductive proofs
- `omega`, `simp`, `norm_num` from Mathlib for proof automation
-/



namespace DenseCheck

-- ============================================================
-- Square-based pairing (ported from F# MonoPairing)
-- ============================================================

/-- Encode a pair of natural numbers into a single natural number.
    Uses the square-based pairing: the pair `(a, b)` is mapped to
    `b² + a` when `a < b`, and `a² + a + b` otherwise. -/
def pair (a b : Nat) : Nat :=
  if a < b then b * b + a
  else a * a + a + b

/-- Decode a natural number back into a pair of natural numbers.
    Inverts `pair`: uses `Nat.sqrt` from core Lean. -/
def unpair (n : Nat) : Nat × Nat :=
  let s := Nat.sqrt n
  if n - s * s < s then (n - s * s, s)
  else (s, n - s * s - s)

-- ============================================================
-- Computational tests (native_decide handles Nat.sqrt reduction)
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
-- Round-trip theorems
-- (Proofs will use Mathlib's Nat.sqrt_le, Nat.lt_succ_sqrt, omega)
-- ============================================================

/-- Unpairing a paired value returns the original pair. -/
theorem unpair_pair (a b : Nat) : unpair (pair a b) = (a, b) := by
  sorry

/-- Pairing an unpaired value returns the original number. -/
theorem pair_unpair (n : Nat) : (let p := unpair n; pair p.1 p.2) = n := by
  sorry

/-- `pair` is injective (follows from `unpair_pair`). -/
theorem pair_injective (a₁ b₁ a₂ b₂ : Nat)
    (h : pair a₁ b₁ = pair a₂ b₂) : a₁ = a₂ ∧ b₁ = b₂ := by
  have h1 := unpair_pair a₁ b₁
  have h2 := unpair_pair a₂ b₂
  rw [h] at h1
  rw [h1] at h2
  exact Prod.mk.inj h2

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

/-- The even bits of `bitPair a b` are the bits of `a`. -/
theorem bitPair_testBit_even (a b k : Nat) :
    (bitPair a b).testBit (2 * k) = a.testBit k := by
  sorry

/-- The odd bits of `bitPair a b` are the bits of `b`. -/
theorem bitPair_testBit_odd (a b k : Nat) :
    (bitPair a b).testBit (2 * k + 1) = b.testBit k := by
  sorry

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

theorem bitUnpair_bitPair (a b : Nat) : bitUnpair (bitPair a b) = (a, b) := by
  sorry

theorem bitPair_bitUnpair (n : Nat) :
    (let p := bitUnpair n; bitPair p.1 p.2) = n := by
  sorry

end DenseCheck
