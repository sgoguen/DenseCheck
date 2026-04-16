import Mathlib.Logic.Denumerable
import Mathlib.Data.Fin.Basic

namespace DenseCheck

-- Drop FinEncodable entirely, or keep it as a separate thing
-- FinDenumerable stands alone with everything it needs
class FinDenumerable (α : Type*) where
  card          : ℕ
  encode        : α → Fin card
  decode        : Fin card → α
  encode_decode : ∀ x, decode (encode x) = x
  decode_encode : ∀ i, encode (decode i) = i

instance : FinDenumerable Bool where
  card          := 2
  encode        := fun b => if b then ⟨1, by decide⟩ else ⟨0, by decide⟩
  decode        := fun n => n.val == 1
  encode_decode := by decide
  decode_encode := by decide

-- encode_decode: round-trip from α → Fin → α gives back the original
example : FinDenumerable.decode (α := Bool) (FinDenumerable.encode false) = false := by native_decide
example : FinDenumerable.decode (α := Bool) (FinDenumerable.encode true)  = true  := by native_decide

-- decode_encode: round-trip from Fin → α → Fin gives back the original
example : FinDenumerable.encode (FinDenumerable.decode (α := Bool) ⟨0, by decide⟩) = ⟨0, by decide⟩ := by native_decide
example : FinDenumerable.encode (FinDenumerable.decode (α := Bool) ⟨1, by decide⟩) = ⟨1, by decide⟩ := by native_decide

end DenseCheck
