import Mathlib.Logic.Denumerable
import Mathlib.Data.Fin.Basic

namespace DenseCheck

/-- A finite type with an explicit computable bijection to `Fin card`. -/
class FinEncodable (α : Type*) where
  card   : ℕ
  encode : α → Fin card
  decode : Fin card → α

instance : FinEncodable Bool where
  card   := 2
  encode := fun b => if b then ⟨1, by omega⟩ else ⟨0, by omega⟩
  decode := fun n => n.val == 1

example : (FinEncodable.encode (α := Bool) false) = ⟨0, by decide⟩ := by native_decide
example : (FinEncodable.encode (α := Bool) true)  = ⟨1, by decide⟩ := by native_decide
example : (FinEncodable.decode (α := Bool) ⟨0, by decide⟩) = false := by native_decide
example : (FinEncodable.decode (α := Bool) ⟨1, by decide⟩) = true  := by native_decide


end DenseCheck
