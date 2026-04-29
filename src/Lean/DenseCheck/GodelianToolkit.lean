import Mathlib.Data.Nat.Pairing

def encodePair (z: Nat) : Nat × Nat :=
    Nat.unpair z

def decodePair (p: Nat × Nat) : Nat :=
    let (x, y) := p
    let m := Nat.max x y
    m * m + m + x - y

universe u


structure NonEmptyList (α : Type u) where
  head : α
  tail : List α

def NonEmptyList.toList (α : Type u) (nel : NonEmptyList α) : List α :=
  nel.head :: nel.tail

def NonEmptyList.length (α : Type u) (nel : NonEmptyList α) : Nat :=
  1 + nel.tail.length

theorem NonEmptyList.length_pos (α : Type u) (nel : NonEmptyList α) : 0 < nel.length  := by
  simp [NonEmptyList.length]
  grind

def NonEmptyList.get {α : Type u} (nel : NonEmptyList α) (n : Fin nel.length) : α :=
  match n with
  | ⟨0, _⟩ => nel.head
  | ⟨Nat.succ k, hk⟩ =>
      nel.tail.get ⟨k, by sorry⟩

def Endofunction (α : Type u) := α → α
def NatDecoder (α : Type u) := Nat → α
def NatEncoder (α : Type u) := α → Nat

def combineChoices
    (α : Type u)
    [Inhabited α]
    (functionList : List ((Nat → α) → Nat → α))
    (h : functionList.length > 0)
     : Nat → α :=

  let length := functionList.length

  let rec chooseFunction (fuel : Nat) (n : Nat) (hf : fuel > 0) : α :=
    -- Can we prove fuel will always be greater than 0?
    match fuel with
    | 0 => default
    | fuel + 1 =>
        let d := n / length
        let r := n % length
        let r : Fin functionList.length := ⟨r, Nat.mod_lt _ (by grind)⟩
        let f := functionList.get r
        f (fun m => chooseFunction fuel m (by sorry)) d

  fun n => chooseFunction n.succ n (by grind)


inductive Expr
  | num : Nat → Expr
  | neg : Expr → Expr
  | add : Expr → Expr → Expr
deriving Inhabited

def Nat.toExpression (x : Nat) : Expr :=

  combineChoices (α := Expr)
    [
      fun f n => Expr.num n,
      fun f n => Expr.neg (f n),
      fun f n => Expr.add (f n) (f n)
    ]
    (by decide)
    x

#eval! Nat.toExpression 345256367
