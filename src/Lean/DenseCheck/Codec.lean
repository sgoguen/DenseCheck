import Mathlib.Data.Nat.Pairing
import DenseCheck.OrderedNodupList

-- open OrderedNodupList

-- Let's open OrderedNodupList


universe u

structure Codec  (Rep : Type u) (Value : Type u) where
  encode : Value → Rep
  decode : Rep → Value
  decode_encode : ∀ v, decode (encode v) = v
  encode_decode : ∀ r, encode (decode r) = r

def Codec.comp
    {A B C : Type u}
    (ab : Codec B A)
    (bc : Codec C B)
    : Codec C A where
  encode := fun a => bc.encode (ab.encode a)
  decode := fun c => ab.decode (bc.decode c)
  decode_encode := by
    intro a
    simp [ab.decode_encode, bc.decode_encode]
  encode_decode := by
    intro c
    simp [ab.encode_decode, bc.encode_decode]

def Codec.xmap
    {Rep A B : Type u}
    (c : Codec Rep A)
    (toB   : A → B)
    (fromB : B → A)
    (to_from : ∀ b, toB (fromB b) = b)
    (from_to : ∀ a, fromB (toB a) = a)
    : Codec Rep B where
  encode := fun b => c.encode (fromB b)
  decode := fun r => toB (c.decode r)
  decode_encode := by
    intro b
    simp [c.decode_encode, to_from]
  encode_decode := by
    intro r
    simp [c.encode_decode, from_to]


def NatCodec := Codec Nat

def pairCodec : NatCodec (Nat × Nat) where
  encode := fun p => Nat.pair p.1 p.2
  decode := Nat.unpair
  decode_encode := by simp
  encode_decode := by simp

def listCodec : Codec Nat (OrderedNodupList Nat) where
  encode := fun l => l.val.length + 1 + l.val.foldl (fun acc n => acc + n + 1) 0
  decode := sorry
  decode_encode := sorry
  encode_decode := sorry
