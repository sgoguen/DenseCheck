import Lean
import Lean.Meta
import Lean.Elab.Command

/-!
# Type Reflection in Lean 4 — A Guided Tour

Lean 4's metaprogramming system lets you *inspect and generate code* based on
types at compile time.  The key insight is that Lean is written in Lean, so
the same data structures the compiler uses internally are available to you in
`#eval`, macros, tactics, and elaborators.

## The ladder of reflection

```
Level 1 │  #check / #print       human-readable, not programmable
Level 2 │  #eval in MetaM        programmatic queries at compile time
Level 3 │  macro / elab          generate syntax / terms from type structure
Level 4 │  deriving handlers     the full auto-derive mechanism
```

We climb the ladder step by step, ending with a macro that auto-generates a
`FinDenumerable` instance for any plain enum.
-/

-- ============================================================
-- § 1  The Environment — your compile-time type dictionary
-- ============================================================

/-
Every top-level name lives in the `Lean.Environment`.
`Lean.ConstantInfo` is a sum type covering every kind of constant:

  | defnInfo   (DefinitionVal)    -- def / noncomputable def
  | axiomInfo  (AxiomVal)         -- axiom
  | thmInfo    (TheoremVal)       -- theorem
  | opaqueInfo (OpaqueVal)        -- opaque
  | quotInfo   (QuotVal)          -- Quot primitives
  | inductInfo (InductiveVal)     -- inductive type
  | ctorInfo   (ConstructorVal)   -- constructor
  | recInfo    (RecursorVal)      -- recursor

`Lean.getEnv` (available in MetaM) hands you the current environment.
-/

open Lean
open Meta

-- #check Lean.getEnv

-- def x := show MetaM _ from do
--   -- let env ← getEnv
--   -- let some ci := env.find? ``Nat | throwError "not found"
--   -- return toString (← ppExpr ci.type)
--   return "Hello, MetaM!"


-- -- Look up `Nat` and see that its *type* is `Type`
-- #eval show MetaM _ from do
--   let env ← getEnv
--   let some ci := env.find? ``Nat | throwError "not found"
--   return toString (← ppExpr ci.type)
--   -- ↪ "Type"

-- open Lean Meta in
-- -- Look up `Nat.add` and see its full type
-- #eval show MetaM _ from do
--   let env ← getEnv
--   let some ci := env.find? ``Nat.add | throwError "not found"
--   return toString (← ppExpr ci.type)
--   -- ↪ "Nat → Nat → Nat"


-- -- ============================================================
-- -- § 2  Structures — reading field names
-- -- ============================================================

-- -- A test structure we'll inspect
-- structure Person where
--   name : String
--   age  : Nat
--   score : Float

-- open Lean in
-- -- `getStructureFields` returns the Array of field Names
-- #eval show MetaM _ from do
--   let env ← getEnv
--   let fields := getStructureFields env ``Person
--   return fields.toList.map toString
--   -- ↪ ["name", "age", "score"]

-- open Lean in
-- -- `getStructureInfo?` gives deeper detail: parent structures, universe, …
-- #eval show MetaM _ from do
--   let env ← getEnv
--   let some si := getStructureInfo? env ``Person | throwError "not a structure"
--   -- si.fieldNames  : Array Name
--   -- si.parents     : Array Name   (inherited structures)
--   -- si.numParams   : Nat
--   return (si.fieldNames.toList.map toString,
--           si.parents.toList.map toString)
--   -- ↪ (["name", "age", "score"], [])


-- -- ============================================================
-- -- § 3  Inductive types — enumerating constructors
-- -- ============================================================

-- /-
-- `InductiveVal` is the payload of `.inductInfo`.  Key fields:

--   iv.name       : Name           -- the type name
--   iv.ctors      : List Name      -- fully-qualified constructor names
--   iv.numParams  : Nat            -- number of type parameters
--   iv.numIndices : Nat            -- number of indices
--   iv.isRec      : Bool           -- is it recursive?
-- -/

-- open Lean in
-- #eval show MetaM _ from do
--   let env ← getEnv
--   let some (.inductInfo iv) := env.find? ``Bool | throwError "not inductive"
--   return iv.ctors.map toString
--   -- ↪ ["Bool.false", "Bool.true"]

-- -- A reusable helper
-- open Lean in
-- def getCtorNames (typeName : Name) : MetaM (List Name) := do
--   let env ← getEnv
--   let some (.inductInfo iv) := env.find? typeName
--     | throwError "{typeName} is not an inductive type"
--   return iv.ctors

-- #eval getCtorNames ``Bool          -- [Bool.false, Bool.true]
-- #eval getCtorNames ``Option        -- [Option.none, Option.some]
-- #eval getCtorNames ``List          -- [List.nil, List.cons]
-- #eval getCtorNames ``Nat           -- [Nat.zero, Nat.succ]
-- #eval getCtorNames ``Array         -- [Array.mk, Array.mkEmpty]
-- #eval getCtorNames ``Person        -- [`Person.mk]
-- #eval getCtorNames ``And           -- [And.intro]
-- #eval getCtorNames ``Or            -- [Or.inl, Or.inr]
-- #eval getCtorNames ``Expr           -- [Expr.bvar, Expr.fvar, Expr.mvar, Expr.sort, Expr.const, Expr.app, Expr.lam, Expr.forallE, Expr.letE, Expr.lit, Expr.mdata, Expr.proj]






-- -- ============================================================
-- -- § 4  Constructor fields — what does each ctor accept?
-- -- ============================================================

-- /-
-- `ConstructorVal` is the payload of `.ctorInfo`.  Key fields:

--   ci.name       : Name   -- fully qualified ctor name
--   ci.induct     : Name   -- the parent inductive type
--   ci.cidx       : Nat    -- 0-based index within the inductive
--   ci.numParams  : Nat    -- shared type parameters (same as iv.numParams)
--   ci.numFields  : Nat    -- fields *specific to this constructor*
--   ci.type       : Expr   -- full type signature as a Lean expression
-- -/

-- open Lean Meta in
-- def describeCtors (typeName : Name) : MetaM String := do
--   let ctorNames ← getCtorNames typeName
--   let mut lines : Array String := #[]
--   for ctorName in ctorNames do
--     let some (.ctorInfo ci) := (← getEnv).find? ctorName | continue
--     let pp ← ppExpr ci.type
--     lines := lines.push s!"  {ctorName} ({ci.numFields} fields) : {pp}"
--   return "\n".intercalate lines.toList

-- #eval describeCtors ``Bool
-- #eval describeCtors ``Option
-- #eval describeCtors ``Nat
-- #eval describeCtors ``List


-- -- ============================================================
-- -- § 5  Lean.Expr — the AST that represents every type and term
-- -- ============================================================

-- /-
-- `Lean.Expr` is the core AST.  Its constructors:

--   .bvar n            -- bound variable (de Bruijn index n)
--   .fvar id           -- free variable
--   .mvar id           -- metavariable (a "hole")
--   .sort u            -- Sort u  (Prop = Sort 0, Type = Sort 1, …)
--   .const n us        -- named constant applied to universe levels
--   .app f a           -- function application  f a
--   .lam n t body bi   -- λ (n : t), body
--   .forallE n t b bi  -- ∀ (n : t), body   (also the type of →)
--   .letE n t v body   -- let n : t := v; body
--   .lit l             -- literal  (Nat literal or String literal)
--   .mdata d e         -- metadata annotation (source info, etc.)
--   .proj n i e        -- structure field projection  e.field_i

-- Useful predicates / destructors:
--   e.isForall         -- is this a ∀ / → ?
--   e.isLambda         -- is this a λ ?
--   e.isApp            -- is this an application?
--   e.isConst          -- is this a constant?
--   e.constName!       -- extract the Name from a `.const`
--   e.getAppFn         -- the head function of an application
--   e.getAppArgs       -- the arguments of an application
-- -/

-- open Lean Meta in
-- -- See List.cons as an Expr
-- #eval show MetaM _ from do
--   let env ← getEnv
--   let some (.ctorInfo ci) := env.find? ``List.cons | throwError ""
--   -- Reduce to weak-head normal form to unfold definitions
--   let t ← whnf ci.type
--   return toString (← ppExpr t)
--   -- ↪ "{α : Type u_1} → α → List α → List α"

-- open Lean Meta in
-- -- Walk a ∀ chain and collect argument types
-- def collectArgs (e : Expr) : MetaM (List String) := do
--   match e with
--   | .forallE _ t body _ =>
--       let rest ← collectArgs body
--       return (toString (← ppExpr t)) :: rest
--   | _ => return []

-- open Lean Meta in
-- #eval show MetaM _ from do
--   let env ← getEnv
--   let some (.ctorInfo ci) := env.find? ``List.cons | throwError ""
--   collectArgs ci.type
--   -- ↪ ["?α", "?α", "List ?α"]   (roughly)


-- -- ============================================================
-- -- § 6  Plain enums — a useful classification for DenseCheck
-- -- ============================================================

-- /-
-- A "plain enum" is an inductive type where every constructor has zero fields.
-- These are trivially enumerable: we just index the constructor list.
-- -/

-- inductive Color where | Red | Green | Blue
--   deriving Repr

-- open Lean in
-- def isPlainEnum (typeName : Name) : MetaM Bool := do
--   let env ← getEnv
--   let some (.inductInfo iv) := env.find? typeName | return false
--   for ctorName in iv.ctors do
--     let some (.ctorInfo ci) := env.find? ctorName | return false
--     if ci.numFields != 0 then return false
--   return true

-- #eval isPlainEnum ``Color    -- true
-- #eval isPlainEnum ``Bool     -- true
-- #eval isPlainEnum ``Nat      -- false  (Nat.succ takes a Nat)
-- #eval isPlainEnum ``Option   -- false  (Option.some takes a value)
-- #eval isPlainEnum ``List     -- false


-- -- ============================================================
-- -- § 7  A custom command: #reflectType
-- -- ============================================================

-- /-
-- `elab` lets you define new *commands* (top-level statements).
-- This one accepts a type name and pretty-prints everything we know about it.
-- -/

-- open Lean Meta Elab Command in
-- elab "#reflectType " t:ident : command => do
--   let typeName := t.getId
--   let env ← getEnv
--   let some ci := env.find? typeName
--     | throwError "Unknown name: {typeName}"
--   -- Structures are stored as .inductInfo — check separately
--   if let some si := getStructureInfo? env typeName then
--     let parents := si.parentInfo.toList.map (fun n => n.structName)
--     let parentsStr := if parents.isEmpty then "none" else toString (parents.toList)
--     logInfo m!"Structure: {typeName}\n\
--                 fields = {si.fieldNames}\n\
--                 parents = {parentsStr}"
--   else
--   match ci with
--   | .inductInfo iv =>
--       logInfo m!"Inductive: {typeName}\n\
--                   numParams  = {iv.numParams}\n\
--                   numIndices = {iv.numIndices}\n\
--                   isRec      = {iv.isRec}\n\
--                   ctors      = {iv.ctors}"
--   | .defnInfo dv =>
--       logInfo m!"Definition: {typeName}\n\
--                   type  = {← ppExpr dv.type}\n\
--                   value = {← ppExpr dv.value}"
--   | _ =>
--       logInfo m!"{typeName} : {← ppExpr ci.type}"

-- #reflectType Bool
-- #reflectType Nat
-- #reflectType List
-- #reflectType Person
-- #reflectType Color


-- -- ============================================================
-- -- § 8  Code generation — producing syntax from type structure
-- -- ============================================================

-- /-
-- The real power of reflection is *generating code*.
-- `Lean.Syntax` is the AST for surface syntax, and `Lean.Meta.mkAppN` etc.
-- let you build terms directly.

-- For learning, the easiest approach is to build a `String` of source code and
-- `logInfo` it so you can copy-paste.  This is what `#showEnumInstance` below
-- does: it prints a ready-to-use `FinDenumerable` instance for any plain enum.

-- (FinDenumerable is defined in Countable.lean — we reference it by its
-- structure field names here without importing it, for illustration.)
-- -/

-- open Lean Meta Elab Command in
-- elab "#showEnumInstance " t:ident : command => do
--   let typeName := t.getId
--   let env ← getEnv
--   -- Validate: must be a plain enum
--   let some (.inductInfo iv) := env.find? typeName
--     | throwError "{typeName} is not an inductive type"
--   for ctorName in iv.ctors do
--     let some (.ctorInfo ci) := env.find? ctorName | throwError "missing ctor"
--     if ci.numFields != 0 then
--       throwError "{ctorName} has {ci.numFields} fields — \
--                   only plain enums are supported"
--   -- Generate the match arms
--   let n := iv.ctors.length
--   let encodeArms ← iv.ctors.enum.mapM fun (i, c) => do
--     let short := c.toString.splitOn "." |>.getLast!
--     return s!"    | .{short} => ⟨{i}, by omega⟩"
--   let decodeArms ← iv.ctors.enum.mapM fun (i, c) => do
--     let short := c.toString.splitOn "." |>.getLast!
--     let pat   := if i < n - 1 then s!"⟨{i}, _⟩" else "⟨_, _⟩"
--     return s!"    | {pat} => .{short}"
--   -- Assemble the instance text
--   let src :=
--     s!"instance : FinDenumerable {typeName} where\n" ++
--     s!"  card          := {n}\n" ++
--     s!"  encode        := fun c => match c with\n" ++
--     (encodeArms.map (· ++ "\n") |> String.join) ++
--     s!"  decode        := fun i => match i with\n" ++
--     (decodeArms.map (· ++ "\n") |> String.join) ++
--     s!"  decode_encode := by decide"
--   logInfo src

-- -- Try it on our types:
-- #showEnumInstance Color
-- #showEnumInstance Bool


-- -- ============================================================
-- -- § 9  Next steps
-- -- ============================================================

-- /-
-- From here, the natural progression for DenseCheck is:

-- 1. **Term-mode code generation** — instead of printing a String, use
--    `Lean.Elab.Term.mkAppM`, `Lean.Meta.mkLambdaFVars`, etc. to build actual
--    `Expr` values and synthesise real instances.

-- 2. **`deriving` handlers** — register a handler with
--    `Lean.Elab.Deriving.registerDerivingHandler` so users can write
--    `deriving FinDenumerable` on their types.

-- 3. **Recursive types** — product types (structs), sum types, and recursive
--    types like `List` need pairing functions (see Pairing.lean) to compose
--    their encoders.

-- 4. **Universe polymorphism** — `Lean.Level` represents universe variables;
--    use `Lean.mkLevelSucc`, `Lean.mkLevelMax`, etc. when generating
--    universe-polymorphic code.

-- Useful references:
-- - *Metaprogramming in Lean 4* (online book): https://leanprover-community.github.io/lean4-metaprogramming-book/
-- - `Lean.Elab.Deriving.BEq` in the Lean 4 source — a simple real deriving handler
-- - Mathlib's `Mathlib.Tactic.DeriveFintype` for a production-grade example
-- -/
