# Domain Modeling Made Denumerable

If you’ve read Scott Wlaschin’s *Domain Modeling Made Functional*, the title of this post probably feels familiar. I’m borrowing it on purpose. That book taught us to take the *shapes* of our problem domain seriously.

But recently, I’ve been obsessed with a specific question: **If our types are just sets of values, why can't we index them?**

Most domain models we build are discriminated unions, records, optional types. They tend to live in a countable space of values. There is no deep mystery preventing us from saying, "Give me the 42nd value of this type." Yet, our testing tools rarely leverage this. We rely on property-based testing to throw random darts at our types, hoping to hit edge cases, and then we rely on "shrinkers" to make sense of the mess.

I wanted something different. I wanted to systematically walk the territory of my domain, one step at a time.

So I built a library called **DenseCheck**. It creates a bijective mapping between your F\# types and the natural numbers. It turns your domain model into a catalog of values that you can browse by index.

**TL;DR:** DenseCheck takes an F# type and gives you a function `bigint -> 'T` that enumerates its values in a dense, repeatable order.  It works for primitives, records, discriminated unions, options, lists, sets, maps, and even recursive types.

## The Problem with Randomness

Let's look at a problem that is annoying to solve with random testing: **Access Control Logic.**

We want to define a recursive set of rules. The danger in these systems is creating a tautology.  This is a rule that looks complex to a human reader but actually allows everyone in.

```fsharp

#r "nuget: DenseCheck, 0.1.0"

open DenseCheck

// Our Domain
type Role = Admin | Editor | Viewer

type AccessRule =
    | Allow
    | Deny
    | HasRole of Role
    | And of AccessRule * AccessRule
    | Or  of AccessRule * AccessRule
    | Not of AccessRule
```

If we use a random generator (like FsCheck), it might hand us a rule like:
`Or(And(HasRole Admin, Allow), Not(HasRole Admin))`

While this example isn't that complex, a random generator will happily produce much more convoluted expressions that are harder to reason about, or it may never find this bug at all.  You have to hope the quick check shrinker can whittle it down to the root cause.

## The DenseCheck Approach

`DenseCheck` doesn't guess. It enumerates. Because it maps integers to values deterministically, Index 0 is always the simplest value, and Index 100 is usually slightly more complex.

We can use this to find the **simplest possible tautologies** in our system. Because `DenseCheck` generates `Sets` and `Lists` just as easily as primitives, we can also generate the *context* (the users) to test against.

```fsharp
// 1. Get our Generator for Rules
// This creates a function: bigint -> AccessRule
let getRule = DenseCheck.getIndex<AccessRule>()

// 2. Generate our Test "Users"

// For now, let's think of a user as the set of Roles they have.
type User = Set<Role>

// We can now ask DenseCheck for the first 100 possible combinations of users (Sets of Roles)
// Since Role only has 3 values, we're more than covered.
let users = DenseCheck.getSetTo<User> 100

// The "Ground Truth" Evaluator
let evaluate (rule: AccessRule) (roles: Set<Role>) : bool =
    let rec eval r =
        match r with
        | Allow -> true
        | Deny -> false
        | HasRole required -> Set.contains required roles
        | And (r1, r2) -> eval r1 && eval r2
        | Or (r1, r2) -> eval r1 || eval r2
        | Not r -> not (eval r)
    eval rule

// We define a Tautology as a rule that returns TRUE for *every* generated user context.
let isTautology rule = 
    users |> List.forall (fun ctx -> evaluate rule ctx)

// And a Contradiction as a rule that returns FALSE for *every* generated user context.
let isContradiction rule = 
    users |> List.forall (fun ctx -> not (evaluate rule ctx))
```

Now, we scan our universe, looking for tautologies and contradictions.

We don't need a random seed; we just check the first 1,000 rules.

```fsharp
printfn "--- Scanning for Logic Bugs ---"

for i in 0I .. 1000I do
    let rule = getRule i
    
    // We ignore the trivial case 'Allow'
    if isTautology rule && rule <> Allow then
        printfn "[%A] Found Complex Tautology: %A" i rule

    // We ignore the trivial case 'Deny'
    if isContradiction rule && rule <> Deny then
        printfn "[%A] Found Complex Contradiction: %A" i rule
```

### Analysing the Results

When you run this, you get immediate insight into the logical structure of your types. You don't get noise; you get the fundamental atoms of failure.

```text
[3] Found Complex Tautology: And (Allow, Allow)
[4] Found Complex Tautology: Or (Allow, Allow)
[5] Found Complex Contradiction: Not Allow
[7] Found Complex Contradiction: And (Allow, Deny)
[8] Found Complex Tautology: Or (Allow, Deny)
[9] Found Complex Tautology: Not Deny
[11] Found Complex Contradiction: And (Deny, Allow)
[12] Found Complex Tautology: Or (Deny, Allow)
[15] Found Complex Contradiction: And (Deny, Deny)
[16] Found Complex Contradiction: Or (Deny, Deny)
[17] Found Complex Contradiction: Not (And (Allow, Allow))
[20] Found Complex Tautology: Or (Allow, HasRole Admin)
[21] Found Complex Contradiction: Not (Or (Allow, Allow))
[23] Found Complex Contradiction: And (Deny, HasRole Admin)
[25] Found Complex Tautology: Not (Not Allow)
```

At index 42, we find `Or (HasRole Admin, Not (HasRole Admin))`.

This is the exact mathematical definition of a tautology. We didn't need to shrink a 50-line random expression to find it. We just counted until we hit it. It was the 42nd simplest thing that could exist in this domain.

## When AI Fails to Help

Many programmers are relying on AI to write a lot of code these days, and I don't know about you, but I've been finding that people often don’t notice when AI generates code with subtle logical flaws.

For example, I asked an AI to generate a simplify function that would reduce AccessRules to their simplest form. The AI produced this code:

```fsharp
// Let's implement a simplify function that reduces our access rule
// to a simpler form.
let simplify = 
    let rec simplifyRec a =
        match a with
        // Double negation
        | Not (Not r) -> simplifyRec r
        // And rules
        
        | And (Allow, r)
        | And (r, Allow) -> simplifyRec r
        | And (Deny, _)
        | And (_, Deny) -> Deny
        // Or rules
        | Or (Deny, r)
        | Or (r, Deny) -> simplifyRec r
        | Or (Allow, _)
        | Or (_, Allow) -> Allow
        // De Morgan
        | Not (And (r1, r2)) ->
            Or (Not (simplifyRec r1), Not (simplifyRec r2)) |> simplifyRec
        | Not (Or (r1, r2)) ->
            And (Not (simplifyRec r1), Not (simplifyRec r2)) |> simplifyRec
        // Push simplification inside
        | And (r1, r2) ->
            let r1' = simplifyRec r1
            let r2' = simplifyRec r2
            if r1' = r1 && r2' = r2 then And (r1', r2') else simplifyRec (And (r1', r2'))
        | Or (r1, r2) ->
            let r1' = simplifyRec r1
            let r2' = simplifyRec r2
            if r1' = r1 && r2' = r2 then Or (r1', r2') else simplifyRec (Or (r1', r2'))
        | Not r ->
            let r' = simplifyRec r
            if r' = r then Not r' else simplifyRec (Not r')
        // Base cases
        | _ -> a
    fun r -> simplifyRec r
```

Does it work?

How do we know if it works or doesn't work?

## Checking Simplification with DenseCheck

We can use `DenseCheck` to validate that our `simplify` function is correct, but we want to make sure we're doing three things:

1. **Preservation of Semantics**: The simplified rule should behave the same as the original rule for all user contexts, otherwise, we've changed the meaning of the rule.
2. **All Tautologies Simplify to 'Allow'**: Any rule that is a tautology should simplify to the `Allow` rule.
3. **All Contradictions Simplify to 'Deny'**: Any rule that is a contradiction should simplify to the `Deny` rule.

First, let's check for preservation of semantics:

```fsharp
let isEquivalent r1 r2 = 
    users |> List.forall (fun ctx -> evaluate r1 ctx = evaluate r2 ctx)
```

Now, we can scan through our rules and validate the simplification:

```fsharp
printfn "--- Verifying Simplification ---"

for i in 0I .. 1000I do
    let rule1 = getRule i
    let rule2 = simplify rule1
    
    // We ignore the trivial case 'Allow'
    if isEquivalent rule1 rule2 = false then
        printfn "[A] Found Non Equivalent Simplification: %A vs %A" rule1 rule2
        
    // We ignore the trivial case 'Allow'
    if isTautology rule2 && rule2 <> Allow then
        printfn "[%A] Found Complex Tautology: %A" i rule2

    // We ignore the trivial case 'Deny'
    if isContradiction rule2 && rule2 <> Deny then
        printfn "[%A] Found Complex Contradiction: %A" i rule2  
```

When I ran this, I found the AI accomplished preservation of semantics, but it failed to simplify all tautologies and contradictions correctly. For example, it produced:

```
[5] Found Complex Contradiction: Not Allow
[9] Found Complex Tautology: Not Deny
[17] Found Complex Contradiction: Or (Not Allow, Not Allow)
[21] Found Complex Contradiction: And (Not Allow, Not Allow)
[33] Found Complex Tautology: Or (Not Allow, Not Deny)
[37] Found Complex Contradiction: And (Not Allow, Not Deny)
[49] Found Complex Tautology: Or (Not Deny, Not Allow)
[53] Found Complex Contradiction: And (Not Deny, Not Allow)
[65] Found Complex Tautology: Or (Not Deny, Not Deny)
[69] Found Complex Tautology: And (Not Deny, Not Deny)
[85] Found Complex Contradiction: And (Not Allow, Not (HasRole Admin))
[97] Found Complex Tautology: Or (Not Deny, Not (HasRole Admin))
[103] Found Complex Contradiction: Not Allow
[105] Found Complex Contradiction: Not Allow
[108] Found Complex Contradiction: Not Allow
[111] Found Complex Contradiction: And (HasRole Admin, Not Allow)
```

Clearly, it forgot that `Not Allow` is just `Deny` and `Not Deny` is just `Allow`.

After I added those two rules, I got this:

```text
--- Verifying Simplification ---
[687] Found Complex Contradiction: And (HasRole Admin, Not (HasRole Admin))
[688] Found Complex Tautology: Or (HasRole Admin, Not (HasRole Admin))
[739] Found Complex Contradiction: And (Not (HasRole Admin), HasRole Admin)
[740] Found Complex Tautology: Or (Not (HasRole Admin), HasRole Admin)
```

It looks like the AI forgot that `Or (A, Not A) = Allow` and `And (A, Not A) = Deny`.

After I added some more rules, I finally got zero failures for the first 1,000 rules!

...but what about the first 10,000 rules, or 100,000?

After expanding my scan to 10,000 rules, I discovered a few more edge cases the AI missed:

```text
[4955] Found Complex Contradiction: And (Not (HasRole Admin), And (HasRole Admin, HasRole Admin))
[4956] Found Complex Tautology: Or (Not (HasRole Admin), And (HasRole Admin, HasRole Admin))
[5095] Found Complex Contradiction: And (And (HasRole Admin, HasRole Admin), Not (HasRole Admin))
[5096] Found Complex Tautology: Or (And (HasRole Admin, HasRole Admin), Not (HasRole Admin))
[5239] Found Complex Contradiction: And (Not (HasRole Admin), Or (HasRole Admin, HasRole Admin))
[5240] Found Complex Tautology: Or (Not (HasRole Admin), Or (HasRole Admin, HasRole Admin))
[5383] Found Complex Contradiction: And (Or (HasRole Admin, HasRole Admin), Not (HasRole Admin))
[5384] Found Complex Tautology: Or (Or (HasRole Admin, HasRole Admin), Not (HasRole Admin))
```

It turns out the AI also forgot that `And (A, A) = A` and `Or (A, A) = A`.

## Reproducible Failures

This approach shines when testing parsers, interpreters, serialization logic, et cetera.

In property-based testing, if a specific seed crashes your parser, you have to hope you logged the seed to reproduce it. With `DenseCheck`, the "seed" is just the index. If index `5239I` crashes your parser, you can store exactly that index and reproduce the failure on any machine, any day, with no extra logging.

## When Not to Use DenseCheck

While `DenseCheck` is a fun tool for generating tests data for smaller domain models, I think it's important to note its limitations.

With DenseCheck, you're almost never doing exhaustive testing. You're testing what you've deemed to be "enough" for your purposes. For large or very complex domains, this only scratches the small-case surface.  However, those small cases are often exactly the ones that get missed.

`DenseCheck` isn't a replacement for `FsCheck`.  I think random testing is essential for exploring the "deep" space of your domain where the index numbers would be too large to reach sequentially, and I see DenseCheck as a nice companion to random testing, and possibly a source of interesting deterministic seeds.

## Conclusion

But if you find yourself being dense about something in your code, or you're being too trusting of the AI-generated code you're tossing into your codebase, maybe enumerative testing à la `DenseCheck` is worth considering.

Happy Advent of F# and Happy Holidays!

## Update

The NuGet package for DenseCheck is available [here](https://www.nuget.org/packages/DenseCheck).

The source code is available in [02-FS-Advent.fsx](./02-FS-Advent.fsx)