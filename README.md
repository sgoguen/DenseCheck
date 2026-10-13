# DenseCheck

DenseCheck is a companion tool for property based testing frameworks like FsCheck, which help you generate test data to validate properties about your code.

### How is DenseCheck different from FsCheck?

* FsCheck generates random instances of your types to test properties
* DenseCheck systematically constructs functions that creates a simple integer to all possible instances of your function, making the entire set of your type denumerable.

### Why use DenseCheck?

* FsCheck's random generation can miss edge cases or important scenarios in your domain model.
* DenseCheck allows you to thoroughly test smaller value spaces in a systematic way random generation cannot.

### Domain Model Checking Made Enumerable!

DenseCheck is a deterministic domain exploration tool for F# that treats your types as sets of the values they inhabit, providing you an infinite and  indexable catalog of values you can enumerate and explore.

## How It Works


### 1. Define a Domain Model or Type

Start with a domain model like this simple role based access control system:

```fsharp
    type Role =
        | Admin
        | Editor
        | Viewer

    type AccessRule =
        | HasRole of Role
        | And of AccessRule * AccessRule
        | Or of AccessRule * AccessRule
        | Not of AccessRule
```

### 2. Ask for a catalog of all possible instances

```fsharp
    let getAccessRule = DenseCheck.getIndex<AccessRule> ()
```

### 3. Enumerate the first 10 instances

```fsharp
    let terms = [ for i in 0I .. 9I -> getAccessRule i ]

    // It will return a list of terms like this:
    let expected =
        [ HasRole Admin
          And(HasRole Admin, HasRole Admin)
          Or(HasRole Admin, HasRole Admin)
          Not(HasRole Admin)
          HasRole Editor
          And(HasRole Admin, And(HasRole Admin, HasRole Admin))
          Or(HasRole Admin, And(HasRole Admin, HasRole Admin))
          Not(And(HasRole Admin, HasRole Admin))
          HasRole Viewer
          And(And(HasRole Admin, HasRole Admin), HasRole Admin) ]
```

### 4. Get a Randomly Large Instance

```fsharp
    let bigRule = getAccessRule 123987123098123987324987234I

    // bigRule will be something like this:
    let expected =
        Or(
            Or(
                Or(
                    Or(HasRole Editor, Not(HasRole Admin)),
                    Not(Not(And(Or(HasRole Admin, HasRole Admin), Or(HasRole Admin, HasRole Admin))))
                ),
                Not(Or(Or(Not(And(HasRole Admin, HasRole Admin)), HasRole Editor), HasRole Viewer))
            ),
            Or(
                Not(Or(HasRole Viewer, HasRole Viewer)),
                Or(
                    And(And(HasRole Admin, And(HasRole Admin, HasRole Admin)), And(HasRole Admin, HasRole Admin)),
                    Or(Not(Or(HasRole Admin, HasRole Admin)), And(HasRole Admin, HasRole Admin))
                )
            )
        )    
```

This is all well and good, but domain models always feel abstract.  It would be nice if we could quickly generate some concrete examples of our `AccessRule` to see what sorts of scenarios our model allows.

Once values are indexable, we can state a property about their behavior and check them in order. Here is a deliberately buggy evaluator: its `Not` case forgets to negate the result.

To run the snippets in this section as one F# script, put `#r "nuget: DenseCheck, 0.1.0"` at the top, then paste the definitions above together with this code and run `dotnet fsi AccessRule.fsx`.

```fsharp
let roles = Set.empty<Role>

let rec evaluate (roles: Set<Role>) (rule: AccessRule) =
    match rule with
    | HasRole role -> Set.contains role roles
    | And (left, right) -> evaluate roles left && evaluate roles right
    | Or (left, right) -> evaluate roles left || evaluate roles right
    | Not rule -> evaluate roles rule // Bug: this should negate the result.

let negationIsComplement rule =
    let expected = not (evaluate roles rule)
    let actual = evaluate roles (Not rule)
    actual = expected

let firstFailure =
    [ 0I .. 1000I ]
    |> List.tryPick (fun index ->
        let rule = getAccessRule index
        if negationIsComplement rule then
            None
        else
            let expected = not (evaluate roles rule)
            let actual = evaluate roles (Not rule)
            Some (index, rule, expected, actual))

match firstFailure with
| Some (index, rule, expected, actual) ->
    printfn "First failure at candidate index %A: Not (%A)" index rule
    printfn "Expected %b; got %b" expected actual
| None ->
    printfn "No failure in the first 1001 candidates"
```

Output:

```text
First failure at candidate index 0: Not (HasRole Admin)
Expected true; got false
```

The property says that negating a rule must flip its Boolean result. DenseCheck supplies candidate rules in index order: index `0` is `HasRole Admin`, so that first candidate exposes the bug for an empty role set. The failing expression is `Not (HasRole Admin)`; it is also AccessRule index `3` in the catalog shown above. The scan checks every smaller candidate before reporting a failure, so this is the first counterexample in the tested prefix. AccessRule is infinite, so checking indices `0` through `1000` does not prove the property for every rule.

The fix is one line: change the `Not` case to `| Not rule -> not (evaluate roles rule)`.

