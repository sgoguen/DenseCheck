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
    let terms = [ for i in 0I .. 29I -> getAccessRule i ]

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

Instead of using `FsCheck` random generation or property based testing, we can use the Instance Catalog to enumerate all the possible `AccessRule` instances.

```fsharp