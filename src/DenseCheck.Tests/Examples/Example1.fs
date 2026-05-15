module DenseCheck.Tests.Examples.Example1

open DenseCheck
open Xunit

// Let's imagine we were working on a domain model for a role based access control system.
// We might start with a simple model like so:

type Role =
    | Admin
    | Editor
    | Viewer

type AccessRule =
    | HasRole of Role
    | And of AccessRule * AccessRule
    | Or of AccessRule * AccessRule
    | Not of AccessRule

// We can list all the possible of a type using the DenseCheck.
// The DenseCheck creates a function that *INDEXES* all possible instances
// of your type.

// It's as if it allows you to construct a finite or infinite set of all the instances
// your type can inhabit and then lets your access those instances using an integer index.
let getAccessRule = DenseCheck.getIndex<AccessRule> ()

[<Fact>]
let ``We can enumerate many items`` () =

    // Let's generate the first 30 access rules
    let terms = [ for i in 0I .. 9I -> getAccessRule i ]

    // It will return a list of terms like this:
    let expected =
        [ HasRole Admin
          HasRole Editor
          HasRole Viewer
          And(HasRole Admin, HasRole Admin)
          Or(HasRole Admin, HasRole Admin)
          Not(HasRole Admin)
          And(HasRole Admin, HasRole Editor)
          Or(HasRole Admin, HasRole Editor)
          Not(HasRole Editor)
          And(HasRole Editor, HasRole Admin) ]

    Assert.Equal<AccessRule list>(expected, terms)

[<Fact>]
let ``Let's generate a big instance`` () =
    let bigInstance = getAccessRule 123987123098123987324987234I

    let expected =
        Not(
            Or(
                Or(
                    Or(
                        And(Not(HasRole Admin), Not(HasRole Admin)),
                        And(Or(HasRole Editor, HasRole Editor), Not(HasRole Editor))
                    ),
                    Or(
                        Not(And(HasRole Viewer, Not(HasRole Editor))),
                        Not(Or(Or(HasRole Admin, HasRole Admin), Or(HasRole Admin, HasRole Editor)))
                    )
                ),
                Or(
                    And(
                        Or(HasRole Viewer, Or(HasRole Editor, HasRole Admin)),
                        Not(Or(HasRole Editor, Not(HasRole Editor)))
                    ),
                    Not(
                        Or(
                            And(Or(HasRole Admin, HasRole Editor), Or(HasRole Editor, HasRole Admin)),
                            And(Or(HasRole Admin, HasRole Admin), And(HasRole Editor, HasRole Admin))
                        )
                    )
                )
            )
        )

    Assert.Equal<AccessRule>(expected, bigInstance)
