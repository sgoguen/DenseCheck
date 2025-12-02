module InstanceCatalog.Tests.Examples.Example1

open InstanceCatalog
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

// We can list all the possible of a type using the InstanceCatalog.
// The InstanceCatalog creates a function that *INDEXES* all possible instances
// of your type.

// It's as if it allows you to construct a finite or infinite set of all the instances
// your type can inhabit and then lets your access those instances using an integer index.
let getAccessRule = InstanceCatalog.getIndex<AccessRule> ()

[<Fact>]
let ``We can enumerate many items`` () =

    // Let's generate the first 30 access rules
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
          And(And(HasRole Admin, HasRole Admin), HasRole Admin)
          Or(And(HasRole Admin, HasRole Admin), HasRole Admin)
          Not(Or(HasRole Admin, HasRole Admin))
          HasRole Viewer
          And(And(HasRole Admin, HasRole Admin), And(HasRole Admin, HasRole Admin))
          Or(And(HasRole Admin, HasRole Admin), And(HasRole Admin, HasRole Admin))
          Not(Not(HasRole Admin))
          HasRole Viewer
          And(HasRole Admin, Or(HasRole Admin, HasRole Admin))
          Or(HasRole Admin, Or(HasRole Admin, HasRole Admin))
          Not(HasRole Editor)
          HasRole Viewer
          And(And(HasRole Admin, HasRole Admin), Or(HasRole Admin, HasRole Admin))
          Or(And(HasRole Admin, HasRole Admin), Or(HasRole Admin, HasRole Admin))
          Not(And(HasRole Admin, And(HasRole Admin, HasRole Admin)))
          HasRole Viewer
          And(Or(HasRole Admin, HasRole Admin), HasRole Admin)
          Or(Or(HasRole Admin, HasRole Admin), HasRole Admin)
          Not(Or(HasRole Admin, And(HasRole Admin, HasRole Admin)))
          HasRole Viewer
          And(Or(HasRole Admin, HasRole Admin), And(HasRole Admin, HasRole Admin)) ]

    Assert.Equal<AccessRule list>(expected, terms)

[<Fact>]
let ``Let's generate a big instance`` () =
    let bigInstance = getAccessRule 123987123098123987324987234I

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

    Assert.Equal<AccessRule>(expected, bigInstance)
