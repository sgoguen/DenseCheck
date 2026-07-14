module DenseCheck.Tests.Countable

open DenseCheck
open Xunit

type Color =
    | Red
    | Green
    | Blue

type BoolExpression =
    | ValueTrue
    | ValueFalse
    | Not of BoolExpression
    | And of BoolExpression * BoolExpression
    | Or of BoolExpression * BoolExpression

type ColorExpression =
    | ColorValue of Color
    | Lighten of ColorExpression
    | Darken of ColorExpression
    | Mix of ColorExpression * ColorExpression

let sampleSize = 100

let inline distinctCheck<'T when 'T: equality> sampleSize =
    let values = DenseCheck.sample<'T> sampleSize 0
    let distinctCount = values |> List.distinct |> List.length

    Assert.True(
        (distinctCount = sampleSize),
        $"Expected at least {sampleSize * 9 / 10} distinct values, but got {distinctCount}."
    )

[<Fact>]
let ``Boolean lists do not repeat in the first large sample`` () = distinctCheck<bool list> sampleSize

[<Fact>]
let ``Enum-like union lists do not repeat in the first large sample`` () = distinctCheck<Color list> sampleSize
// let count = DenseCheck.sample<Color list> sampleSize 0 |> List.distinct |> List.length
// Assert.True((count = sampleSize), $"Expected {sampleSize} distinct values, but got {count}.")

[<Fact>]
let ``Recursive boolean expressions do not repeat in the first large sample`` () =
    distinctCheck<BoolExpression> sampleSize

[<Fact>]
let ``Recursive color expressions do not repeat in the first large sample`` () =
    distinctCheck<ColorExpression> sampleSize

[<Fact>]
let ``Set of bigints do not repeat in the first large sample`` () = distinctCheck<bigint Set> sampleSize

[<Fact>]
let ``Boolean Tests`` () = distinctCheck<bool list> sampleSize

// [<Fact>]
// let ``Boolean Sets``() =
//     distinctCheck<bool Set> sampleSize



[<Fact>]
let ``Boolean Sets Size`` () =
    let bools = Countable.Primitives.forBool
    Assert.Equal(2I, bools.DomainSize)
    let boolSets = Countable.toSet bools
    Assert.Equal(4I, boolSets.DomainSize)
