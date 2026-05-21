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
    Assert.True((distinctCount = sampleSize), $"Expected at least {sampleSize * 9 / 10} distinct values, but got {distinctCount}.")

[<Fact>]
let ``Boolean lists do not repeat in the first large sample`` () =
    let count = DenseCheck.sample<bool list> sampleSize 0 |> List.distinct |> List.length
    Assert.True((count = sampleSize), $"Expected {sampleSize} distinct values, but got {count}.")

[<Fact>]
let ``Enum-like union lists do not repeat in the first large sample`` () =
    let count = DenseCheck.sample<Color list> sampleSize 0 |> List.distinct |> List.length
    Assert.True((count = sampleSize), $"Expected {sampleSize} distinct values, but got {count}.")

[<Fact>]
let ``Recursive boolean expressions do not repeat in the first large sample`` () =
    let count = DenseCheck.sample<BoolExpression> sampleSize 0 |> List.distinct |> List.length
    Assert.True((count = sampleSize), $"Expected {sampleSize} distinct values, but got {count}.")

[<Fact>]
let ``Recursive color expressions do not repeat in the first large sample`` () =
    let count = DenseCheck.sample<ColorExpression> sampleSize 0 |> List.distinct |> List.length
    Assert.True((count = sampleSize), $"Expected {sampleSize} distinct values, but got {count}.")

// [<Fact>]
// let ``Lists of lists of booleans do not repeat in the first large sample`` () =
//     let constr = DenseCheck.makeConstructor (typeof<bool Set>)
    

[<Fact>]
let ``Set of bigints do not repeat in the first large sample`` () =
    let count = DenseCheck.sample<bigint Set> sampleSize 0 |> List.distinct |> List.length
    Assert.True((count = sampleSize), $"Expected {sampleSize} distinct values, but got {count}.")

[<Fact>]
let ``Boolean Tests``() = 
    let boolList = Countable.Primitives.forBool |> Countable.toList
    Assert.True(boolList.IsInfinite, "Expected the countable for bool list to be infinite.")
