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

let assertFirstSamplesAreDistinct<'T when 'T: equality> sampleSize =
    let values = DenseCheck.sample<'T> sampleSize 0
    let distinctCount = values |> List.distinct |> List.length

    Assert.Equal(sampleSize, distinctCount)

[<Fact>]
let ``Boolean lists do not repeat in the first large sample`` () =
    assertFirstSamplesAreDistinct<bool list> 10000

[<Fact>]
let ``Enum-like union lists do not repeat in the first large sample`` () =
    assertFirstSamplesAreDistinct<Color list> 10000

[<Fact>]
let ``Recursive boolean expressions do not repeat in the first large sample`` () =
    assertFirstSamplesAreDistinct<BoolExpression> 10000

[<Fact>]
let ``Recursive color expressions do not repeat in the first large sample`` () =
    assertFirstSamplesAreDistinct<ColorExpression> 10000
