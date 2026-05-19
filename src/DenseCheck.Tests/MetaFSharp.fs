module DenseCheck.Tests.MetaFSharp

open System
open DenseCheck
open Xunit

let private finiteObj size label =
    Countable.finiteCountable size (fun n -> box $"{label}-{n}")

let private infiniteObj label =
    Countable.infinite (fun n -> box $"{label}-{n}")

type ExampleUnion =
    | NoneCase
    | IntCase of int
    | StringCase of string

type SingleFieldRecord = { Value: bool }

type MultiFieldRecord =
    { Flag: bool
      Name: string
      Count: int }

[<Fact>]
let ``encodeParts round trips through decodeParts`` () =
    let examples =
        [ []
          [ 0I ]
          [ 3I ]
          [ 0I; 0I ]
          [ 2I; 5I ]
          [ 1I; 2I; 3I ]
          [ 13I; 0I; 21I; 8I ] ]

    for parts in examples do
        Assert.Equal<bigint list>(parts, DenseCheck.decodeParts (DenseCheck.encodeParts parts) parts.Length)

[<Fact>]
let ``decodeParts round trips through encodeParts for positive part counts`` () =
    for partCount in 1..5 do
        for n in 0I..50I do
            Assert.Equal(n, DenseCheck.decodeParts n partCount |> DenseCheck.encodeParts)

[<Fact>]
let ``decodeFiniteListIndices enumerates finite lists by length then digits`` () =
    let decoded =
        [ for n in 0I..7I -> DenseCheck.decodeFiniteListIndices 2I n ]

    let expected =
        [ []
          [ 0I ]
          [ 1I ]
          [ 0I; 0I ]
          [ 1I; 0I ]
          [ 0I; 1I ]
          [ 1I; 1I ]
          [ 0I; 0I; 0I ] ]

    Assert.Equal<bigint list list>(expected, decoded)

[<Fact>]
let ``decodeFiniteListIndices handles singleton domains`` () =
    Assert.Equal<bigint list>([ 0I; 0I; 0I; 0I ], DenseCheck.decodeFiniteListIndices 1I 4I)

[<Fact>]
let ``decodeFiniteListIndices rejects empty domains`` () =
    Assert.Throws<ArgumentException>(fun () -> DenseCheck.decodeFiniteListIndices 0I 0I |> ignore)

[<Fact>]
let ``decodeField wraps finite fields and leaves infinite fields unbounded`` () =
    let finite = finiteObj 2I "finite"
    let infinite = infiniteObj "infinite"

    Assert.Equal("finite-1", DenseCheck.decodeField finite 5I :?> string)
    Assert.Equal("infinite-5", DenseCheck.decodeField infinite 5I :?> string)

[<Fact>]
let ``decodeFields uses mixed radix finite fields and split infinite fields`` () =
    let fields =
        [| finiteObj 2I "left"
           infiniteObj "middle"
           finiteObj 3I "right" |]

    let decoded =
        DenseCheck.decodeFields fields 17I
        |> Array.map (fun value -> value :?> string)

    Assert.Equal<string array>([| "left-1"; "middle-2"; "right-2" |], decoded)

[<Fact>]
let ``makeConstructor decodes Set<int> deterministically for small indices`` () =
    let decodeSet = DenseCheck.getIndex<Set<int>> ()

    Assert.Equal<Set<int>>(Set.empty<int>, decodeSet 0I)
    Assert.Equal<Set<int>>(Set.ofList [ Int32.MinValue ], decodeSet 1I)
    Assert.Equal<Set<int>>(Set.ofList [ Int32.MinValue; Int32.MinValue + 1 ], decodeSet 3I)

[<Fact>]
let ``makeConstructor decodes int list in length then digit order`` () =
    let decodeList = DenseCheck.getIndex<int list> ()

    Assert.Equal<int list>([], decodeList 0I)
    Assert.Equal<int list>([ Int32.MinValue ], decodeList 1I)
    Assert.Equal<int list>([ Int32.MinValue + 1 ], decodeList 2I)

[<Fact>]
let ``makeConstructor decodes Map<int, string> including zero index`` () =
    let decodeMap = DenseCheck.getIndex<Map<int, string>> ()

    Assert.Equal<Map<int, string>>(Map.empty<int, string>, decodeMap 0I)
    Assert.Equal<Map<int, string>>(Map.ofList [ (Int32.MinValue, "a") ], decodeMap 1I)

[<Fact>]
let ``makeConstructor decodes unions with nullary and payload cases`` () =
    let decodeUnion = DenseCheck.getIndex<ExampleUnion> ()

    Assert.Equal(NoneCase, decodeUnion 0I)
    Assert.Equal(IntCase Int32.MinValue, decodeUnion 1I)
    Assert.Equal(StringCase "a", decodeUnion 4294967297I)

[<Fact>]
let ``makeConstructor decodes records with one and many fields`` () =
    let decodeSingle = DenseCheck.getIndex<SingleFieldRecord> ()
    let decodeMulti = DenseCheck.getIndex<MultiFieldRecord> ()

    Assert.Equal({ Value = false }, decodeSingle 0I)
    Assert.Equal({ Value = true }, decodeSingle 1I)
    Assert.Equal(
        { Flag = false
          Name = "a"
          Count = Int32.MinValue },
        decodeMulti 0I
    )
