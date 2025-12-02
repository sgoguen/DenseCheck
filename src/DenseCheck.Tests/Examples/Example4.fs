namespace DenseCheck

open System.Collections.Generic
open System.Text
open DenseCheck.DenseCheck
open Xunit

module Example4 =

    type boolean =
            | False
            | True
            | Not of boolean 
            | And of boolean Set  //  This is not supported
            | Or of boolean list 

    let pick = getIndex<boolean> ()

    [<Fact>]
    let ``First 1000 should be distinct`` () =
        let alreadyCreated = new Dictionary<boolean, bigint>()
        for i in 0I..1000I do
            let created = pick i
            let added = alreadyCreated.TryAdd(created, i)
            if added = false then
                let foundAt = alreadyCreated[created]
                Assert.Fail($"Duplicate {i} found at {foundAt} for object {created}")

    // Example with Map
    type expr =
        | Var of string
        | Num of int
        | Lookup of Map<string, expr>  // Map support!
        | Add of expr * expr

    let pickExpr = getIndex<expr> ()

    [<Fact>]
    let ``First 1000 expressions should be distinct`` () =
        let alreadyCreated = new Dictionary<expr, bigint>()
        for i in 0I..1000I do
            let created = pickExpr i
            let added = alreadyCreated.TryAdd(created, i)
            if added = false then
                let foundAt = alreadyCreated[created]
                Assert.Fail($"Duplicate {i} found at {foundAt} for object {created}")
                
    type FsPrimitive = 
        | Int
        | Bool
        | Char
        | String
        | Unit
        | Float
        | BigInt

    type FsType =
            | Primitive of FsPrimitive
            | Tuple of FsType list
            | Record of Map<string, FsType>
            
    let pickType = getIndex<FsType> ()
    
    [<Fact(Skip = "TODO - Fix distinctness issue")>]
    let ``First 1000 types should be distinct`` () =
        let alreadyCreated = new Dictionary<FsType, bigint>()
        for i in 0I..1000I do
            let created = pickType i
            let added = alreadyCreated.TryAdd(created, i)
            if added = false then
                let foundAt = alreadyCreated[created]
                Assert.Fail($"Duplicate {i} found at {foundAt} for object {created}")    