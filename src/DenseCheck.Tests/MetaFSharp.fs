namespace DenseCheck

open DenseCheck

module DenseCheck =

    open FSharp.Reflection
    open GodelianTooklit
    open System
    open Functions
    open Nat.Mappings

    /// Given an integer and a count, decode the integer into a list of 'count' integers
    let rec decodeParts (n: bigint) (k: int) : bigint list =
        if k <= 0 then
            []
        elif k = 1 then
            [ n ]
        else
            let a, b = encodePair n
            a :: decodeParts b (k - 1)

    /// Encode a list of integers into a single integer (inverse of decodeParts)
    let rec encodeParts (parts: bigint list) : bigint =
        match parts with
        | [] -> 0I
        | [ n ] -> n
        | head :: rest -> decodePair (head, encodeParts rest)

    //  Convert a bigint to a variable name using letters
    //  a, b, ..., aa, ab, ..., ba, bb, ...
    let toVarName (n: bigint) : string =
        let rec toVarName' n acc =
            if n < 0I then
                acc
            else
                let charCode = int (n % 26I) + int 'a'
                let newChar = char charCode
                toVarName' (n / 26I - 1I) (string newChar + acc)

        toVarName' n ""

    // let (|Primitive|Set|List|Map|Union|Record|) (t:Type) =
    //     if t = typeof<string> then
    //         Primitive(t)
    //     elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Set<_>> then
    //         let elementType = t.GetGenericArguments()[0]
    //         let elementConstructor = lazy (recMake elementType)            
    //         Set(t)
    //     elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>> then
    //         List(t)
    //     elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Map<_, _>> then
    //         Map(t)
    //     elif FSharpType.IsUnion(t) then
    //         Union(t)
    //     elif FSharpType.IsRecord(t) then
    //         Record(t)
    //     else
    //         failwithf $"Type %A{t} is not supported by the Godelian constructor"

    let infinite<'a> (f: bigint -> 'a) : Countable<'a> =
        { new Countable<'a> with
            member _.Decode n = f n
            // member _.Encode v = failwith "Not implemented"
            member _.IsInfinite = true
            member _.DomainSize = -1I }

    let finiteCountable<'a> (size: bigint) (f: bigint -> 'a) : Countable<'a> =
        { new Countable<'a> with
            member _.Decode n = f n
            // member _.Encode v = failwith "Not implemented"
            member _.IsInfinite = false
            member _.DomainSize = size }

    let createCountable (recMake: Type -> Countable<'obj>) (t: Type) : Countable<'obj> =
        if t = typeof<string> then
            infinite <| fun n -> box (toVarName (n))
        else if t = typeof<int> then
            finiteCountable (bigint (int System.Int32.MaxValue)) <| fun n -> box (int n)
        elif t = typeof<bool> then
            // For booleans, we might use even/odd as false/true.
             finiteCountable 2I <| fun n -> box ((n % 2I) = 0I)
        elif t = typeof<bigint> then
            infinite <| fun n -> box n
        elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Set<_>> then
            // For Set<T>, use nat2set to get Set<bigint>, then map each element through the constructor for T
            let elementType = t.GetGenericArguments()[0]
            let elementConstructor = lazy (recMake elementType)

            infinite <| fun (n: bigint) ->
                let indexSet = Pairing.nat2Set n
                // Create a typed array of elements
                let typedArray = System.Array.CreateInstance(elementType, indexSet.Count)

                indexSet
                |> Seq.iteri (fun i idx ->
                    let element = elementConstructor.Value.Decode idx
                    typedArray.SetValue(element, i))
                // Use reflection to call Set.ofArray
                let setType =
                    typedefof<Set<_>>.Assembly.GetType("Microsoft.FSharp.Collections.SetModule")

                let ofArrayMethod =
                    setType.GetMethod("OfArray").MakeGenericMethod([| elementType |])

                ofArrayMethod.Invoke(null, [| typedArray |])
        elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>> then
            // For list<T>, use nat2unorderedlist to get list<bigint>, then map each element through the constructor for T
            let elementType = t.GetGenericArguments()[0]
            let elementConstructor = lazy (recMake elementType)

            infinite <| fun (n: bigint) ->
                let indexList = nat2unorderedlist n
                // Create a typed array of elements
                let typedArray = System.Array.CreateInstance(elementType, indexList.Length)

                indexList
                |> List.iteri (fun i idx ->
                    let element = elementConstructor.Value.Decode idx
                    typedArray.SetValue(element, i))
                // Use reflection to call List.ofArray
                let listModule =
                    typedefof<list<_>>.Assembly.GetType("Microsoft.FSharp.Collections.ListModule")

                let ofArrayMethod =
                    listModule.GetMethod("OfArray").MakeGenericMethod([| elementType |])

                ofArrayMethod.Invoke(null, [| typedArray |])
        elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Map<_, _>> then
            // For Map<K,V>:
            // 1. Split n into (domainNat, valuesNat) via pairing
            // 2. Decode domain as Set<bigint> (key indices)
            // 3. Decode values as List<bigint> using decodeParts
            // 4. Zip keys and values together into Map
            let typeArgs = t.GetGenericArguments()
            let keyType = typeArgs[0]
            let valueType = typeArgs[1]
            let keyConstructor = lazy (recMake keyType)
            let valueConstructor = lazy (recMake valueType)

            infinite <| fun (n: bigint) ->
                // Special case: n=0 maps to empty map
                if n = 0I then
                    let mapModule =
                        typedefof<Map<_, _>>.Assembly.GetType("Microsoft.FSharp.Collections.MapModule")

                    let ofListMethod =
                        mapModule.GetMethod("OfList").MakeGenericMethod([| keyType; valueType |])

                    let emptyList =
                        let tupleType = typedefof<_ * _>.MakeGenericType([| keyType; valueType |])
                        let listType = typedefof<list<_>>.MakeGenericType([| tupleType |])
                        FSharpValue.MakeUnion(FSharpType.GetUnionCases(listType)[0], [||]) // Empty list

                    ofListMethod.Invoke(null, [| emptyList |])
                else
                    // For n > 0: decode (n-1) to ensure domainNat > 0 for non-empty maps
                    let domainNat, valuesNat = encodePair (n - 1I)
                    let domainSet = Pairing.nat2Set (domainNat + 1I) // Ensure at least one key
                    let sortedDomainList = domainSet |> Set.toList |> List.sort
                    let size = List.length sortedDomainList
                    let valueIndices = decodeParts valuesNat size

                    // Create arrays for keys and values
                    let keysArray = System.Array.CreateInstance(keyType, size)
                    let valuesArray = System.Array.CreateInstance(valueType, size)

                    sortedDomainList
                    |> List.iteri (fun i keyIdx ->
                        let key = keyConstructor.Value.Decode keyIdx
                        keysArray.SetValue(key, i))

                    valueIndices
                    |> List.iteri (fun i valueIdx ->
                        let value = valueConstructor.Value.Decode valueIdx
                        valuesArray.SetValue(value, i))

                    // Create typed array of tuples
                    let tupleType = typedefof<_ * _>.MakeGenericType([| keyType; valueType |])
                    let tuplesArray = System.Array.CreateInstance(tupleType, size)

                    for i in 0 .. size - 1 do
                        let key = keysArray.GetValue(i)
                        let value = valuesArray.GetValue(i)
                        let tuple = FSharpValue.MakeTuple([| key; value |], tupleType)
                        tuplesArray.SetValue(tuple, i)

                    // Convert array to list, then to Map
                    let listModule =
                        typedefof<list<_>>.Assembly.GetType("Microsoft.FSharp.Collections.ListModule")

                    let ofArrayMethod =
                        listModule.GetMethod("OfArray").MakeGenericMethod([| tupleType |])

                    let pairsList = ofArrayMethod.Invoke(null, [| tuplesArray |])

                    let mapModule =
                        typedefof<Map<_, _>>.Assembly.GetType("Microsoft.FSharp.Collections.MapModule")

                    let ofListMethod =
                        mapModule.GetMethod("OfList").MakeGenericMethod([| keyType; valueType |])

                    ofListMethod.Invoke(null, [| pairsList |])
        elif FSharpType.IsUnion(t) then
            // For union types, we need to handle nullary cases (0 fields) specially
            // to avoid duplicates. We reserve the first K natural numbers for K nullary cases,
            // then use the pairing function for non-nullary cases.
            let cases = FSharpType.GetUnionCases(t)

            // Identify nullary and non-nullary cases
            let nullaryIndices =
                cases
                |> Array.mapi (fun i c -> (i, c.GetFields().Length = 0))
                |> Array.filter snd
                |> Array.map fst

            let nonNullaryIndices =
                cases
                |> Array.mapi (fun i c -> (i, c.GetFields().Length > 0))
                |> Array.filter snd
                |> Array.map fst

            let caseConstructors =
                lazy
                    cases
                    |> Array.map (fun unionCase ->
                        let fields = unionCase.GetFields()
                        let fieldConstructors = fields |> Array.map (fun f -> recMake f.PropertyType)

                        fun (n: bigint) ->
                            if fieldConstructors.Length = 0 then
                                // Nullary case: no fields.
                                FSharpValue.MakeUnion(unionCase, [||])
                            elif fieldConstructors.Length = 1 then
                                // Single field: pass the entire number.
                                let fieldVal = fieldConstructors[0].Decode n
                                FSharpValue.MakeUnion(unionCase, [| fieldVal |])
                            else
                                // Multiple fields: split n into as many parts as needed.
                                let parts = decodeParts n fieldConstructors.Length
                                let fieldVals = fieldConstructors |> Array.mapi (fun i cons -> cons.Decode parts[i])
                                FSharpValue.MakeUnion(unionCase, fieldVals))

            infinite <| fun (n: bigint) ->
                let caseConstructors = caseConstructors.Value
                let len = Array.length caseConstructors

                if len = 1 then
                    // Only one case, pass entire number to it
                    caseConstructors[0]n
                elif nullaryIndices.Length = 0 then
                    // No nullary cases, use DivRem (correct enumeration)
                    let d, r = bigint.DivRem(n, bigint len)
                    caseConstructors[int r]d
                elif nonNullaryIndices.Length = 0 then
                    // All cases are nullary - this is a FINITE type with exactly len distinct values
                    // Only map 0..len-1 to the cases, anything beyond is invalid
                    let caseIndex = int n

                    if caseIndex < len then
                        caseConstructors[caseIndex]0I
                    else
                        // This is beyond the finite range - treat as out of bounds
                        // Return the last case to maintain totality, but this indicates
                        // the parent encoding should account for finite field types
                        caseConstructors[len - 1]0I
                else
                    // Mixed nullary and non-nullary cases
                    // Reserve first K numbers for K nullary cases
                    let nullaryCount = bigint nullaryIndices.Length

                    if n < nullaryCount then
                        // Use one of the nullary cases
                        caseConstructors[nullaryIndices[int n]]0I
                    else
                        // Use DivRem for non-nullary cases
                        let n' = n - nullaryCount
                        let nonNullaryCount = bigint nonNullaryIndices.Length
                        let d, r = bigint.DivRem(n', nonNullaryCount)
                        caseConstructors[nonNullaryIndices[int r]]d
        elif FSharpType.IsRecord(t) then
            // For records, build a constructor that decodes each field.

            let fieldConstructors =
                lazy
                    let fields = FSharpType.GetRecordFields(t)
                    fields |> Array.map (fun f -> recMake f.PropertyType)

            infinite <| fun (n: bigint) ->
                let fieldConstructors = fieldConstructors.Value

                if fieldConstructors.Length = 0 then
                    FSharpValue.MakeRecord(t, [||])
                elif fieldConstructors.Length = 1 then
                    let fieldVal = fieldConstructors[0].Decode n
                    FSharpValue.MakeRecord(t, [| fieldVal |])
                else
                    let parts = decodeParts n fieldConstructors.Length
                    let fieldVals = fieldConstructors |> Array.mapi (fun i cons -> cons.Decode parts[i])
                    FSharpValue.MakeRecord(t, fieldVals)
        else
            failwithf $"Type %A{t} is not supported by the Godelian constructor"        
    
    // /// Recursively build a Gödelian constructor for any supported type.
    // /// The returned function takes a bigint and produces an instance boxed as obj.
    // let createGodelianConstructorForType (recMake: Type -> InfCtor<'obj>) (t: Type) : InfCtor<'obj> =
    //     if t = typeof<string> then
    //         fun n -> box (toVarName (n))
    //     else if t = typeof<int> then
    //         fun n -> box (int n)
    //     elif t = typeof<bool> then
    //         // For booleans, we might use even/odd as false/true.
    //         fun n -> box ((n % 2I) = 0I)
    //     elif t = typeof<bigint> then
    //         fun n -> box n
    //     elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Set<_>> then
    //         // For Set<T>, use nat2set to get Set<bigint>, then map each element through the constructor for T
    //         let elementType = t.GetGenericArguments()[0]
    //         let elementConstructor = lazy (recMake elementType)

    //         fun (n: bigint) ->
    //             let indexSet = Pairing.nat2Set n
    //             // Create a typed array of elements
    //             let typedArray = System.Array.CreateInstance(elementType, indexSet.Count)

    //             indexSet
    //             |> Seq.iteri (fun i idx ->
    //                 let element = elementConstructor.Value idx
    //                 typedArray.SetValue(element, i))
    //             // Use reflection to call Set.ofArray
    //             let setType =
    //                 typedefof<Set<_>>.Assembly.GetType("Microsoft.FSharp.Collections.SetModule")

    //             let ofArrayMethod =
    //                 setType.GetMethod("OfArray").MakeGenericMethod([| elementType |])

    //             ofArrayMethod.Invoke(null, [| typedArray |])
    //     elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>> then
    //         // For list<T>, use nat2unorderedlist to get list<bigint>, then map each element through the constructor for T
    //         let elementType = t.GetGenericArguments()[0]
    //         let elementConstructor = lazy (recMake elementType)

    //         fun (n: bigint) ->
    //             let indexList = nat2unorderedlist n
    //             // Create a typed array of elements
    //             let typedArray = System.Array.CreateInstance(elementType, indexList.Length)

    //             indexList
    //             |> List.iteri (fun i idx ->
    //                 let element = elementConstructor.Value idx
    //                 typedArray.SetValue(element, i))
    //             // Use reflection to call List.ofArray
    //             let listModule =
    //                 typedefof<list<_>>.Assembly.GetType("Microsoft.FSharp.Collections.ListModule")

    //             let ofArrayMethod =
    //                 listModule.GetMethod("OfArray").MakeGenericMethod([| elementType |])

    //             ofArrayMethod.Invoke(null, [| typedArray |])
    //     elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Map<_, _>> then
    //         // For Map<K,V>:
    //         // 1. Split n into (domainNat, valuesNat) via pairing
    //         // 2. Decode domain as Set<bigint> (key indices)
    //         // 3. Decode values as List<bigint> using decodeParts
    //         // 4. Zip keys and values together into Map
    //         let typeArgs = t.GetGenericArguments()
    //         let keyType = typeArgs[0]
    //         let valueType = typeArgs[1]
    //         let keyConstructor = lazy (recMake keyType)
    //         let valueConstructor = lazy (recMake valueType)

    //         fun (n: bigint) ->
    //             // Special case: n=0 maps to empty map
    //             if n = 0I then
    //                 let mapModule =
    //                     typedefof<Map<_, _>>.Assembly.GetType("Microsoft.FSharp.Collections.MapModule")

    //                 let ofListMethod =
    //                     mapModule.GetMethod("OfList").MakeGenericMethod([| keyType; valueType |])

    //                 let emptyList =
    //                     let tupleType = typedefof<_ * _>.MakeGenericType([| keyType; valueType |])
    //                     let listType = typedefof<list<_>>.MakeGenericType([| tupleType |])
    //                     FSharpValue.MakeUnion(FSharpType.GetUnionCases(listType)[0], [||]) // Empty list

    //                 ofListMethod.Invoke(null, [| emptyList |])
    //             else
    //                 // For n > 0: decode (n-1) to ensure domainNat > 0 for non-empty maps
    //                 let domainNat, valuesNat = encodePair (n - 1I)
    //                 let domainSet = Pairing.nat2Set (domainNat + 1I) // Ensure at least one key
    //                 let sortedDomainList = domainSet |> Set.toList |> List.sort
    //                 let size = List.length sortedDomainList
    //                 let valueIndices = decodeParts valuesNat size

    //                 // Create arrays for keys and values
    //                 let keysArray = System.Array.CreateInstance(keyType, size)
    //                 let valuesArray = System.Array.CreateInstance(valueType, size)

    //                 sortedDomainList
    //                 |> List.iteri (fun i keyIdx ->
    //                     let key = keyConstructor.Value keyIdx
    //                     keysArray.SetValue(key, i))

    //                 valueIndices
    //                 |> List.iteri (fun i valueIdx ->
    //                     let value = valueConstructor.Value valueIdx
    //                     valuesArray.SetValue(value, i))

    //                 // Create typed array of tuples
    //                 let tupleType = typedefof<_ * _>.MakeGenericType([| keyType; valueType |])
    //                 let tuplesArray = System.Array.CreateInstance(tupleType, size)

    //                 for i in 0 .. size - 1 do
    //                     let key = keysArray.GetValue(i)
    //                     let value = valuesArray.GetValue(i)
    //                     let tuple = FSharpValue.MakeTuple([| key; value |], tupleType)
    //                     tuplesArray.SetValue(tuple, i)

    //                 // Convert array to list, then to Map
    //                 let listModule =
    //                     typedefof<list<_>>.Assembly.GetType("Microsoft.FSharp.Collections.ListModule")

    //                 let ofArrayMethod =
    //                     listModule.GetMethod("OfArray").MakeGenericMethod([| tupleType |])

    //                 let pairsList = ofArrayMethod.Invoke(null, [| tuplesArray |])

    //                 let mapModule =
    //                     typedefof<Map<_, _>>.Assembly.GetType("Microsoft.FSharp.Collections.MapModule")

    //                 let ofListMethod =
    //                     mapModule.GetMethod("OfList").MakeGenericMethod([| keyType; valueType |])

    //                 ofListMethod.Invoke(null, [| pairsList |])
    //     elif FSharpType.IsUnion(t) then
    //         // For union types, we need to handle nullary cases (0 fields) specially
    //         // to avoid duplicates. We reserve the first K natural numbers for K nullary cases,
    //         // then use the pairing function for non-nullary cases.
    //         let cases = FSharpType.GetUnionCases(t)

    //         // Identify nullary and non-nullary cases
    //         let nullaryIndices =
    //             cases
    //             |> Array.mapi (fun i c -> (i, c.GetFields().Length = 0))
    //             |> Array.filter snd
    //             |> Array.map fst

    //         let nonNullaryIndices =
    //             cases
    //             |> Array.mapi (fun i c -> (i, c.GetFields().Length > 0))
    //             |> Array.filter snd
    //             |> Array.map fst

    //         let caseConstructors =
    //             lazy
    //                 cases
    //                 |> Array.map (fun unionCase ->
    //                     let fields = unionCase.GetFields()
    //                     let fieldConstructors = fields |> Array.map (fun f -> recMake f.PropertyType)

    //                     fun (n: bigint) ->
    //                         if fieldConstructors.Length = 0 then
    //                             // Nullary case: no fields.
    //                             FSharpValue.MakeUnion(unionCase, [||])
    //                         elif fieldConstructors.Length = 1 then
    //                             // Single field: pass the entire number.
    //                             let fieldVal = fieldConstructors[0]n
    //                             FSharpValue.MakeUnion(unionCase, [| fieldVal |])
    //                         else
    //                             // Multiple fields: split n into as many parts as needed.
    //                             let parts = decodeParts n fieldConstructors.Length
    //                             let fieldVals = fieldConstructors |> Array.mapi (fun i cons -> cons parts[i])
    //                             FSharpValue.MakeUnion(unionCase, fieldVals))

    //         fun (n: bigint) ->
    //             let caseConstructors = caseConstructors.Value
    //             let len = Array.length caseConstructors

    //             if len = 1 then
    //                 // Only one case, pass entire number to it
    //                 caseConstructors[0]n
    //             elif nullaryIndices.Length = 0 then
    //                 // No nullary cases, use DivRem (correct enumeration)
    //                 let d, r = bigint.DivRem(n, bigint len)
    //                 caseConstructors[int r]d
    //             elif nonNullaryIndices.Length = 0 then
    //                 // All cases are nullary - this is a FINITE type with exactly len distinct values
    //                 // Only map 0..len-1 to the cases, anything beyond is invalid
    //                 let caseIndex = int n

    //                 if caseIndex < len then
    //                     caseConstructors[caseIndex]0I
    //                 else
    //                     // This is beyond the finite range - treat as out of bounds
    //                     // Return the last case to maintain totality, but this indicates
    //                     // the parent encoding should account for finite field types
    //                     caseConstructors[len - 1]0I
    //             else
    //                 // Mixed nullary and non-nullary cases
    //                 // Reserve first K numbers for K nullary cases
    //                 let nullaryCount = bigint nullaryIndices.Length

    //                 if n < nullaryCount then
    //                     // Use one of the nullary cases
    //                     caseConstructors[nullaryIndices[int n]]0I
    //                 else
    //                     // Use DivRem for non-nullary cases
    //                     let n' = n - nullaryCount
    //                     let nonNullaryCount = bigint nonNullaryIndices.Length
    //                     let d, r = bigint.DivRem(n', nonNullaryCount)
    //                     caseConstructors[nonNullaryIndices[int r]]d
    //     elif FSharpType.IsRecord(t) then
    //         // For records, build a constructor that decodes each field.

    //         let fieldConstructors =
    //             lazy
    //                 let fields = FSharpType.GetRecordFields(t)
    //                 fields |> Array.map (fun f -> recMake f.PropertyType)

    //         fun (n: bigint) ->
    //             let fieldConstructors = fieldConstructors.Value

    //             if fieldConstructors.Length = 0 then
    //                 FSharpValue.MakeRecord(t, [||])
    //             elif fieldConstructors.Length = 1 then
    //                 let fieldVal = fieldConstructors[0]n
    //                 FSharpValue.MakeRecord(t, [| fieldVal |])
    //             else
    //                 let parts = decodeParts n fieldConstructors.Length
    //                 let fieldVals = fieldConstructors |> Array.mapi (fun i cons -> cons parts[i])
    //                 FSharpValue.MakeRecord(t, fieldVals)
    //     else
    //         failwithf $"Type %A{t} is not supported by the Godelian constructor"


    let makeConstructor = memoizeRec createCountable

    /// The public API: automatically create a Gödelian constructor for type 'T.
    let getIndex<'T> () : bigint -> 'T =
        let cons = makeConstructor (typeof<'T>)
        fun n -> cons.Decode n |> unbox<'T>

    let sample<'T> (pageSize: int) (pageIndex: int) : 'T list =
        let cons = makeConstructor (typeof<'T>)
        let startIdx = (bigint pageIndex) * (bigint pageSize)
        let endIdx = startIdx + bigint pageSize - 1I
        [ for i in startIdx .. endIdx -> cons.Decode i |> unbox<'T> ]
        