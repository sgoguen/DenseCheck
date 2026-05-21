namespace DenseCheck

open DenseCheck

module DenseCheck =

    open FSharp.Reflection
    open GodelianTooklit
    open System
    open Functions
    open Nat.Mappings
    open Countable

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

    /// Decode a natural number into a word over a finite alphabet.
    ///
    /// domainSize is the alphabet size. The result is a list of element indices,
    /// each in [0, domainSize). Lists are ordered by length first, then by their
    /// base-domainSize digits, so every finite list has exactly one index and no
    /// index decodes to an out-of-domain element.
    let decodeFiniteListIndices (domainSize: bigint) (n: bigint) : bigint list =
        if domainSize <= 0I then
            invalidArg "domainSize" "Finite list element domain size must be positive"
        elif domainSize = 1I then
            List.replicate (int n) 0I
        else
            let rec findLength length blockSize remaining =
                if remaining < blockSize then
                    length, remaining
                else
                    findLength (length + 1) (blockSize * domainSize) (remaining - blockSize)

            let length, offset = findLength 0 1I n

            let rec decodeDigits remaining count =
                if count = 0 then
                    []
                else
                    let q, r = bigint.DivRem(remaining, domainSize)
                    r :: decodeDigits q (count - 1)

            decodeDigits offset length

    /// Decode one field, respecting a finite field's declared domain.
    ///
    /// Infinite fields consume the whole index. Finite fields reduce the index
    /// modulo DomainSize because callers may be splitting a larger product space.
    let decodeField (constructor: Countable<'obj>) (n: bigint) =
        if constructor.IsInfinite then
            constructor.Decode n
        else
            constructor.Decode(n % constructor.DomainSize)

    /// Decode a product of fields without duplicating finite combinations.
    ///
    /// The finite fields form a mixed-radix product. The infinite fields share
    /// the quotient left after that product is removed. This keeps finite fields
    /// bounded while still giving recursive or otherwise infinite fields an
    /// unbounded index stream.
    let decodeFields (constructors: Countable<'obj> array) (n: bigint) =
        let values = Array.zeroCreate<obj> constructors.Length

        let finiteFields =
            constructors
            |> Array.indexed
            |> Array.filter (fun (_, constructor) -> not constructor.IsInfinite)

        let infiniteFields =
            constructors
            |> Array.indexed
            |> Array.filter (fun (_, constructor) -> constructor.IsInfinite)

        let finiteDomainSize =
            finiteFields
            |> Array.fold (fun product (_, constructor) -> product * constructor.DomainSize) 1I

        let infiniteIndex, finiteOffset =
            if infiniteFields.Length = 0 then
                0I, n
            else
                bigint.DivRem(n, finiteDomainSize)

        let mutable remainingFiniteOffset = finiteOffset

        finiteFields
        |> Array.iter (fun (position, constructor) ->
            let quotient, fieldIndex = bigint.DivRem(remainingFiniteOffset, constructor.DomainSize)
            remainingFiniteOffset <- quotient
            values[position] <- constructor.Decode fieldIndex)

        let infiniteParts = decodeParts infiniteIndex infiniteFields.Length

        infiniteFields
        |> Array.iteri (fun i (position, constructor) -> values[position] <- constructor.Decode infiniteParts[i])

        values

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


    /// Box a typed countable so reflection-built constructors can share one shape.
    let boxCountable (c: Countable<'T>) : Countable<'obj> = Countable.boxCountable c

    let primitiveCountable (t: Type) : Countable<'obj> option =
        if t = typeof<string> then
            // Keep variable-like strings for readability in generated samples.
            Some(infinite <| fun n -> box (toVarName n))
        elif t = typeof<bool> then
            Some(boxCountable Countable.Primitives.forBool)
        elif t = typeof<char> then
            Some(boxCountable Countable.Primitives.forChar)
        elif t = typeof<byte> then
            Some(boxCountable Countable.Primitives.forByte)
        elif t = typeof<sbyte> then
            Some(boxCountable Countable.Primitives.forSByte)
        elif t = typeof<int16> then
            Some(boxCountable Countable.Primitives.forInt16)
        elif t = typeof<uint16> then
            Some(boxCountable Countable.Primitives.forUInt16)
        elif t = typeof<int> then
            Some(boxCountable Countable.Primitives.forInt32)
        elif t = typeof<uint32> then
            Some(boxCountable Countable.Primitives.forUInt32)
        elif t = typeof<int64> then
            Some(boxCountable Countable.Primitives.forInt64)
        elif t = typeof<uint64> then
            Some(boxCountable Countable.Primitives.forUInt64)
        elif t = typeof<nativeint> || t = typeof<IntPtr> then
            Some(boxCountable Countable.Primitives.forNativeInt)
        elif t = typeof<unativeint> || t = typeof<UIntPtr> then
            Some(boxCountable Countable.Primitives.forUNativeInt)
        elif t = typeof<decimal> then
            Some(boxCountable Countable.Primitives.forDecimal)
        elif t = typeof<double> then
            Some(boxCountable Countable.Primitives.forDouble)
        elif t = typeof<single> then
            Some(boxCountable Countable.Primitives.forSingle)
        elif t = typeof<unit> then
            Some(boxCountable Countable.Primitives.forUnit)
        elif t = typeof<bigint> then
            Some(infinite <| fun n -> box n)
        else
            None

    let private makeTypedArray (elementType: Type) (values: obj seq): Array =
        let values = values |> Seq.toArray
        let typedArray = System.Array.CreateInstance(elementType, values.Length)

        values
        |> Array.iteri (fun i value -> typedArray.SetValue(value, i))

        typedArray

    // TODO: Cache module/method reflection lookups for list/set/map builders to reduce per-call overhead.
    let makeFSharpList (elementType: Type) (values: obj seq) =
        let listModule =
            typedefof<list<_>>.Assembly.GetType("Microsoft.FSharp.Collections.ListModule")

        let ofArrayMethod =
            // TODO: Cache module/method reflection lookups for list/set/map builders to reduce per-call overhead???
            listModule.GetMethod("OfArray").MakeGenericMethod([| elementType |])

        ofArrayMethod.Invoke(null, [| makeTypedArray elementType values |])

    let makeFSharpSet (elementType: Type) (values: obj seq) =
        let setModule =
            typedefof<Set<_>>.Assembly.GetType("Microsoft.FSharp.Collections.SetModule")

        let ofArrayMethod =
            setModule.GetMethod("OfArray").MakeGenericMethod([| elementType |])

        ofArrayMethod.Invoke(null, [| makeTypedArray elementType values |])

    let makeFSharpMap (keyType: Type) (valueType: Type) (pairs: (obj * obj) seq) =
        let tupleType = typedefof<_ * _>.MakeGenericType([| keyType; valueType |])

        let pairValues =
            pairs
            |> Seq.map (fun (key, value) -> FSharpValue.MakeTuple([| key; value |], tupleType))

        let pairsList = makeFSharpList tupleType pairValues

        let mapModule =
            typedefof<Map<_, _>>.Assembly.GetType("Microsoft.FSharp.Collections.MapModule")

        let ofListMethod =
            mapModule.GetMethod("OfList").MakeGenericMethod([| keyType; valueType |])

        ofListMethod.Invoke(null, [| pairsList |])

    let createSetCountable (recMake: Type -> Countable<'obj>) (t: Type) : Countable<'obj> =
        let elementType = t.GetGenericArguments()[0]
        let elementConstructor = lazy (recMake elementType)

        infinite <| fun (n: bigint) ->
            Pairing.nat2Set n
            |> Seq.map elementConstructor.Value.Decode
            |> makeFSharpSet elementType

    let createListCountable (recMake: Type -> Countable<'obj>) (t: Type) : Countable<'obj> =
        let elementType = t.GetGenericArguments()[0]
        let elementConstructor = lazy (recMake elementType)

        infinite <| fun (n: bigint) ->
            let elementConstructor = elementConstructor.Value

            let indexList =
                if elementConstructor.IsInfinite then
                    nat2unorderedlist n
                else
                    decodeFiniteListIndices elementConstructor.DomainSize n

            indexList
            |> List.map elementConstructor.Decode
            |> makeFSharpList elementType

    let createMapCountable (recMake: Type -> Countable<'obj>) (t: Type) : Countable<'obj> =
        let typeArgs = t.GetGenericArguments()
        let keyType = typeArgs[0]
        let valueType = typeArgs[1]
        let keyConstructor = lazy (recMake keyType)
        let valueConstructor = lazy (recMake valueType)

        infinite <| fun (n: bigint) ->
            if n = 0I then
                makeFSharpMap keyType valueType []
            else
                // Decode (n - 1) so every non-zero map index has a non-empty key domain.
                let domainNat, valuesNat = encodePair (n - 1I)
                let sortedDomainList = Pairing.nat2Set (domainNat + 1I) |> Set.toList |> List.sort
                let valueIndices = decodeParts valuesNat sortedDomainList.Length

                List.zip sortedDomainList valueIndices
                |> List.map (fun (keyIdx, valueIdx) ->
                    keyConstructor.Value.Decode keyIdx, valueConstructor.Value.Decode valueIdx)
                |> makeFSharpMap keyType valueType

    let private getCaseDomainSize (recMake: Type -> Countable<'obj>) (declaringType: Type) (unionCase: UnionCaseInfo) =
        unionCase.GetFields()
        |> Array.fold
            (fun domain field ->
                match domain with
                | None -> None
                | Some _ when field.PropertyType = declaringType -> None
                | Some product ->
                    let fieldConstructor = recMake field.PropertyType

                    if fieldConstructor.IsInfinite then
                        None
                    else
                        Some(product * fieldConstructor.DomainSize))
            (Some 1I)

    let createUnionCountable (recMake: Type -> Countable<'obj>) (t: Type) : Countable<'obj> =
        let cases = FSharpType.GetUnionCases(t)

        let caseConstructors =
            lazy
                cases
                |> Array.map (fun unionCase ->
                    let fields = unionCase.GetFields()
                    let fieldConstructors = fields |> Array.map (fun f -> recMake f.PropertyType)

                    fun (n: bigint) ->
                        if fieldConstructors.Length = 0 then
                            FSharpValue.MakeUnion(unionCase, [||])
                        else
                            let fieldVals = decodeFields fieldConstructors n
                            FSharpValue.MakeUnion(unionCase, fieldVals))

        let caseDomainSizes =
            cases |> Array.map (getCaseDomainSize recMake t)

        if caseDomainSizes |> Array.forall Option.isSome then
            let finiteCaseSizes = caseDomainSizes |> Array.map Option.get
            let totalSize = finiteCaseSizes |> Array.sum

            finiteCountable totalSize (fun n ->
                let caseConstructors = caseConstructors.Value

                let rec findCase index remaining =
                    if remaining < finiteCaseSizes[index] then
                        index, remaining
                    else
                        findCase (index + 1) (remaining - finiteCaseSizes[index])

                let caseIndex, caseOffset = findCase 0 n
                let f = caseConstructors[caseIndex]
                f caseOffset)
        else
            let finiteCaseIndices =
                caseDomainSizes
                |> Array.indexed
                |> Array.choose (fun (index, domainSize) -> domainSize |> Option.map (fun size -> index, size))

            let infiniteCaseIndices =
                caseDomainSizes
                |> Array.indexed
                |> Array.choose (fun (index, domainSize) -> if domainSize.IsNone then Some index else None)

            let finiteDomainSize = finiteCaseIndices |> Array.sumBy snd

            infinite <| fun (n: bigint) ->
                let caseConstructors = caseConstructors.Value

                if n < finiteDomainSize then
                    let rec findFiniteCase index remaining =
                        let caseIndex, caseSize = finiteCaseIndices[index]

                        if remaining < caseSize then
                            caseIndex, remaining
                        else
                            findFiniteCase (index + 1) (remaining - caseSize)

                    let caseIndex, caseOffset = findFiniteCase 0 n
                    caseConstructors[caseIndex]caseOffset
                else
                    let d, r = bigint.DivRem(n - finiteDomainSize, bigint infiniteCaseIndices.Length)
                    caseConstructors[infiniteCaseIndices[int r]]d

    let createRecordCountable (recMake: Type -> Countable<'obj>) (t: Type) : Countable<'obj> =
        let fieldConstructors =
            lazy
                FSharpType.GetRecordFields(t)
                |> Array.map (fun f -> recMake f.PropertyType)

        infinite <| fun (n: bigint) ->
            let fieldConstructors = fieldConstructors.Value

            if fieldConstructors.Length = 0 then
                FSharpValue.MakeRecord(t, [||])
            elif fieldConstructors.Length = 1 then
                let fieldVal = decodeField fieldConstructors[0] n
                FSharpValue.MakeRecord(t, [| fieldVal |])
            else
                let parts = decodeParts n fieldConstructors.Length
                let fieldVals = fieldConstructors |> Array.mapi (fun i cons -> decodeField cons parts[i])
                FSharpValue.MakeRecord(t, fieldVals)

    let createCountable (recMake: Type -> Countable<'obj>) (t: Type) : Countable<'obj> =
        match primitiveCountable t with
        | Some constructor -> constructor
        | None when t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Set<_>> ->
            createSetCountable recMake t
        | None when t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>> ->
            createListCountable recMake t
        | None when t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Map<_, _>> ->
            createMapCountable recMake t
        | None when FSharpType.IsUnion(t) ->
            createUnionCountable recMake t
        | None when FSharpType.IsRecord(t) ->
            createRecordCountable recMake t
        | None ->
            failwithf $"Type %A{t} is not supported by the Godelian constructor"

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

    let getSetTo<'T when 'T: equality and 'T : comparison> (maxSize : int): 'T list =
        sample<'T> maxSize 0 |> Set.ofList |> Set.toList
