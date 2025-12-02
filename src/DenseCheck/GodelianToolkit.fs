namespace DenseCheck

module GodelianTooklit =

    open Nat

    // A constructor for an infinite domain
    type InfCtor<'T> = bigint -> 'T
    
    type Countable<'T> =
        abstract member Decode: bigint -> 'T
        // abstract member Encode: 'T -> bigint
        abstract member IsInfinite: bool
        abstract member DomainSize: bigint

    type FinCtor<'T> = FindCtor of size: bigint * ctor: (bigint -> 'T)

    let encodePair (z: bigint) : bigint * bigint = MonoPairing.unpair z

    let (|Pair|) = encodePair

    let decodePair (a: bigint, b: bigint) : bigint = MonoPairing.pair a b


    let combineChoices (functionList: (InfCtor<'a> -> InfCtor<'a>) list) (n: bigint) : 'a =
        let length = bigint (List.length functionList)

        let rec chooseFunction n =
            let d, r = bigint.DivRem(n, length)
            let f = functionList[int r]
            f chooseFunction d

        chooseFunction n


    let combineChoicesWithContext
        (getOptions: 'a -> (('a -> InfCtor<'b>) -> InfCtor<'b>) list)
        (initialContext: 'a)
        : InfCtor<'b> =

        //  We add a parameter that now includes context
        let rec chooseFunction context n =
            let functionList = getOptions context
            let length = bigint (List.length functionList)
            let d, r = bigint.DivRem(n, length)
            let f = functionList[int r]
            f chooseFunction d

        chooseFunction initialContext


    let tryFiniteFirst
        (numberOfFiniteOptions: int)
        (finiteConstuctor: int -> 'b)
        (infiniteConstructors: ('a -> InfCtor<'b>) list)
        =
        let numberOfFiniteOptions = numberOfFiniteOptions - 1
        let length = bigint (List.length infiniteConstructors)

        if numberOfFiniteOptions >= 0 then
            [ (fun enc n ->
                  if n <= (bigint numberOfFiniteOptions) then
                      finiteConstuctor (int n)
                  else
                      let n = n - (bigint (numberOfFiniteOptions + 1))
                      let d, r = bigint.DivRem(n, length)
                      let f = infiniteConstructors[int r]
                      f enc d) ]
        else
            infiniteConstructors
