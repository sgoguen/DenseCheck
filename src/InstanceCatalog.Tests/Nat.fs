module EverythingIsFS.Nat

type Nat = bigint

let sqrt (z: bigint) : bigint =
    if z < 0I then
        invalidArg "z" "Cannot compute the square root of a negative number"
    elif z = 0I then
        0I
    else
        let rec newtonRaphson (x: bigint) : bigint =
            let nextX = (x + z / x) / 2I
            if nextX >= x then x else newtonRaphson nextX

        newtonRaphson z

module MonoPairing =

    let pair (a: Nat) (b: Nat) =
        if a < b then b * b + a else a * a + a + b

    let unpair (n: Nat) =
        let s = sqrt n
        if n - s * s < s then (n - s * s, s) else (s, n - s * s - s)

module Mappings =

    let nat2pair (z: bigint) : bigint * bigint = MonoPairing.unpair z

    let nat2orderedpair (z: bigint) : bigint * bigint =
        let (a, b) = nat2pair z
        (a, a + b)

    let pair2ordered (a: bigint, b: bigint) = (a, a + b + 1I)

    let nat2pairWithId (skip: bigint) (z: bigint) : bigint * bigint =
        if z < skip then
            (z, z)
        else
            let (a, b) = nat2pair (z - skip)
            (a + skip, b + skip)

    let skip (i: bigint) (n: bigint) : bigint = n + i

    let nat2orderedPairWithId (z: bigint) : bigint * bigint =
        let (a, b) = nat2pair z |> pair2ordered
        (a + 1I, b + 1I)


    let nat2set (n: Nat) : Set<Nat> =
        let rec nat2exps n x =
            if n = 0I then
                []
            else
                let xs = nat2exps (n / 2I) (x + 1I)
                if n % 2I = 0I then xs else x :: xs

        (if n >= 0I then nat2exps n 0I else []) |> Set.ofList

    let set2nat (s: Set<Nat>) : Nat =
        s |> Set.fold (fun acc exp -> acc + (1I <<< int exp)) 0I

    let nat2orderedlist (n: Nat) =
        let s = nat2set n |> Set.toSeq
        [ for (i, a) in Seq.indexed s -> a - (bigint i) ]

    let rec nat2unorderedlist (n: Nat) =
        if n = 0I then
            []
        else
            let (b, a) = nat2pair n
            a :: nat2unorderedlist b

    let rec unorderedlist2nat (lst: Nat list) : Nat =
        match lst with
        | [] -> 0I
        | head :: rest ->
            let restEncoded = unorderedlist2nat rest
            MonoPairing.pair restEncoded head
