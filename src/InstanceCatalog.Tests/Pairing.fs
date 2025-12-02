module EverythingIsFS.Pairing

open System.Numerics

/// Nat2Set conversion: converts a natural number to the set of exponents for its 1-bits.
/// For example, 42 (binary 101010) becomes {1, 3, 5}.
let nat2Set (n: BigInteger) : Set<BigInteger> =
    if n < BigInteger.Zero then
        invalidArg "n" "n must be nonnegative"

    let rec loop acc exponent num =
        if num = BigInteger.Zero then
            acc
        else
            let newAcc =
                if (num &&& BigInteger.One) = BigInteger.One then
                    Set.add exponent acc
                else
                    acc

            loop newAcc (exponent + BigInteger.One) (num >>> 1)

    loop Set.empty BigInteger.Zero n

/// Set2Nat conversion: converts a set of exponents back to a natural number.
/// For example, {1, 3, 5} becomes 2^1 + 2^3 + 2^5 = 42.
let set2Nat (s: Set<BigInteger>) : BigInteger =
    s
    |> Set.fold (fun acc exp -> acc + (BigInteger.One <<< int exp)) BigInteger.Zero

/// BitPair: encodes a pair (i, j) of natural numbers as a single natural number.
/// Uses bit-interleaving: evens(i) ++ odds(j)
/// where evens(i) = map (2*) (nat2set i) and odds(j) = map (2*x + 1) (nat2set j)
let bitPair (i: BigInteger, j: BigInteger) : BigInteger =
    let evens = nat2Set i |> Set.map (fun x -> 2I * x)
    let odds = nat2Set j |> Set.map (fun x -> 2I * x + 1I)
    let combined = Set.union evens odds
    set2Nat combined

/// BitUnpair: decodes a natural number back to a pair (i, j).
/// Partitions the set into even and odd parts, then maps them back.
let bitUnpair (n: BigInteger) : BigInteger * BigInteger =
    let xs = nat2Set n
    let xsEven = xs |> Set.filter (fun x -> x % 2I = 0I)
    let xsOdd = xs |> Set.filter (fun x -> x % 2I <> 0I)

    let decode s =
        s |> Set.map (fun x -> x / 2I) |> set2Nat

    (decode xsEven, decode xsOdd)
