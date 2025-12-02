module DenseCheck.Example2

open DenseCheck.DenseCheck

//  Consider this language
type Term =
    | Value of bigint
    | Add of Term * Term
    | Mul of Term * Term

    override this.ToString() =
        match this with
        | Value b -> $"%A{b}"
        | Add(t1, t2) -> $"({t1} + {t2})"
        | Mul(t1, t2) -> $"({t1} * {t2})"


//  Now, let's see what we can do to implement it for this...
let pick = getIndex<Term> ()

let testIt () =
    for i in 0I .. 100I do
        printfn $"//  %A{i} - %s{(pick i).ToString()}"

//  0 - 0
//  1 - (0 + 0)
//  2 - (0 * 0)
//  3 - 1
//  4 - (0 + (0 + 0))
//  5 - (0 * (0 + 0))
//  6 - 2
//  7 - ((0 + 0) + 0)
//  8 - ((0 + 0) * 0)
//  9 - 3
//  10 - ((0 + 0) + (0 + 0))
//  11 - ((0 + 0) * (0 + 0))
//  12 - 4
//  13 - (0 + (0 * 0))
//  14 - (0 * (0 * 0))
//  15 - 5
//  16 - ((0 + 0) + (0 * 0))
//  17 - ((0 + 0) * (0 * 0))
//  18 - 6
//  19 - ((0 * 0) + 0)
//  20 - ((0 * 0) * 0)
//  21 - 7
//  22 - ((0 * 0) + (0 + 0))
//  23 - ((0 * 0) * (0 + 0))
//  24 - 8
//  25 - ((0 * 0) + (0 * 0))
//  26 - ((0 * 0) * (0 * 0))
//  27 - 9
//  28 - (0 + 1)
