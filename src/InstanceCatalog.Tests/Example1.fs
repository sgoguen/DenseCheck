module EverythingIsFS.Example1

open EverythingIsFS.GodelianTooklit

type Term =
    | Var of int
    | Lambda of Term
    | App of Term * Term


let naiveConstructor: bigint -> Term =
    combineChoices
        [ fun _ n -> Var(int (n))
          fun enc (Pair(_, body)) -> Lambda(enc body)
          fun enc (Pair(l, r)) -> App(enc l, enc r) ]
