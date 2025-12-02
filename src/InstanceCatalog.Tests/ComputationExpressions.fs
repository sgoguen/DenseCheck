module EverythingIsFS.ComputationExpressions

(*
This module explores how we can construct a computation expression

Methods

Bind - M<'T> * ('T -> M<'U>) -> M<'U>
    * Called for let! and do! in computation expressions.

BindN - (M<'T1> * M<'T2> * ... * M<'TN> * ('T1 * 'T2 ... * 'TN -> M<'U>)) -> M<'U>
    * Called for efficient let! and and! in computation expressions without merging inputs.
    * for example, Bind3, Bind4.

Delay - (unit -> M<'T>) -> Delayed<'T>
    * Wraps a computation expression as a function. Delayed<'T> can be any type, commonly M<'T> or unit -> M<'T> are used. The default implementation returns a M<'T>.

Return - 'T -> M<'T>
    * Called for return in computation expressions.

ReturnFrom - 	M<'T> -> M<'T>
    * Called for return! in computation expressions.

ReturnFromFinal - M<'T> -> M<'T>
    * If present, called for return! and do! when in tail-call position.

BindReturn - (M<'T1> * ('T1 -> 'T2)) -> M<'T2>
    * Called for an efficient let! ... return in computation expressions.

BindNReturn - (M<'T1> * M<'T2> * ... * M<'TN> * ('T1 * 'T2 ... * 'TN -> M<'U>)) -> M<'U>
    * Called for efficient let! ... and! ... return in computation expressions without merging inputs.
    * for example, Bind3Return, Bind4Return.

MergeSources - (M<'T1> * M<'T2>) -> M<'T1 * 'T2>
    * Called for and! in computation expressions.

MergeSourcesN - (M<'T1> * M<'T2> * ... * M<'TN>) -> M<'T1 * 'T2 * ... * 'TN>
    * Called for and! in computation expressions, but improves efficiency by reducing the number of tupling nodes.
    * for example, MergeSources3, MergeSources4.

Run - Delayed<'T> -> M<'T> or M<'T> -> 'T
    * Executes a computation expression.

Combine - M<'T> * Delayed<'T> -> M<'T> or M<unit> * M<'T> -> M<'T>
    * Called for sequencing in computation expressions.

For - seq<'T> * ('T -> M<'U>) -> M<'U> or seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    * Called for for...do expressions in computation expressions.

TryFinally - Delayed<'T> * (unit -> unit) -> M<'T>
    * Called for try...finally expressions in computation expressions.

TryWith - Delayed<'T> * (exn -> M<'T>) -> M<'T>
    * Called for try...with expressions in computation expressions.

Using - 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    * Called for use bindings in computation expressions.

While - (unit -> bool) * Delayed<'T> -> M<'T> or (unit -> bool) * Delayed<unit> -> M<unit>
    * Called for while...do expressions in computation expressions.

Yield - 'T -> M<'T>
    * Called for yield in computation expressions.

YieldFrom - M<'T> -> M<'T>
    * Called for yield! in computation expressions.

YieldFromFinal - M<'T> -> M<'T>
    * If present, called for yield! when in tail-call position.

Zero - unit -> M<unit>
    * Called for empty else branches of if...then expressions in computation expressions.

Quote - Quotations.Expr<'T> -> Quotations.Expr<'T>
    * Indicates that the computation expression is passed to the Run member as a quotation. 
    * It translates all instances of a computation into a quotation.

*)
