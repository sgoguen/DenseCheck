namespace DenseCheck

open System.Collections.Generic
open System.Text
open DenseCheck.DenseCheck
open Xunit

module Example3 =
    open System

    type expression =
        | Variable of int (* a variable *)
        | Numeral of int (* integer constant *)
        | Plus of expression * expression (* addition [e1 + e2] *)
    //| Minus of expression * expression (* difference [e1 - e2] *)
    //| Times of expression * expression (* product [e1 * e2] *)
    //| Divide of expression * expression (* quotient [e1 / e2] *)
    //| Remainder of expression * expression (* remainder [e1 % e2] *)

    type boolean =
        | True (* constant [true] *)
        | False (* constant [false] *)
        | Equal of expression * expression (* equal [e1 = e2] *)
        | Less of expression * expression (* less than [e1 < e2] *)
        | And of boolean Set (* conjunction [b1 and b2] *)
        | Or of boolean Set (* disjunction [b1 or b2] *)
        | Not of boolean (* negation [not b] *)

    type command =
        //| Skip (* no operation [skip] *)
        //| New of string * expression * command (* variable declaration [new x := e in c] *)
        //| Print of expression (* print expression [print e] *)
        | Assign of string * expression (* assign a variable [x := e] *)
        | Sequence of command * command (* sequence commands [c1 ; c2] *)
        | While of boolean * command (* loop [while b do c done] *)
        | Conditional of boolean * command * command (* conditional [if b then c1 else c2 end] *)


    let rec printCommand (p: Printer.Printer) (c: command) : unit =
        //  Let's print the above commands to resemble that of a language that
        //  resembles JavaScript where we don't have to declare variables before we use them
        match c with
        | Assign(var, expr) ->
            p.Print(var)
            p.Print(" = ")
            printExpression expr p
            p.Print(";")
        | Sequence(c1, c2) ->
            printCommand p c1
            p.PrintNewLine()
            printCommand p c2
        | While(cond, body) ->
            p.Print("while (")
            printBoolean cond p
            p.Print(") {")
            p.PushIndentation()
            p.PrintNewLine()
            printCommand p body
            p.PopIndentation()
            p.PrintNewLine()
            p.Print("}")
        | Conditional(cond, thenCmd, elseCmd) ->
            p.Print("if (")
            printBoolean cond p
            p.Print(") {")
            p.PushIndentation()
            p.PrintNewLine()
            printCommand p thenCmd
            p.PopIndentation()
            p.PrintNewLine()
            p.Print("} else {")
            p.PushIndentation()
            p.PrintNewLine()
            printCommand p elseCmd
            p.PopIndentation()
            p.PrintNewLine()
            p.Print("}")

    and printExpression (e: expression) (p: Printer.Printer) : unit =
        match e with
        | Variable v -> p.Print(sprintf "x%d" v)
        | Numeral n -> p.Print(sprintf "%d" n)
        | Plus(e1, e2) ->
            printExpression e1 p
            p.Print(" + ")
            printExpression e2 p

    and printBoolean (b: boolean) (p: Printer.Printer) : unit =
        match b with
        | True -> p.Print("true")
        | False -> p.Print("false")
        | Equal(e1, e2) ->
            printExpression e1 p
            p.Print(" == ")
            printExpression e2 p
        | Less(e1, e2) ->
            printExpression e1 p
            p.Print(" < ")
            printExpression e2 p
        | And(bs) ->
            let first = ref true
            p.Print("(")
            for b1 in bs do
                if !first then
                    first := false
                else
                    p.Print(" && ")
                printBoolean b1 p
            p.Print(")")
        | Or(bs) ->
            let first = ref true
            p.Print("(")
            for b1 in bs do
                if !first then
                    first := false
                else
                    p.Print(" || ")
                printBoolean b1 p
            p.Print(")")
        | Not(b) ->
            p.Print("!")
            printBoolean b p




    //  Now, let's see what we can do to implement it for this...
    let pick = getIndex<command> ()

    type StringWriter() =
        let sb = new StringBuilder()
        member this.GetCode() = sb.ToString()
        interface Printer.Writer with
            member _.Write(s: string) = sb.Append(s) |> ignore
        interface IDisposable with
            member _.Dispose() = ()
            
    let toCode (c: command) =
        use writer = new StringWriter()
        use printer = new Printer.PrinterImpl(writer)
        
        c |> printCommand printer
        printer.Flush()
        writer.GetCode()
        
    
    type ConsoleWriter() =
        interface Printer.Writer with
            member _.Write(s: string) = Console.Write(s)

        interface IDisposable with
            member _.Dispose() = ()

    let testFSharp (n: bigint) =
        use writer = new ConsoleWriter()
        use printer = new Printer.PrinterImpl(writer) // :> Printer.Printer

        pick n |> printCommand printer

        printer.Flush()


    [<Fact>]
    let ``First 1000 should be distinct`` () =
        let alreadyCreated = new Dictionary<command, bigint>()
        for i in 0I..1000I do
            let created = pick i
            let added = alreadyCreated.TryAdd(created, i)
            if added = false then
                let foundAt = alreadyCreated[created]
                Assert.Fail($"Duplicate {i} found at {foundAt} for object {created}")

    [<Fact>]
    let ``Print one big one`` () =
        let bigIndex = 1234567890304982340732492340982349872234047I
        let created = pick bigIndex
        let code = toCode created
        // Print to console for visual inspection
        printfn "%s" code
        Assert.True(code.Length > 0)