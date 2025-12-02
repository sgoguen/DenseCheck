module InstanceCatalog.AutoConstructor

open GodelianTooklit

    
open InstanceCatalog
        
    //  What we want to do is create an algebraic specification for this language that tightens the
    //  space so we're not creating these duplicate and redundant terms in the first place using 
    //  algebraic rules like so:
    
    //  0 + a = a                  //  0 is the identity
    //  a + b = b + a              //  Addition is commutative
    //  (a + b) + c = a + (b + c)  //  Addition is associative
    //  
    //  0 * a = 0                  //  0 is ????  for multiplication
    //  1 * a = a                  //  1 is the identity for multiplication
    //  a * b = b + a              //  Multiplication is commutative
    //  (a * b) * c = a * (b * c)  //  Multiplication is associative
    //  
    //  The OBJ and Maude programming languages have built in features that can easily apply these
    //  rules to constructors.  When terms are constructed, they are automatically reduced to their 
    //  normal form.
    //
    //  For example, commutivity implies the thing reduce to a form where the left component is always
    //  less than the right component.
    //
    //  A non-associative, non-commutative binary operation can be used to construct a binary tree 
    //  whereas an associative, non-commutative binary operation can be used to construct a list.
    //  Adding commutivity to the list turns the list into an ordered list, resembling a multiset.
    //  If one defines idempotence as (a * a = a), that multiset turns into an ordered list with 
    //  distinct elements which reflects a set.
    //
    //  I want you to consider how the above functions in the Mappings module, like nat2set and pair2ordered,
    //  play a role in creating a tighter bijection.  
    //
    //  Let's consider a number of different ways to approach this:
    //
    //  1. We could annotate types with attributes.
    //  2. Rather than createGodelianConstructorFor returning a (bigint -> 'T) function, we could
    //     return an object that would allow us to further refine construction:
    //
    //     For example:
    //
    //     let ctor = createGodelianConstructorFor<Term>()
    //                  .MarkCommutative(Add)
    //                  .HasIdentity(Add, 0)
    //  3. Ideally, it would be cool to leverage expressions to define these rules, for example:
    //
    //     let ctor = createGodelianConstructorFor<Term>()
    //                  .Refine(fun (a) -> Add(a, a) = a)
    //                  .Refine(fun (a, b) -> Add(a, b) = Add(b, a))
    //
    //     But I don't know how to go about beginning to implement that.  I feel like need to deeply understand
    //     OBJ / Maude and Instutions to begin to approach it.
    //  
    //  I'm not thrilled about the above suggestions and I'm open to other ideas, but I'd like to explore
    //  how to approach this problem.
    
    
    



// module Example3 = 
//     open System
//
//     type expression =
//       | Variable of int (* a variable *)
//       | Numeral of int (* integer constant *)
//       | Plus of expression * expression  (* addition [e1 + e2] *)
//       //| Minus of expression * expression (* difference [e1 - e2] *)
//       //| Times of expression * expression (* product [e1 * e2] *)
//       //| Divide of expression * expression (* quotient [e1 / e2] *)
//       //| Remainder of expression * expression (* remainder [e1 % e2] *)
//
//     type boolean =
//       | True (* constant [true] *)
//       | False (* constant [false] *)
//       | Equal of expression * expression (* equal [e1 = e2] *)
//       | Less of expression * expression (* less than [e1 < e2] *)
//       | And of boolean * boolean (* conjunction [b1 and b2] *)
//       | Or of boolean * boolean (* disjunction [b1 or b2] *)
//       | Not of boolean (* negation [not b] *)
//
//     type command =
//       //| Skip (* no operation [skip] *)
//       //| New of string * expression * command (* variable declaration [new x := e in c] *)
//       //| Print of expression (* print expression [print e] *)
//       | Assign of string * expression (* assign a variable [x := e] *)
//       | Sequence of command * command (* sequence commands [c1 ; c2] *)
//       | While of boolean * command (* loop [while b do c done] *)
//       | Conditional of boolean * command * command (* conditional [if b then c1 else c2 end] *)
//
//
//     let rec printCommand (p: Printer.Printer) (c:command) : unit = 
//         //  Let's print the above commands to resemble that of a language that
//         //  resembles JavaScript where we don't have to declare variables before we use them
//                 match c with
//                 | Assign(var, expr) ->
//                     p.Print(var)
//                     p.Print(" = ")
//                     printExpression expr p
//                     p.Print(";")
//                 | Sequence(c1, c2) ->
//                     printCommand p c1
//                     p.PrintNewLine()
//                     printCommand p c2
//                 | While(cond, body) ->
//                     p.Print("while (")
//                     printBoolean cond p
//                     p.Print(") {")
//                     p.PushIndentation()
//                     p.PrintNewLine()
//                     printCommand p body
//                     p.PopIndentation()
//                     p.PrintNewLine()
//                     p.Print("}")
//                 | Conditional(cond, thenCmd, elseCmd) ->
//                     p.Print("if (")
//                     printBoolean cond p
//                     p.Print(") {")
//                     p.PushIndentation()
//                     p.PrintNewLine()
//                     printCommand p thenCmd
//                     p.PopIndentation()
//                     p.PrintNewLine()
//                     p.Print("} else {")
//                     p.PushIndentation()
//                     p.PrintNewLine()
//                     printCommand p elseCmd
//                     p.PopIndentation()
//                     p.PrintNewLine()
//                     p.Print("}")
//         
//     and printExpression (e: expression) (p: Printer.Printer): unit =
//         match e with
//         | Variable v -> p.Print(sprintf "x%d" v)
//         | Numeral n -> p.Print(sprintf "%d" n)
//         | Plus(e1, e2) ->
//             printExpression e1 p
//             p.Print(" + ")
//             printExpression e2 p
//
//     and printBoolean (b: boolean) (p: Printer.Printer): unit =
//         match b with
//         | True -> p.Print("true")
//         | False -> p.Print("false")
//         | Equal(e1, e2) ->
//             printExpression e1 p
//             p.Print(" == ")
//             printExpression e2 p
//         | Less(e1, e2) ->
//             printExpression e1 p
//             p.Print(" < ")
//             printExpression e2 p
//         | And(b1, b2) ->
//             printBoolean b1 p
//             p.Print(" && ")
//             printBoolean b2 p
//         | Or(b1, b2) ->
//             printBoolean b1 p
//             p.Print(" || ")
//             printBoolean b2 p
//         | Not(b) ->
//             p.Print("!")
//             printBoolean b p
//         
//         
//     
//
//     //  Now, let's see what we can do to implement it for this...
//     let pick = createGodelianConstructorFor<command>()
//
//     type ConsoleWriter() = 
//         interface Printer.Writer with
//             member _.Write(s: string) = Console.Write(s)
//         interface IDisposable with
//             member _.Dispose() = ()
//     
//     let testFSharp(n: bigint) = 
//         use writer = new ConsoleWriter()
//         use printer = new Printer.PrinterImpl(writer)  // :> Printer.Printer
//
//         pick n |> printCommand printer
//         
//         printer.Flush()

//  Prints the following
//
//  while (x10 == x0 + x0 + x0 + x0 || false || true && false || true && !false) {
//    if (478 == 18 + x0) {
//        if (true) {
//            a = x0;
//        } else {
//            a = x0;
//        }
//        a = x0;
//        a = x0;
//        if (false) {
//            a = x0;
//            a = x0;
//        } else {
//            a = x0;
//        }
//    } else {
//        if (true) {
//            a = x0;
//            a = x0;
//        } else {
//            a = x0;
//        }
//        e = 1;
//    }
//  }


//  