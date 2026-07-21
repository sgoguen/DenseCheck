include "Sqrt.dfy"
include "Nat.dfy"
include "Pairing.dfy"

import opened Pairing
import opened NatString

type InfCtor<T> = (nat -> T)
type RecCtor<!T> = (InfCtor<T> -> InfCtor<T>)

function combineChoices<T>(baseCase: InfCtor<T>, functionList: seq<RecCtor<T>>, n: nat): T
  decreases n
{
  var length := |functionList| + 1;
  var r := n % length;
  var d := n / length;
  if r == 0 then
    baseCase(n)
  else
    var f := functionList[r - 1];
    var g := (n2: nat) => if n2 < n then combineChoices(baseCase, functionList, n2) else baseCase(n2);
    f(g)(d)
}

datatype CombinedChoiced<!T> = 
  | CombinedChoice(baseCase: InfCtor<T>, functionList: seq<RecCtor<T>>) {

    function Lookup(n: nat): T
        decreases n
    {
        var length := |this.functionList| + 1;

        var r := n % length;
        var d := n / length;
        if r == 0 then
            this.baseCase(d)
        else
            var f := this.functionList[r - 1];
            var g := f((n2: nat) => if n2 < n then this.Lookup(n2) else this.baseCase(n2));
            g(d)
    }
  }


datatype Expr =
  | Const(value: nat)
  | Add(left: Expr, right: Expr)
  | Mul(left: Expr, right: Expr) {

    function toString(): string
    {
        match this
        case Const(value) => natToString(value)
        case Add(left, right) => "(" + left.toString() + " + " + right.toString() + ")"
        case Mul(left, right) => "(" + left.toString() + " * " + right.toString() + ")"
    }

  }


method showExamples()
{
  var getConst : InfCtor<Expr> := (n: nat) => Const(n);
  var getAdd : RecCtor<Expr> := (r: InfCtor<Expr>) => (n: nat) => var (x, y) := unpair(n); Add(r(x), r(y));
  var getMul : RecCtor<Expr> := (r: InfCtor<Expr>) => (n: nat) => var (x, y) := unpair(n); Mul(r(x), r(y));
  var combined := CombinedChoice(getConst, [getAdd, getMul]);
  var makeExpr := (n:nat) => combined.Lookup(n);

  var startAt := 1000;
  var take := 100;
  var i := startAt;
  while i < startAt + take
  {
    var term := makeExpr(i);
    print i, " ", term.toString(), "\n";
    i := i + 1;
  }
}

method Main() {
  showExamples();
}


