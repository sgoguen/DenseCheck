include "Pairing.dfy"
include "Nat.dfy"

import opened Pairing
import opened NatString

datatype Term =
  | IDX(value: nat)
  | ABS(body: Term)
  | APP(func: Term, arg: Term) {

  predicate isClosedRec(l: nat) {
    match this
    case IDX(value) => value < l
    case ABS(body) => body.isClosedRec(l + 1)
    case APP(func, arg) => func.isClosedRec(l) && arg.isClosedRec(l)
  }

  predicate isClosed() {
    this.isClosedRec(0)
  }
}

function closedTermFromIntRec(l: int, n: nat): Term
  requires l >= -1
  decreases n - l
  ensures closedTermFromIntRec(l, n).isClosedRec(l + 1)
{
  if (n <= l) then
    IDX(n)
  else
    var n2 := n - (l + 1);
    var opt := n2 % 2;
    if (opt == 0) then
      ABS(closedTermFromIntRec(l + 1, n2 / 2))
    else
      var n3 := n2 / 2;
      var (x, y) := unpair(n3);
      APP(closedTermFromIntRec(l, x), closedTermFromIntRec(l, y))
}

function intToClosedTerm(n: nat): Term
  ensures intToClosedTerm(n).isClosed()
{
  closedTermFromIntRec(-1, n)
}

function closedTermToIntRec(d: int, term: Term): nat
    requires d >= -1
    decreases term
{
  match term
  case IDX(value) => if value > d then 0 else value
  case ABS(body) => closedTermToIntRec(d + 1, body) * 2 + d + 1
  case APP(func, arg) => 
    var x := closedTermToIntRec(d, func);
    var y := closedTermToIntRec(d, arg);
    var n := pair(x, y);
    n * 2 + d + 2
}

function closedTermToInt(term: Term): nat
    requires term.isClosed()
{
  match term
  case IDX(value) => 0
  case ABS(body) => closedTermToIntRec(0, body) * 2
  case APP(func, arg) => 
    var x := closedTermToIntRec(-1, func);
    var y := closedTermToIntRec(-1, arg);
    var n := pair(x, y);
    n * 2 + 1
}


lemma {:vcs_split_on_every_assert} roundTripRec(d: int, term: Term)
  requires d >= -1
  requires term.isClosedRec(d + 1)
  ensures closedTermFromIntRec(d, closedTermToIntRec(d, term)) == term
  decreases term
{
  match term
  case IDX(v) =>
    assert v <= d;
    calc {
      closedTermFromIntRec(d, closedTermToIntRec(d, term));
      closedTermFromIntRec(d, v);
      IDX(v);
      term;
    }

  case ABS(body) =>
    roundTripRec(d + 1, body);
    var k := closedTermToIntRec(d + 1, body);
    assert closedTermToIntRec(d, term) == (d + 1) + 2 * k;

    calc {
      closedTermFromIntRec(d, closedTermToIntRec(d, term));
      closedTermFromIntRec(d, (d + 1) + 2 * k);
      ABS(closedTermFromIntRec(d + 1, 2 * k / 2));
      ABS(closedTermFromIntRec(d + 1, k));
      ABS(body);
      term;
    }

case APP(func, arg) =>
  roundTripRec(d, func);
  roundTripRec(d, arg);

  var x := closedTermToIntRec(d, func);
  var y := closedTermToIntRec(d, arg);
  var p := pair(x, y);

  Pairing.unpairPair(x, y);
  assert closedTermToIntRec(d, term) == (d + 1) + (2 * p + 1);

  calc {
    closedTermFromIntRec(d, closedTermToIntRec(d, term));
    closedTermFromIntRec(d, (d + 1) + (2 * p + 1));
    APP(
      closedTermFromIntRec(d, (unpair((2 * p + 1) / 2)).0),
      closedTermFromIntRec(d, (unpair((2 * p + 1) / 2)).1)
    );
    APP(
      closedTermFromIntRec(d, (unpair(p)).0),
      closedTermFromIntRec(d, (unpair(p)).1)
    );
    APP(closedTermFromIntRec(d, x), closedTermFromIntRec(d, y));
    APP(func, arg);
    term;
  }
}


lemma anyClosedTermRoundTrips(term: Term)
  requires term.isClosed()
  ensures intToClosedTerm(closedTermToInt(term)) == term
{
  roundTripRec(-1, term);
}

function toString(term: Term): string
{
  match term
  case IDX(value) => natToString(value)
  case ABS(body) => "(\\." + toString(body) + ")"
  case APP(func, arg) => "(" + toString(func) + " " + toString(arg) + ")"
}

method showExamples()
{
  var startAt := 0;
  var take := 100;
  var i := startAt;
  while i < startAt + take
  {
    var term := intToClosedTerm(i);
    print i, " ", toString(term), "\n";
    i := i + 1;
  }
}

method Main() {
  showExamples();
}