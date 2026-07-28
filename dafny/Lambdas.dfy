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

function natToTerm(n: nat): Term {
  var n2 := n / 3;
  var d := n % 3;
  match d
  case 0 => IDX(n2)
  case 1 => ABS(natToTerm(n2))
  case 2 => var (x, y) := unpair(n2); APP(natToTerm(x), natToTerm(y))
}

function termToNat(term: Term): nat {
  match term
  case IDX(value) => 3 * value
  case ABS(body) => 3 * termToNat(body) + 1
  case APP(func, arg) => 3 * pair(termToNat(func), termToNat(arg)) + 2
}

// ...existing code...

lemma roundTripTermToNat(term: Term)
  ensures natToTerm(termToNat(term)) == term
{
  match term
  case IDX(v) =>
    assert termToNat(term) == 3 * v;
    assert (3 * v) % 3 == 0;
    assert (3 * v) / 3 == v;
  // natToTerm(3*v) takes case 0 => IDX(v)

  case ABS(body) =>
    roundTripTermToNat(body);
    var b := termToNat(body);

    assert termToNat(term) == 3 * b + 1;
    assert (3 * b + 1) % 3 == 1;
    assert (3 * b + 1) / 3 == b;

    assert natToTerm(termToNat(term)) == ABS(natToTerm(b));
    assert natToTerm(b) == body;
    assert natToTerm(termToNat(term)) == term;

  case APP(func, arg) =>
    roundTripTermToNat(func);
    roundTripTermToNat(arg);

    var x := termToNat(func);
    var y := termToNat(arg);

    assert termToNat(term) == 3 * pair(x, y) + 2;
    assert (3 * pair(x, y) + 2) % 3 == 2;
    assert (3 * pair(x, y) + 2) / 3 == pair(x, y);

    // If not already available, prove/use:
    // lemma unpairPair(x: nat, y: nat) ensures unpair(pair(x, y)) == (x, y)
    unpairPair(x, y);
    assert unpair(pair(x, y)) == (x, y);

    assert natToTerm(termToNat(term)) == APP(natToTerm(x), natToTerm(y));
    assert natToTerm(x) == func;
    assert natToTerm(y) == arg;
    assert natToTerm(termToNat(term)) == term;
}

// ...existing code...

lemma roundTripNatToTerm(n: nat)
  ensures termToNat(natToTerm(n)) == n
  decreases n
{
  var d := n % 3;
  var q := n / 3;
  assert n == 3 * q + d;

  match d
  case 0 =>
    calc {
      termToNat(natToTerm(n));
      termToNat(IDX(q));
      3 * q;
      n;
    }

  case 1 =>
    roundTripNatToTerm(q);
    calc {
      termToNat(natToTerm(n));
      termToNat(ABS(natToTerm(q)));
      3 * termToNat(natToTerm(q)) + 1;
      3 * q + 1;
      n;
    }

  case 2 =>
    var (x, y) := unpair(q);
    roundTripNatToTerm(x);
    roundTripNatToTerm(y);
    assert pair(x, y) == q;
    calc {
      termToNat(natToTerm(n));
      termToNat(APP(natToTerm(x), natToTerm(y)));
      3 * pair(termToNat(natToTerm(x)), termToNat(natToTerm(y))) + 2;
      3 * pair(x, y) + 2;
      3 * q + 2;
      n;
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

lemma closedTermFromIntRecRoundTrip(l: int, n: nat)
  requires l >= -1
  ensures closedTermToIntRec(l, closedTermFromIntRec(l, n)) == n
  decreases n - l
{
  if (n <= l) {
    assert closedTermFromIntRec(l, n) == IDX(n);
    calc {
      closedTermToIntRec(l, closedTermFromIntRec(l, n));
      closedTermToIntRec(l, IDX(n));
      n;
    }
  } else {
    var n2 := n - (l + 1);
    var opt := n2 % 2;
    if (opt == 0) {
      closedTermFromIntRecRoundTrip(l + 1, n2 / 2);
      calc {
        closedTermToIntRec(l, closedTermFromIntRec(l, n));
        closedTermToIntRec(l, ABS(closedTermFromIntRec(l + 1, n2 / 2)));
        closedTermToIntRec(l + 1, closedTermFromIntRec(l + 1, n2 / 2)) * 2 + l + 1;
        (n2 / 2) * 2 + l + 1;
        n2 + l + 1;
        n;
      }
    } else {
      var n3 := n2 / 2;
      var (x, y) := unpair(n3);

      assert pair(x, y) == n3;
      assert x <= n3;
      assert y <= n3;

      closedTermFromIntRecRoundTrip(l, x);
      closedTermFromIntRecRoundTrip(l, y);

      calc {
        closedTermToIntRec(l, closedTermFromIntRec(l, n));
        closedTermToIntRec(l, APP(closedTermFromIntRec(l, x), closedTermFromIntRec(l, y)));
        pair(closedTermToIntRec(l, closedTermFromIntRec(l, x)), closedTermToIntRec(l, closedTermFromIntRec(l, y))) * 2 + l + 2;
        pair(x, y) * 2 + l + 2;
        n3 * 2 + l + 2;
        n2 + l + 1;
        n;
      }
    }
  }
}

lemma natToTermRoundTrip(n: nat)
  ensures closedTermToInt(intToClosedTerm(n)) == n
{
  closedTermFromIntRecRoundTrip(-1, n);
  calc {
    closedTermToInt(intToClosedTerm(n));
    closedTermToInt(closedTermFromIntRec(-1, n));
    closedTermToIntRec(-1, closedTermFromIntRec(-1, n));
    n;
  }
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