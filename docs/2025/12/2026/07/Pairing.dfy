include "Sqrt.dfy"

module Pairing {

  import opened Nat

  function pair(a: nat, b: nat): nat
    ensures pair(a, b) >= a && pair(a, b) >= b
    ensures a < b ==> pair(a, b) == b * b + a
    ensures a >= b ==> pair(a, b) == a * a + a + b
  {
    if a < b then b * b + a else a * a + a + b
  }

  function unpair(n: nat): (nat, nat)
    ensures var (a, b) := unpair(n); pair(a, b) == n
  {
    var (s, r) := sqrt(n);
    if r < s then (r, s) else (s, r - s)
  }

  // Direction 1: unpair(pair(a, b)) == (a, b)
  lemma unpairPair(a: nat, b: nat)
    ensures unpair(pair(a, b)) == (a, b)
  {
    var n := pair(a, b);
    if a < b {
      // n = b*b + a, and 0 <= a < b < 2*b+1
      sqrtUnique(n, b, a);
      // sqrt(n) == (b, a), and a < b, so unpair yields (a, b)
    } else {
      // n = a*a + a + b, and 0 <= a+b <= 2*a < 2*a+1
      sqrtUnique(n, a, a + b);
      // sqrt(n) == (a, a+b), and a+b >= a, so unpair yields (a, (a+b)-a) == (a, b)
    }
  }

  // Direction 2: pair(unpair(n)) == n  (follows directly from unpair's postcondition)
  lemma pairUnpair(n: nat)
    ensures var (a, b) := unpair(n); pair(a, b) == n
  {}
}