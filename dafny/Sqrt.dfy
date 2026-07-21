module Nat {

  function sqrt(z: nat): (nat, nat)
    requires z >= 0
    ensures var (s, r) := sqrt(z); s * s + r == z && 0 <= r < 2 * s + 1
  {
    if z == 0 then (0, 0)
    else
      var (s, r) := sqrt(z - 1);
      if r + 1 < 2 * s + 1 then (s, r + 1)
      else (s + 1, 0)
  }

    // Helper: sqrt is uniquely determined by its postcondition.
  // If s*s + r == n and 0 <= r < 2*s+1, then sqrt(n) == (s, r).
  lemma sqrtUnique(n: nat, s: nat, r: nat)
    requires s * s + r == n
    requires 0 <= r < 2 * s + 1
    ensures sqrt(n) == (s, r)
  {
    if n == 0 {
      // s == 0, r == 0
    } else if r > 0 {
      sqrtUnique(n - 1, s, r - 1);
    } else {
      // r == 0, so n = s*s and s >= 1
      assert s >= 1;
      var s' := s - 1;
      var r' := 2 * s - 2;
      assert s' * s' + r' == n - 1;
      assert 0 <= r' < 2 * s' + 1;
      sqrtUnique(n - 1, s', r');
    }
  }

}