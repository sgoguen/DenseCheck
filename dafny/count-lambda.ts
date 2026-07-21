
function unpair(z: number): [number, number] {
  const m = Math.floor(Math.sqrt(z))
  const m2 = m * m
  if (z - m2 < m) {
    return [z - m2, m]
  } else {
    return [m, m2 + 2 * m - z]
  }
}

function pair(x: number, y: number): number {
  const m = Math.max(x, y)
  return m * m + m + x - y
}

type Term = 
  | { kind: "IDX"; value: number }
  | { kind: "ABS"; body: Term }
  | { kind: "APP"; func: Term; arg: Term }

function closedTermFromIntRec(l: number, n: number): Term {
    //@ requires l >= -1
    //@ requires n >= 0
    if (n <= l) {
        return { kind: "IDX", value: n }
    } else {
        n = n - (l + 1)
        //@ assert n >= 0
        const opt = n % 2
        if (opt === 0) {
            return { kind: "ABS", body: closedTermFromIntRec(l + 1, Math.floor(n / 2)) }
        } else {
            n = Math.floor(n / 2)
            const [x, y] = unpair(n)
            return { kind: "APP", func: closedTermFromIntRec(l, x), arg: closedTermFromIntRec(l, y) }
        }
    }
}


function intToClosedTerm(n: number): Term {
    //@ requires n >= 0
    return closedTermFromIntRec(-1, n)
}

function closedTermToIntRec(d: number, term: Term): number {
    switch (term.kind) {
        case "IDX":
            if (term.value > d) {
                throw new Error("Invalid term")
            }
            return term.value
        case "ABS":
            return closedTermToIntRec(d + 1, term.body) * 2 + d + 1
        case "APP":
            const x = closedTermToIntRec(d, term.func)
            const y = closedTermToIntRec(d, term.arg)
            const n = pair(x, y)
            return n * 2 + d + 2
    }
}

function closedTermToInt(term: Term): number {
    switch (term.kind) {
        case "IDX":
            throw new Error("Invalid term")
        case "ABS":
            return closedTermToIntRec(0, term.body) * 2
        case "APP":
            const x = closedTermToIntRec(-1, term.func)
            const y = closedTermToIntRec(-1, term.arg)
            const n = pair(x, y)
            return n * 2 + 1
    }
}

function toString(term: Term): string {
    switch (term.kind) {
        case "IDX":
            return term.value.toString()
        case "ABS":
            return `(λ.${toString(term.body)})`
        case "APP":
            return `(${toString(term.func)} ${toString(term.arg)})`
    }
}

function showExamples() {
    const startAt = 0;
    const take = 100;
    for (let i = startAt; i < startAt + take; i++) {
        const term = intToClosedTerm(i)
        console.log(i, toString(term), closedTermToInt(term))
    }
}

showExamples()