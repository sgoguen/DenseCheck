trait Codec<Rep, Value> {
    function IsValidRep(r: Rep): bool
        reads this
    function IsValidValue(v: Value): bool
        reads this
    function Encode(v: Value): Rep
        requires IsValidValue(v)
        ensures IsValidRep(Encode(v))
        ensures Decode(Encode(v)) == v
        reads this
    function Decode(r: Rep): Value
        requires IsValidRep(r)
        ensures IsValidValue(Decode(r))
        reads this
}

