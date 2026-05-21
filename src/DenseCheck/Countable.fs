namespace DenseCheck

[<AbstractClass>]
type Countable<'T>() =
    abstract member Decode: bigint -> 'T
    // abstract member Encode: 'T -> bigint
    abstract member IsInfinite: bool
    abstract member DomainSize: bigint

type BoxedCountable<'T>(inner: Countable<'T>) =
    inherit Countable<obj>()
    override _.Decode n = box (inner.Decode n)
    override _.IsInfinite = inner.IsInfinite
    override _.DomainSize = inner.DomainSize


type InfiniteCountable<'T>(decode: bigint -> 'T) =
    inherit Countable<'T>()
    override _.Decode n = decode n
    override _.IsInfinite = true
    override _.DomainSize = -1I

type FiniteCountable<'T>(size: bigint, decode: bigint -> 'T) =
    inherit Countable<'T>()
    override _.Decode n = decode n
    override _.IsInfinite = false
    override _.DomainSize = size


module Countable = 
    let boxCountable<'T> (c: Countable<'T>) : Countable<'obj> = BoxedCountable<'T>(c) :> Countable<obj>
    let infinite<'a> (f: bigint -> 'a) : Countable<'a> = InfiniteCountable(f)

    let finiteCountable<'a> (size: bigint) (f: bigint -> 'a) : Countable<'a> = FiniteCountable(size, f)

    let toList (countable: Countable<'a>) : Countable<'a list> =
        if countable.IsInfinite then
            infinite (fun n ->
                let rec decodeList n =
                    if n = 0I then
                        []
                    else
                        let head = countable.Decode((n - 1I) % countable.DomainSize)
                        let tail = decodeList ((n - 1I) / countable.DomainSize)
                        head :: tail

                decodeList n)
        else
            let size = countable.DomainSize
            let rec decodeList n =
                if n = 0I then
                    []
                else
                    let head = countable.Decode((n - 1I) % size)
                    let tail = decodeList ((n - 1I) / size)
                    head :: tail
            infinite (decodeList)

    // let toSet (countable: Countable<'a>) : Countable<'a list> =
    //     if countable.IsInfinite then
    //         infinite (fun n ->
    //             let rec decodeSet n acc =
    //                 if n = 0I then
    //                     List.rev acc
    //                 else
    //                     let elem = countable.Decode((n - 1I) % countable.DomainSize)
    //                     let newAcc = if List.contains elem acc then acc else elem :: acc
    //                     decodeSet ((n - 1I) / countable.DomainSize) newAcc

    //             decodeSet n [])
    //     else
    //         let size = countable.DomainSize
    //         let newSize = Nat.fact size
    //         finiteCountable newSize (fun n ->
    //             let rec decodeSet n acc =
    //                 if n = 0I then
    //                     List.rev acc
    //                 else
    //                     let elem = countable.Decode((n - 1I) % size)
    //                     let newAcc = if List.contains elem acc then acc else elem :: acc
    //                     decodeSet ((n - 1I) / size) newAcc

    //             decodeSet n [])

    let getPage (countable: Countable<'a>) (pageSize: bigint) (pageIndex: bigint) : 'a list =
        let start = pageIndex * pageSize
        let endExclusive = start + pageSize
        let endExclusive = if countable.IsInfinite then endExclusive else min endExclusive countable.DomainSize
        [ for n in start .. endExclusive - 1I -> countable.Decode n ]
        
    module Primitives =
        let forByte = finiteCountable 256I (byte)
        let forBool = finiteCountable 2I (fun n -> n = 1I)
        let forChar = finiteCountable 65536I (char)
        let forInt32 = finiteCountable 4294967296I (fun n -> int32 (n - 2147483648I))
        let forInt64 = finiteCountable 18446744073709551616I (fun n -> int64 (n - 9223372036854775808I))
        let forSByte = finiteCountable 256I (fun n -> sbyte (n - 128I))
        let forInt16 = finiteCountable 65536I (fun n -> int16 (n - 32768I))
        let forUInt16 = finiteCountable 65536I (fun n -> uint16 n)
        let forUInt32 = finiteCountable 4294967296I (fun n -> uint32 n)
        let forUInt64 = finiteCountable 18446744073709551616I (fun n -> uint64 n)
        let forNativeInt =
            // Platform-sized signed integer using two's complement offset
            let bits = System.IntPtr.Size * 8
            let size = 1I <<< bits
            let offset = 1I <<< (bits - 1)
            finiteCountable size (fun n -> nativeint (int64 (n - offset)))
        let forUNativeInt =
            // Platform-sized unsigned integer
            let bits = System.IntPtr.Size * 8
            let size = 1I <<< bits
            finiteCountable size (fun n -> if bits = 32 then unativeint (uint32 n) else unativeint (uint64 n))
        let forDouble =
            // Map each of the 2^64 bit patterns to a double via bit-cast
            let domain = 18446744073709551616I
            finiteCountable domain (fun n ->
                let bits = uint64 n |> int64
                System.BitConverter.Int64BitsToDouble bits)
        let forSingle =
            // Map each of the 2^32 bit patterns to a single via bit-cast
            let domain = 4294967296I
            finiteCountable domain (fun n ->
                let bits = uint32 n |> int32
                System.BitConverter.Int32BitsToSingle bits)
        let forDecimal =
            // Enumerate decimals by sign, scale (0..28), and 96-bit mantissa modulo 10^28
            infinite (fun n ->
                let sign = if n % 2I = 0I then 1M else -1M
                let n' = n / 2I
                let scale = int (n' % 29I)
                let mantissaIndex = n' / 29I
                // Reduce mantissa to [0, 10^28 - 1]
                let tenPow28 =
                    let rec pow acc k = if k = 0 then acc else pow (acc * 10I) (k - 1)
                    pow 1I 28
                let mantissa = mantissaIndex % tenPow28
                let value = (decimal mantissa)
                let divisor = Microsoft.FSharp.Core.Operators.pown 10M scale
                sign * (value / divisor))
        let forBigInt = infinite (fun n -> n)
        let forString =
            infinite (fun n ->
                let rec decodeChars n =
                    if n = 0I then
                        []
                    else
                        let q, r = bigint.DivRem(n - 1I, 65536I)
                        let c = char (int r)
                        c :: decodeChars q

                let chars = decodeChars n
                System.String(List.toArray (List.rev chars)))
        let forUnit = finiteCountable 1I (fun _ -> ())
        