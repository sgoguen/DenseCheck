namespace DenseCheck

type Countable<'T> =
    abstract member Decode: bigint -> 'T
    // abstract member Encode: 'T -> bigint
    abstract member IsInfinite: bool
    abstract member DomainSize: bigint

module Countable = 
    let infinite<'a> (f: bigint -> 'a) : Countable<'a> =
        { new Countable<'a> with
            member _.Decode n = f n
            member _.IsInfinite = true
            member _.DomainSize = -1I }

    let finiteCountable<'a> (size: bigint) (f: bigint -> 'a) : Countable<'a> =
        { new Countable<'a> with
            member _.Decode n = f n
            // member _.Encode v = failwith "Not implemented"
            member _.IsInfinite = false
            member _.DomainSize = size }
        
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
        