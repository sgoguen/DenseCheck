namespace DenseCheck

[<AbstractClass>]
type Countable<'T> =
    abstract member Decode: bigint -> 'T
    // abstract member Encode: 'T -> bigint
    abstract member IsInfinite: bool
    abstract member DomainSize: bigint


module Countable =
    val boxCountable: Countable<'T> -> Countable<obj>
    val infinite: (bigint -> 'a) -> Countable<'a>
    val finiteCountable: bigint -> (bigint -> 'a) -> Countable<'a>

    val toList: Countable<'a> -> Countable<'a list>
    val toSet: Countable<'a> -> Countable<'a Set> when 'a: equality

    val getPage : Countable<'a> -> bigint -> bigint -> 'a list
    
    module Primitives =
        val forByte: Countable<byte>
        val forBool: Countable<bool>
        val forChar: Countable<char>
        val forInt32: Countable<int32>
        val forInt64: Countable<int64>
        val forSByte: Countable<sbyte>
        val forInt16: Countable<int16>
        val forUInt16: Countable<uint16>
        val forUInt32: Countable<uint32>
        val forUInt64: Countable<uint64>
        val forNativeInt: Countable<nativeint>
        val forUNativeInt: Countable<unativeint>
        val forDouble: Countable<double>
        val forSingle: Countable<single>
        val forDecimal: Countable<decimal>
        val forString: Countable<string>
        val forUnit: Countable<unit>