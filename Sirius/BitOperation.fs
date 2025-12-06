[<AutoOpen>]
module BitOperation
let inline Square (x: System.UInt128) = Common.Square_NB - 1 - int(System.UInt128.TrailingZeroCount(x))
