[<AutoOpen>]

module Move
let inline Init() = uint32(0)
let inline InitNull() = uint32(1 <<< 23)
let inline To(x :uint32) = x &&& uint32(0x007f)
let inline From(x :uint32) = (x >>> 7) &&& uint32(0x007f)
let inline FlagPromo(x :uint32) = (x >>> 14) &&& uint32(1)
let inline PieceType(x :uint32) = (x >>> 15) &&& uint32(0x000f)
let inline CapPiece(x :uint32) = (x >>> 19) &&& uint32(0x000f)
let inline Pack(f:uint32, t:uint32, pc:uint32, cap:uint32, pro:uint32) = uint32(cap <<< 19) ||| uint32(pc <<< 15) ||| uint32(pro <<< 14) ||| uint32(f <<< 7) ||| uint32(t)