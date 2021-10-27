import scodec.bits.HexStringSyntax

val bits1 = hex"01".bits
val bits2 = hex"02".bits
val res = bits1 ++ bits2
println(res.toHex)