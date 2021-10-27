import code.model.BMP.BitCount

/*object BitCount extends Enumeration{
  type BitCount = Value
  val _1Color         = Value(1)
  val _16Color        = Value(4)
  val _256Color       = Value(8)
  val _16TrueColor    = Value(16)
  val _24TrueColor    = Value(24)
  val _32TrueColor    = Value(32)

  implicit val codec: Codec[BitCount] = enumerated(uint16,BitCount)
}*/

val color1 = BitCount._1Color
val codec = BitCount.codec
val encoded = codec.encode(color1)
val color16 = BitCount._16Color
val encoded2 = codec.encode(color16)

val color16T = BitCount._16TrueColor
val color16T_v = color16T.id
val color16T_e = codec.encode(color16T)
val color16T_d = codec.decode(color16T_e.require).require.value