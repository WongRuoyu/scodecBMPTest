import scodec.codecs.{enumerated, uint16, uint32}
import scodec.{Attempt, Codec}

object Compression extends Enumeration {
  type Compression = Value
  val None = Value(0)
  val RLE8 = Value(1)
  val RLE4 = Value(2)
  val MASK = Value(3)

  def L2I(x:Long) : Attempt[Int] = uint32.encode(x).map(bv=>uint16.decode(bv.takeRight(16)).require.value)
  def I2L(x:Int):Long = x.toLong
  implicit val codec: Codec[Compression] = enumerated(uint32.narrow(L2I,I2L),Compression)
}

val codec = Compression.codec

val noCompresson = Compression.None
val noCompression_e = codec.encode(noCompresson).require
val noCompression_d = codec.decode(noCompression_e).require.value

val RLE8 = Compression.RLE8
val RLE8_e = codec.encode(RLE8).require
val RLE8_d = codec.decode(RLE8_e).require.value

val RLE4 = Compression.RLE4
val RLE4_e = codec.encode(RLE4).require
val RLE4_d = codec.decode(RLE4_e).require.value

val MASK = Compression.MASK
val MASK_e = codec.encode(MASK).require
val MASK_d = codec.decode(MASK_e).require.value
