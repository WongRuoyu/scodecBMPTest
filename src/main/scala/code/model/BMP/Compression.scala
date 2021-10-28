package code.model.BMP

import scodec.codecs.{enumerated, uint16L, uint32L}
import scodec.{Attempt, Codec}

object Compression extends Enumeration {
  type Compression = Value
  val NoCompression: Compression  = Value(0)
  val RLE8         : Compression  = Value(1)
  val RLE4         : Compression  = Value(2)
  val MASK         : Compression  = Value(3)

  private def L2I(x:Long) : Attempt[Int] = uint32L.encode(x).map(bv=>uint16L.decode(bv.take(16)).require.value)
  private def I2L(x:Int):Long = x.toLong
  implicit val codec: Codec[Compression] = enumerated(uint32L.narrow(L2I,I2L),Compression)
}
