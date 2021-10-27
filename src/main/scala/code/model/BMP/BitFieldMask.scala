package code.model.BMP

import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs._

case class BitFieldMask(redMask:BitVector,greenMask:BitVector,blueMask:BitVector)

object BitFieldMask {
  implicit val codec: Codec[BitFieldMask] = {
    ("redMask"  | bits(32))::
      ("greenMask"  | bits(32))::
      ("blueMask"  | bits(32))
  }.as[BitFieldMask]
}
