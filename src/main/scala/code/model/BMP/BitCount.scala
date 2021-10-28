package code.model.BMP

import code.model.BMP
import scodec.Codec
import scodec.codecs.{enumerated, uint16, uint16L}

object BitCount extends Enumeration{self =>
  type BitCount = Value
  val _1Color     : Value    = Value(1)
  val _16Color    : Value    = Value(4)
  val _256Color   : Value    = Value(8)
  val _16TrueColor:Value     = Value(16)
  val _24TrueColor:Value     = Value(24)
  val _32TrueColor:Value     = Value(32)

  implicit val codec: Codec[BitCount] = enumerated(uint16L,BitCount)

  override def toString(): String = self match {
    case BitCount._1Color      => "1Color"
    case BitCount._16Color     => "16Color"
    case BitCount._256Color    => "256Color"
    case BitCount._16TrueColor    => "16TrueColor"
    case BitCount._24TrueColor    => "24TrueColor"
    case BitCount._32TrueColor    => "32TrueColor"
  }
}