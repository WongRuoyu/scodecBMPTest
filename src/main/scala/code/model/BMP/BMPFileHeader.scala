package code.model.BMP

import scodec.bits._
import scodec.codecs._
import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import shapeless.HNil


case class BMPFileHeader(bfType: String,
                         bfSize: Long,
                         bfReserved1: Int = 0,
                         bfReserved2: Int = 0,
                         bfOffBits: Long) {
  override val toString: String = s"BMPFileHeader(bfType:%s,bfSize:%d Bytes,bfOffBits:%d Bytes)".format(this.bfType,this.bfSize,this.bfOffBits)
}

object BMPFileHeader {

  def apply(bfType:String,bfSize:Long,bfOffBits:Long) =  new BMPFileHeader(bfType = bfType,bfSize = bfSize,bfOffBits=bfOffBits)

  implicit val codec: Codec[BMPFileHeader] = {
    val componet_Codec = {
      ("bfType" | fixedSizeBytes(2,ascii)) ::
        ("bfSize" | uint32L) ::
        ("bfReserved1" | constant(hex"0000")) ::
        ("bfReserved2" | constant(hex"0000")) ::
        ("bfOffBits" | uint32L)
    }

    new Codec[BMPFileHeader] {
      override def sizeBound: SizeBound = SizeBound.exact(112)

      override def encode(header: BMPFileHeader): Attempt[BitVector] = for {
        encoded <- componet_Codec.encode(header.bfType :: header.bfSize :: () :: () :: header.bfOffBits :: HNil)
      } yield encoded

      override def decode(bits: BitVector): Attempt[DecodeResult[BMPFileHeader]] = {
        componet_Codec.decode(bits) map {
          _ map {
            h =>
              val t = h.tupled
              BMPFileHeader(t._1, t._2, t._5)
          }
        }
      }
    }
  }
}