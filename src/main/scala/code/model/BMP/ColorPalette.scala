package code.model.BMP

import scodec.bits.{BitVector, HexStringSyntax}
import scodec.codecs._
import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import shapeless.HNil

case class ColorPalette(rgbBlue: Int,
                        rgbGreen: Int,
                        rgbRed: Int,
                        rgbReserved: Int = 0){
  override def toString: String = s"ColorTable(Blue=%d,Green=%d,Red=%d,Rev=%d)".format(this.rgbBlue,this.rgbGreen,this.rgbRed,this.rgbReserved)
}

object ColorPalette {

  def apply(blue: Int, green: Int, red: Int) = new ColorPalette(rgbBlue = blue, rgbGreen = green, rgbRed = red)

  implicit val codec: Codec[ColorPalette] = {
    val componetCodec = {
      ("rgbBlue"      | uint8) ::
        ("rgbGreen"   | uint8) ::
        ("rgbRed"     | uint8) ::
        ("rgbReserved" | constant(hex"00"))
    }

    new Codec[ColorPalette] {
      override def sizeBound: SizeBound = SizeBound.exact(32)

      override def encode(table: ColorPalette): Attempt[BitVector] =
        for {
          encoded <- componetCodec.encode(table.rgbBlue :: table.rgbGreen :: table.rgbRed :: () :: HNil)
        } yield encoded

      override def decode(bits: BitVector): Attempt[DecodeResult[ColorPalette]] = componetCodec.decode(bits) map {
        _ map {
          h =>
            val t = h.tupled
            ColorPalette(t._1, t._2, t._3)
        }
      }
    }
  }
}
