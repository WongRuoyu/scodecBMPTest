package code.model.BMP

import scodec.bits.{BitVector, HexStringSyntax}
import scodec.codecs.{Discriminator, ascii, bits, constant, discriminated, list, listOfN, peek, uint4, uint8, _}
import scodec.{Attempt, Codec, DecodeResult, SizeBound, Transformer}
import shapeless.HNil


sealed trait CompressedItem extends BMPDataItem

object CompressedItem {

  implicit val discriminator: Discriminator[BMPDataItem, CompressedItem, Int] = Discriminator(1)

  implicit val codec: Codec[CompressedItem] = discriminated[CompressedItem]
    .by(uint16)
    .typecase(0, RLECode.codec)
    .typecase(1, RGBCode.codec)
}

sealed trait RGBCode extends CompressedItem

object RGBCode {

  implicit val discriminator: Discriminator[CompressedItem, RGBCode, Int] = Discriminator(1)

  implicit val codec: Codec[RGBCode] = discriminated[RGBCode]
    .by(uint16)
    .typecase(16, RGBCode16.codec)
    .typecase(32, RGBCode32.codec)
}

case class RGBCode16(bits: BitVector) extends RGBCode

object RGBCode16 {
  implicit val codec: Codec[RGBCode16] = bits(16L).as[RGBCode16]
  implicit val discriminator: Discriminator[RGBCode, RGBCode16, Int] = Discriminator(16)
}


case class RGBCode32(bits: BitVector) extends RGBCode

object RGBCode32 {
  implicit val codec: Codec[RGBCode32] = bits(32).as[RGBCode32]
  implicit val discriminator: Discriminator[RGBCode, RGBCode32, Int] = Discriminator(32)
}


sealed trait RLECode extends CompressedItem

object RLECode {

  implicit val discriminator: Discriminator[CompressedItem, RLECode, Int] = Discriminator(0)

  val componentCodec: Codec[Int :: Int :: HNil] = peek(uint8 :: uint8)
  implicit val codec: Codec[RLECode] = new Codec[RLECode] {
    override def sizeBound: SizeBound = SizeBound.atLeast(16)

    override def encode(value: RLECode): Attempt[BitVector] = value match {
      case eol: EOL => EOL.codec.encode(eol)
      case eof: EOF => EOF.codec.encode(eof)
      case skipped: Skipped => Skipped.codec.encode(skipped)
      case unCoded: Uncoded => Uncoded.codec.encode(unCoded)
      case enCoded: Encoded => Encoded.codec.encode(enCoded)
    }

    override def decode(bits: BitVector): Attempt[DecodeResult[RLECode]] = componentCodec.decode(bits)
      .flatMap { res =>
        res.value match {
          case List(0, 0) => EOL.codec.decode(res.remainder)
          case List(0, 1) => EOF.codec.decode(res.remainder)
          case List(0, 2) => Skipped.codec.decode(res.remainder)
          case List(0, _) => Uncoded.codec.decode(res.remainder)
          case _ => Encoded.codec.decode(res.remainder)
        }
      }
  }
}


case class Encoded(count: Int, data: Int) extends RLECode

object Encoded {
  implicit val codec: Codec[Encoded] = {
    ("count" | uint8) ::
      ("value" | uint8)
  }.as[Encoded]

  implicit val discriminator: Discriminator[RLECode, Encoded, Int] = Discriminator(0)
}

case class Uncoded(data: List[Int]) extends RLECode

object Uncoded {
  implicit val codec: Codec[Uncoded] = new Codec[Uncoded] {

    override def sizeBound: SizeBound = SizeBound.atLeast(32)

    override def encode(value: Uncoded): Attempt[BitVector] = {
      val prefix: BitVector = hex"00".bits
      val len: BitVector = uint8.encode(value.data.length).require
      val data: BitVector = list(uint8).encode(value.data).require
      val numMoreBytes: Int = value.data.length % 2
      //      val numOfBytes:Int = 1+1+ (value.data.length / 2)*2 + numMoreBytes
      val numOfBytes: Int = 1 + 1 + value.data.length + numMoreBytes
      val re = (prefix ++ len ++ data).padTo(numOfBytes * 8)
      Attempt.successful(re)
    }

    override def decode(bits: BitVector): Attempt[DecodeResult[Uncoded]] = {
      val codec = constant(hex"00") :: listOfN(uint8, uint8)
      val res = codec.decode(bits).map { re =>
        val remainder = if (re.value.tail.head.length % 2 == 0) re.remainder else re.remainder.drop(8)
        DecodeResult(Uncoded(re.value.tail.head), remainder)
      }
      res
    }
  }

  implicit val discriminator: Discriminator[RLECode, Uncoded, Int] = Discriminator(1)
}

case class Skipped(xOffset: Int, yOffset: Int) extends RLECode

object Skipped {

  implicit val discriminator: Discriminator[RLECode, Skipped, Int] = Discriminator(2)

  implicit val codec: Codec[Skipped] = {
    val codec: Codec[Skipped] = {
      ("xOffset" | uint8) ::
        ("yOffset" | uint8)
    }.as[Skipped]

    new Codec[Skipped] {
      override def sizeBound: SizeBound = SizeBound.exact(32)

      override def encode(value: Skipped): Attempt[BitVector] = {
        val encoded: BitVector = codec.encode(value).require
        val prefix: BitVector = hex"0002".bits
        Attempt.successful(prefix ++ encoded)
      }

      override def decode(bits: BitVector): Attempt[DecodeResult[Skipped]] =
        (constant(hex"0002") :: codec).decode(bits).map { res =>
          DecodeResult(res.value.tail.head, res.remainder)
          //          DecodeResult(res.value.tail.head,BitVector.empty)
        }
    }
  }
}


/*
* the end of the scan-line
* */
case class EOL(bits: BitVector = hex"0000".bits) extends RLECode

object EOL {
  def apply(data: Any) = new EOL(bits = hex"0000".bits)

  def apply() = new EOL(bits = hex"0000".bits)

  implicit val discriminator: Discriminator[RLECode, EOL, Int] = Discriminator(3)
  implicit val codec: Codec[EOL] = bits(16).as[EOL]
}

/*
* the end of the BMP data
* */
case class EOF(bits: BitVector = hex"0001".bits) extends RLECode

object EOF {
  def apply() = new EOF(bits = hex"0001".bits)

  implicit val discriminator: Discriminator[RLECode, EOF, Int] = Discriminator(4)
  implicit val codec: Codec[EOF] = bits(16).as[EOF]
}

/*final case class SixteenBitsItem(bits: BitVector) extends CompressedItem

object SixteenBitsItem {
  implicit val codec: Codec[SixteenBitsItem] = bits(16).as[SixteenBitsItem]
  //  implicit val discriminator: Discriminator[UnCompressedItem,EightBitsItem,Int] = Discriminator(16)
  implicit val discriminator: Discriminator[CompressedItem, SixteenBitsItem, Int] = Discriminator(2)

  //  implicit val transformer: Transformer[SixteenBitsItem,BMPDataItem] =
}*/


// Unpressed------------------------------------------------------------------------------------
sealed trait UnCompressedItem extends BMPDataItem

object UnCompressedItem {
  implicit val codec: Codec[UnCompressedItem] = discriminated[UnCompressedItem]
    .by(uint8)
    .typecase(0,ColorPalettedItem.codec)
    .typecase(1,ColoredItem.codec)
}

sealed trait ColorPalettedItem extends UnCompressedItem

object ColorPalettedItem {
  implicit val discriminator: Discriminator[UnCompressedItem,ColorPalettedItem,Int] = Discriminator(0)
  implicit val codec: Codec[ColorPalettedItem] = discriminated[ColorPalettedItem]
    .by(uint8)
    .typecase(1,MonoItem.codec)
    .typecase(4,FourBitsItem.codec)
    .typecase(8,EightBitsItem.codec)
}

sealed trait ColoredItem extends UnCompressedItem

object ColoredItem {
  implicit val discriminator: Discriminator[UnCompressedItem,ColoredItem,Int] = Discriminator(1)
  implicit val codec: Codec[ColoredItem] = discriminated[ColoredItem]
    .by(uint8)
    .typecase(24,TwentiesBitsColoredItem.codec)
    .typecase(32,ThirtiesBitsColoredItem.codec)
}

// subclasses for ColorPalettedItem---------------------------
case class MonoItem(index: Int) extends ColorPalettedItem

object MonoItem {
  implicit val codec: Codec[MonoItem] = uint(1).as[MonoItem]
  implicit val discriminator: Discriminator[UnCompressedItem, FourBitsItem, Int] = Discriminator(1)
}

//4-bits item
case class FourBitsItem(index: Int) extends ColorPalettedItem

object FourBitsItem {
  implicit val codec: Codec[FourBitsItem] = uint4.as[FourBitsItem]
  implicit val discriminator: Discriminator[UnCompressedItem, FourBitsItem, Int] = Discriminator(4)
}

//8-bits item
case class EightBitsItem(index: Int) extends ColorPalettedItem

object EightBitsItem {
  implicit val codec: Codec[EightBitsItem] = uint8.as[EightBitsItem]
  implicit val discriminator: Discriminator[UnCompressedItem, EightBitsItem, Int] = Discriminator(8)
}
// subclasses for ColoredItem-----------------------------------

// the 16-bit bmp data must be compessed by Compression.MASK
//case class SixteenBitsColoredItem(bits:BitVector) extends ColoredItem

/*object SixteenBitsColoredItem {

}*/

case class TwentiesBitsColoredItem(blue:Int,green: Int,red: Int) extends ColoredItem {
  override val toString: String = s"TwentiesBitsColoredItem(red:%d,green:%d,blue:%d)".format(red,green,blue)
}
  /*case class TwentiesBitsColoredItem(red:Int,green: Int,blue: Int) extends ColoredItem {
    override val toString: String = s"TwentiesBitsColoredItem(red:%d,green:%d,blue:%d)".format(red,green,blue)
  }*/

object TwentiesBitsColoredItem {
  implicit val codec: Codec[TwentiesBitsColoredItem] = {
      ("blue" | uint8) ::
        ("green" | uint8) ::
        ("red" | uint8)
  }.as[TwentiesBitsColoredItem]
  implicit val discriminator: Discriminator[UnCompressedItem, EightBitsItem, Int] = Discriminator(24)
}

case class ThirtiesBitsColoredItem(red:Int,green:Int,blue:Int,alpha:Int) extends ColoredItem{
  override def toString: String = s"ThirtiesBitsColoredItem(red:%d,green:%d,blue:%d,alpha:%d)".format(red,green,blue,alpha)
}

object ThirtiesBitsColoredItem {
  implicit val codec: Codec[ThirtiesBitsColoredItem] = {
    ("red"  | uint8)::
      ("green"  | uint8)::
      ("blue"  | uint8)::
      ("alpha"  | uint8)
  }.as[ThirtiesBitsColoredItem]
  implicit val discriminator: Discriminator[UnCompressedItem, EightBitsItem, Int] = Discriminator(32)
}


sealed trait BMPDataItem

object BMPDataItem {
  implicit val codec: Codec[BMPDataItem] = discriminated[BMPDataItem]
    .by(uint16)
    .typecase(0, UnCompressedItem.codec)
    .typecase(1, CompressedItem.codec)
}