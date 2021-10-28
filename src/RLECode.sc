
import code.model.BMP.{EOF, EOL, Encoded, RLECode, Skipped, Uncoded}
import scodec.bits.{BitVector, HexStringSyntax}
import scodec.codecs.{list, peek, uint8}
import scodec.{Attempt, Codec, DecodeResult, SizeBound}

val bits = hex"0304050600034556670002780002050102780000091E0001".bits

/*object RLECode {
  val componentCodec = peek(uint8::uint8)
  implicit val codec: Codec[RLECode] = new Codec[RLECode] {
    override def sizeBound: SizeBound = SizeBound.atLeast(16)

    override def encode(value: RLECode): Attempt[BitVector] = value match {
      case eol:EOL         => EOL.codec.encode(eol)
      case eof:EOF         => EOF.codec.encode(eof)
      case skipped:Skipped => Skipped.codec.encode(skipped)
      case unCoded:Uncoded => Uncoded.codec.encode(unCoded)
      case enCoded:Encoded => Encoded.codec.encode(enCoded)
    }

    override def decode(bits: BitVector): Attempt[DecodeResult[RLECode]] = componentCodec.decode(bits)
      .flatMap{ res =>
        res.value.toList match {
          case 0::0::Nil => EOL.codec.decode(res.remainder)
          case 0::1::Nil => EOF.codec.decode(res.remainder)
          case 0::2::Nil => Skipped.codec.decode(res.remainder)
          case 0::_::Nil => Uncoded.codec.decode(res.remainder)
          case _         => Encoded.codec.decode(res.remainder)
        }
      }
  }
}*/

val listCodec = list(RLECode.codec)
val decoded = listCodec.decode(bits).require.value
decoded.foreach(println)

val data = List(Encoded(3,4),Encoded(5,6),
  Uncoded(List(69,86,103)),Encoded(2,120),
  Skipped(5,1),Encoded(2,120),EOL(),Encoded(9,30),EOF())

val coded = listCodec.encode(data).require
println(coded.equals(bits))