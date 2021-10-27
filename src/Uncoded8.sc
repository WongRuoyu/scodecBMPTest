import code.model.BMP.Uncoded
import scodec.bits.HexStringSyntax
import scodec.codecs.{constant, listOfN, uint8}

println(4%2)
println(4/2)

println(7%2)
println(7/2)

val bits = hex"000345566700".bits

val codec1 = constant(hex"00"):: listOfN(uint8,uint8)
val c1 = codec1.decode(bits).require


val codec = Uncoded.codec
val decoed = codec.decode(bits).require


//val codec =  constant(hex"00")::uint8 :: HNil
//codec.toCodec.decode(hex"0003050406".bits)

/*case class Uncoded8(data:List[Int])

object Uncoded8{
  implicit val codec: Codec[Uncoded8] = new Codec[Uncoded8]{
    override def sizeBound: SizeBound = SizeBound.atLeast(32)
    override def encode(value: Uncoded8): Attempt[BitVector] = {
      val listCodec=list(uint8)
      val prefix: BitVector = hex"00".bits
      val len: BitVector = uint8.encode(value.data.length).require
      val data: BitVector = list(uint8).encode(value.data).require
      val numMoreBytes: Int = value.data.length % 2
      val numOfBytes:Int = 1+1+ (value.data.length / 2)*2 + numMoreBytes
//      val numOfBytes:Int = 1+1+ value.data.length + numMoreBytes
      val re = (prefix ++ len ++ data).padTo(numOfBytes*8)
      Attempt.successful(re)
    }

    override def decode(bits: BitVector): Attempt[DecodeResult[Uncoded8]] = {
      val codec = constant(hex"00"):: listOfN(uint8,uint8)
      val res = codec.decode(bits).map(re =>
        DecodeResult(Uncoded8(re.value.tail.head),BitVector.empty)
      )
      res
    }
  }
}*/


val unC = Uncoded(List(4,5,6,7,8,9,10,11))
val encoded = Uncoded.codec.encode(unC).require
println(encoded.toHex)
val decoded = Uncoded.codec.decode(encoded).require.value
println(decoded)
