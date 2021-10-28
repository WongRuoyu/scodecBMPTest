import scodec.codecs.{limitedSizeBytes, uint8, utf8}
import scodec.Codec
import scodec.bits.HexStringSyntax
import scodec.codecs._
import shapeless.HNil
val componentCodec = peek(uint8 :: uint8)
case class Person(name:String,age:Int = 1:Int,t:String)


val pCodec = {
  ("name"     | limitedSizeBytes(2,utf8))::
    ("age"     | constant(hex"01"))::
    ("t"       | limitedSizeBytes(2,utf8))
}.dropUnits
