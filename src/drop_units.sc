import scodec.codecs.{limitedSizeBytes, uint8, utf8}
import scodec.Codec
import scodec.bits.HexStringSyntax
import scodec.codecs._

case class Person(name:String,age:Int = 1:Int,t:String)


val pCodec = {
  ("name"     | limitedSizeBytes(2,utf8))::
    ("age"     | constant(hex"01"))::
    ("t"       | limitedSizeBytes(2,utf8))
}.dropUnits


val p1 = Person("P1",2,"t1")
val p1_encoded = pCodec.encode(p1).require
println(p1_encoded.toHex)
val p1_decoded  = pCodec.decode(p1_encoded).require
println(p1_decoded)