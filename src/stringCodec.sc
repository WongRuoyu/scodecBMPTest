
import scodec.Codec
import scodec.bits.HexStringSyntax
import scodec.codecs._
val decoded1 = fixedSizeBytes(2,ascii).decode(hex"424d".bits)
val decoded1 = fixedSizeBytes(2,utf8).decode(hex"424d".bits)
val decoded = fixedSizeBytes(2,utf8).decode(hex"8950".bits)

case class Person(name:String,age:Int,t:String)

val pCodec: Codec[Person] = {
  ("name"     | limitedSizeBytes(2,utf8))::
    ("age"     | int8)::
    ("t"       | limitedSizeBytes(2,utf8))
}.as[Person]


val p1 = Person("P1",2,"t1")
val p1_encoded = pCodec.encode(p1).require
println(p1_encoded.toHex)
val p1_decoded  = pCodec.decode(p1_encoded).require
println(p1_decoded)