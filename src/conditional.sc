import scodec.Codec
import scodec.codecs.{bool, conditional, ignore, int64, uint8, utf8_32}
import shapeless.HList.ListCompat.::
import scodec.Codec._
import shapeless.HNil

case class Flags(x:Boolean,y:Boolean,z:Boolean)

val flagsCodec = (bool :: bool :: bool :: ignore(5)).as[Flags]

val codec = flagsCodec.flatPrepend {flags =>
  conditional(flags.x,uint8) :: conditional(flags.y,int64) :: conditional(flags.z,utf8_32)
}

//val codecTyped: Codec[(Flags,Option[Int],Option[Long],Option[String])] = codec

//val codecTyped: Codec[Flags :: Option[Int] :: Option[Long] :: Option[String] :: HNil] = codec

val v1 = Flags(x = true,y = true,z = true) :: Some(1) :: Some(1L) :: Some("Hi") :: HNil

val res1 = codec.encode(v1).require

val v2 = Flags(x=true,y = true,z = false) :: Some(1) :: Some(1L) :: Some("Hi") :: HNil
val res2 = codec.encode(v2).require.toBin
val v3 = Flags(x = true,y = false,z = false) :: Some(1) :: Some(1L) :: Some("Hi") :: HNil
val res2 = codec.encode(v3).require.toBin
