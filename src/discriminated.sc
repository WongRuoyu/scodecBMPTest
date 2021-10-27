import scodec.Codec
import scodec.codecs.{Discriminator, ascii, discriminated, uint8}

sealed trait People

object People {
  implicit val codec: Codec[People] = discriminated[People].by(uint8)
    .typecase(2,Worker.codec)
    .typecase(3,Farmer.codec)
}
case class Farmer(name:String) extends People

object Farmer {
  implicit val discriminator: Discriminator[People,Farmer,Int] = Discriminator(3)
  implicit val codec: Codec[Farmer] = ascii.as[Farmer]
}
case class Worker(age:Int) extends People

object Worker {
  implicit val discriminator: Discriminator[People,Worker,Int] = Discriminator(2)
  implicit val codec: Codec[Worker] = uint8.as[Worker]
}


val worker = Worker(6)
val encoded = Worker.codec.encode(worker).require // 0x06
val encoded2 = People.codec.encode(worker).require // 0x0206