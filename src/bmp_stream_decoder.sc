import cats.effect.{Blocker, ContextShift, IO}
import code.model.BMP.{BMPFile, BMPFileHeader}
import fs2.Stream
import scodec.bits.BitVector
import scodec.stream.StreamDecoder

import java.io.File
import java.nio.file.Paths
import scala.concurrent.ExecutionContext
//import cats.effect.internals.IO

implicit val cs:ContextShift[IO] = IO.contextShift(ExecutionContext.global)

//val path = Paths.get("D:\\itellJworks\\scodecBMPTest\\src\\test1.bmp")
val path = Paths.get("/run/media/ruoyu/Doc/itellJworks/scodecBMPTest/src/test1.bmp")
/*val bmpFileHeaderStreamDecoder: StreamDecoder[BMPFileHeader] = StreamDecoder.once(BMPFileHeader.codec)
val bmpFileHeader:Stream[IO,BMPFileHeader] =
  Stream
    .resource(Blocker[IO])
    .flatMap { blocker =>
      fs2.io.file
        .readAll[IO](path, blocker, 256).chunks
        .map(c => BitVector.view(c.toArray))
        .through(bmpFileHeaderStreamDecoder.toPipe)
    }

bmpFileHeader.compile.toVector.unsafeRunAsync{
  case Left(value) => value.printStackTrace()
  case Right(header) => header.foreach(println _)
}*/


val bmpStreamDecoder: StreamDecoder[BMPFile] = StreamDecoder.once(BMPFile.codec)
val bmp: Stream[IO,BMPFile] =
  Stream
    .resource(Blocker[IO])
    .flatMap { blocker =>
      fs2.io.file
        .readAll[IO](path, blocker, 128).chunks
        .map(c => BitVector.view(c.toArray))
        .through(bmpStreamDecoder.toPipe)
    }

bmp.compile.toVector.unsafeRunAsync{
  case Left(throwable) => throwable.printStackTrace()
  case Right(vector) => vector.foreach(x=> println(x.toString))
}

println("finished.")