import cats.effect.IO
import fs2.Stream

val eff:Stream[IO,Int] = Stream.eval(IO {println("Begin Run!!");1+1})
val eff_compiled = eff.compile
eff.compile.toVector.unsafeRunAsync(x=> println(x.right.get))