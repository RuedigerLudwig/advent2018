package savinien.aoc18.common

import zio._
import java.io.IOException
import scala.io.Source

type AdventInput = Has[AdventInput.Service]

object AdventInput:
  trait Service:
    def getData: IO[AdventException, String];

  def live(day: Int): Layer[Nothing, AdventInput] =
    ZLayer.succeed[AdventInput.Service] {
      new Service {
        override def getData: IO[AdventException, String] =
          getContent(f"input/day$day%02d.txt")

        private def openFile(path: String): Managed[IOException, Source] =
          val acquire =
            ZIO(Source.fromResource(path)).refineToOrDie[IOException]
          val release = (source: Source) => ZIO(source.close()).orDie

          Managed.make(acquire)(release)

        private def getContent(path: String): ZIO[Any, AdventException, String] =
          openFile(path)
            .use { source =>
              ZIO(source.mkString)
            }
            .refineToOrDie[IOException]
            .catchAll {
              e => ZIO.fail(ThrowableException(e))
            }

      }
    }

def getData: ZIO[AdventInput, Throwable, String] =
  ZIO.accessM(_.get.getData)
