package savinien.aoc18.advent

import zio._
import zio.macros.accessible
import java.io.IOException
import scala.io.Source

object input {
  type AdventInput = Has[AdventInput.Service]

  @accessible
  object AdventInput {
    trait Service {
      def getData: Task[List[String]];
    }

    val live = (day: Int) =>
      ZLayer.succeed {
        new Service {
          override def getData: Task[List[String]] =
            getLines(f"input/day$day%02d.txt")

          private def openFile(path: String): Managed[IOException, Source] = {
            val acquire = ZIO(Source.fromResource(path)).refineToOrDie[IOException]
            val release = (source: Source) => ZIO(source.close()).orDie

            Managed.make(acquire)(release)
          }

          private def getLines(path: String): ZIO[Any, IOException, List[String]] =
            openFile(path)
              .use { source =>
                ZIO(source.getLines().toList)
              }
              .refineToOrDie[IOException]

        }

      }
  }
}
