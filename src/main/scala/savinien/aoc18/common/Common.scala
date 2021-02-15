package savinien.aoc18.day01

import java.io.IOException
import scala.io.Source
import zio._

object Common {
  def openFile(path: String): Managed[IOException, Source] = {
    val acquire = ZIO(Source.fromResource(path)).refineToOrDie[IOException]
    val release = (source: Source) => ZIO(source.close()).orDie

    Managed.make(acquire)(release)
  }

  def getLines(path: String): ZIO[Any, IOException, List[String]] =
    openFile(path)
      .use { source =>
        ZIO(source.getLines().toList)
      }
      .refineToOrDie[IOException]

}
