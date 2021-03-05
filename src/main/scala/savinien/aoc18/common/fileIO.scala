package savinien.aoc18.common

import java.io.IOException
import scala.io.Source
import zio._

object FileReader:
  private def openFile(path: String): Managed[IOException, Source] =
    val acquire =
      ZIO(Source.fromResource(path)).refineToOrDie[IOException]
    val release = (source: Source) => ZIO(source.close()).orDie

    Managed.make(acquire)(release)

  def getContent(path: String): AdventTask[String] =
    openFile(path)
      .use { source =>
        ZIO(source.mkString)
      }
      .refineToOrDie[IOException]
      .catchAll {
        e => ZIO.fail(ThrowableException(e))
      }