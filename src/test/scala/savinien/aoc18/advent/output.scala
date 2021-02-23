package savinien.aoc18.advent

import output.AdventOutput
import zio.test.mock._
import zio._

object AdventOutputMock extends Mock[AdventOutput] { self =>
  object OutputInt    extends Effect[(Int, Int), Nothing, Unit]
  object OutputString extends Effect[(Int, String), Nothing, Unit]
  object Error        extends Effect[(Int, String), Nothing, Unit]

  private object empty extends Effect[Unit, Nothing, Unit]
  val Empty = empty().optional

  // def empty: ULayer[AdventOutput] = Expectation.NoCalls(self)

  val compose: URLayer[Has[Proxy], AdventOutput] =
    ZLayer.fromService { proxy =>
      new AdventOutput.Service {
        def outputInt(part: Int, result: Int): zio.UIO[Unit]       = proxy(OutputInt, part, result)
        def outputString(part: Int, result: String): zio.UIO[Unit] = proxy(OutputString, part, result)
        def error(part: Int, result: String): zio.UIO[Unit]        = proxy(Error, part, result)
      }
    }
}
