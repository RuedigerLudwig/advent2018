package savinien.aoc18.advent

import output.AdventOutput
import zio.test.mock._
import zio._

object AdventOutputMock extends Mock[AdventOutput] {
  object OutputInt    extends Effect[(Int, Int), Nothing, Unit]
  object OutputString extends Effect[(Int, String), Nothing, Unit]

  val compose: URLayer[Has[Proxy], AdventOutput] =
    ZLayer.fromService { proxy =>
      new AdventOutput.Service {
        def outputInt(part: Int, result: Int): zio.UIO[Unit]       = proxy(OutputInt, part, result)
        def outputString(part: Int, result: String): zio.UIO[Unit] = proxy(OutputString, part, result)
      }
    }
}
