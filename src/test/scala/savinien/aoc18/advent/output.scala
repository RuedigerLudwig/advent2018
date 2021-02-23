package savinien.aoc18
package advent

import advent._
import zio.test.mock._
import zio._

object AdventOutputMock extends Mock[AdventOutput] { self =>
  object OutputInt        extends Effect[(Int, Int), Nothing, Unit]
  object OutputSingleLine extends Effect[(Int, String), Nothing, Unit]
  object OutputMultiLine  extends Effect[(Int, String), Nothing, Unit]
  object Error            extends Effect[(Int, AdventException), Nothing, Unit]

  private object empty extends Effect[Unit, Nothing, Unit]
  val Empty = empty().optional

  // def empty: ULayer[AdventOutput] = Expectation.NoCalls(self)

  val compose: URLayer[Has[Proxy], AdventOutput] =
    ZLayer.fromService { proxy =>
      new AdventOutput.Service {
        def outputInt(part: Int, result: Int): zio.UIO[Unit]           = proxy(OutputInt, part, result)
        def outputSingleLine(part: Int, result: String): zio.UIO[Unit] = proxy(OutputSingleLine, part, result)
        def outputMultiLine(part: Int, result: String): zio.UIO[Unit]  = proxy(OutputMultiLine, part, result)
        def error(part: Int, error: AdventException): zio.UIO[Unit]    = proxy(Error, part, error)
      }
    }
}
