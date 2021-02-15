package advent

import zio._
import zio.test.mock._
import savinien.aoc18.advent.output.AdventOutput

object AdventOutputMock extends Mock[AdventOutput] {
  object Output {
    object _0 extends Effect[(Int, String), Nothing, Unit]
    object _1 extends Effect[(Int, Int), Nothing, Unit]
  }

  val compose: URLayer[Has[Proxy], AdventOutput] =
    ZLayer.fromService { proxy =>
      new AdventOutput.Service {
        def output(part: Int, result: String) = proxy(Output._0, part, result)
        def output(part: Int, result: Int)    = proxy(Output._1, part, result)
      }
    }
}
