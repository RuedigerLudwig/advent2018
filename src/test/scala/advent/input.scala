package advent

import zio._
import zio.test.mock._
import savinien.aoc18.advent.input.AdventInput

object AdventInputMock extends Mock[AdventInput] {
  object GetData extends Effect[Unit, Nothing, List[String]]

  val compose: URLayer[Has[Proxy], AdventInput] =
    ZLayer.fromService { proxy =>
      new AdventInput.Service {
        def getData = proxy(GetData)
      }
    }
}
