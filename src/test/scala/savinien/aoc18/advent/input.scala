package savinien.aoc18.advent

import input.AdventInput
import zio.test.mock._
import zio._

object AdventInputMock extends Mock[AdventInput]:
  object GetData extends Effect[Unit, Nothing, List[String]]

  val compose: URLayer[Has[Proxy], AdventInput] =
    ZLayer.fromService { proxy =>
      new AdventInput.Service {
        def getData = proxy(GetData)
      }
    }
