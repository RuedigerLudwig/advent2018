package savinien.aoc18
package advent

import advent._
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
