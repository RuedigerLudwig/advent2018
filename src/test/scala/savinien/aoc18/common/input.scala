package savinien.aoc18
package common

import zio._
import zio.test.mock._

object AdventInputMock extends Mock[AdventInput]:
  object GetData extends Effect[Unit, Nothing, String]

  val compose: URLayer[Has[Proxy], AdventInput] =
    ZLayer.fromService { proxy =>
      new AdventInput.Service {
        def getData = proxy(GetData)
      }
    }
