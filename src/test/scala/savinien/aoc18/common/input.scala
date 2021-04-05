package savinien.aoc18
package common

import zio.*
import zio.test.mock.*

object AdventInputMock extends Mock[AdventInput]:
  object GetData extends Effect[Unit, Nothing, String]

  val compose: URLayer[Has[Proxy], AdventInput] =
    ZLayer.fromService: 
      proxy => new AdventInput.Service: 
        def getData = proxy(GetData)