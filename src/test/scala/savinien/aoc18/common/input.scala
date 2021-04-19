package savinien.aoc18
package common

import zio.*
import zio.test.mock.*
import zio.test.Assertion.*

object AdventInputMock extends Mock[AdventInput]:
  object GetData extends Effect[Unit, Nothing, String]
  object GetIntSetting extends Effect[(String, Int), Nothing, Int]

  val compose: URLayer[Has[Proxy], AdventInput] =
    ZLayer.fromService {
      proxy => new AdventInput.Service {
        def getData = proxy(GetData)
        def getIntSetting(setting: String, default: Int) = proxy(GetIntSetting, setting, default)
    }}

object AdventAssertions:
  def isSetting(setting: String) =
    isCase[(String, Any), Unit]("setting", 
      (s, _) => if s == setting then Some(()) else None, 
      anything)
