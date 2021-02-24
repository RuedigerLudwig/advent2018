package savinien.aoc18

import advent._
import zio._

package object dayXX {
  def live: URLayer[AdventInput with AdventOutput, SingleDay] =
    ZLayer.fromServices[
        AdventInput.Service
      , AdventOutput.Service
      , SingleDay.Service
    ] { (input, output) => TemplateService(input, output) }
}
