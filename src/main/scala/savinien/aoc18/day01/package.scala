package savinien.aoc18

import zio._
import advent._

package object day01:
  def live: URLayer[AdventInput with AdventOutput, SingleDay] =
    ZLayer.fromServices[
        AdventInput.Service
      , AdventOutput.Service
      , SingleDay.Service
    ] { (input, output) => ChronalService(input, output) }
