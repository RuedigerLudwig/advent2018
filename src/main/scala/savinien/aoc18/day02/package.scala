package savinien.aoc18

import zio._
import advent._

package object day02:
  def live: URLayer[AdventInput with AdventOutput, SingleDay] =
    ZLayer.fromServices[
        AdventInput.Service
      , AdventOutput.Service
      , SingleDay.Service
    ] { (input, output) => InventoryService(input, output) }