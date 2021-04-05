package savinien.aoc18

import zio.*
import common.*

package object day02:
  def live: URLayer[AdventInput, SingleDay] =
    ZLayer.fromService[ AdventInput.Service , SingleDay.Service ] { InventoryService(_) }