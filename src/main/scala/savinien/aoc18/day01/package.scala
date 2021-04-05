package savinien.aoc18

import zio.*
import common.*

package object day01:
  def live: URLayer[AdventInput, SingleDay] =
    ZLayer.fromService[ AdventInput.Service , SingleDay.Service ] { ChronalService(_) }
