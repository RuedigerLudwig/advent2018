package savinien.aoc18

import common.*
import zio.*

package object day07:
  def live: URLayer[AdventInput, SingleDay] =
    ZLayer.fromService[ AdventInput.Service , SingleDay.Service ] { PartsService(_) }
