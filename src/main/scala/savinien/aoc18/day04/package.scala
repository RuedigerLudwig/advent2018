package savinien.aoc18

import common._
import zio._

package object day04:
  def live: URLayer[AdventInput, SingleDay] =
    ZLayer.fromService[ AdventInput.Service , SingleDay.Service ] { GuardService(_) }
