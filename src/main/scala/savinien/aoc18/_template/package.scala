package savinien.aoc18

import advent._
import zio._

package object dayXX:
  def live: URLayer[AdventInput, SingleDay] =
    ZLayer.fromService[ AdventInput.Service , SingleDay.Service ] { TemplateService(_) }
