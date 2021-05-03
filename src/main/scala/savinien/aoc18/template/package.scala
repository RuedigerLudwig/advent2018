package savinien.aoc18

import common.*
import zio.*

package object dayXX:
  def live: URLayer[AdventInput, SingleDay] =
    ZLayer.fromService[ AdventInput.Service , SingleDay.Service ] { TemplateService.apply }
