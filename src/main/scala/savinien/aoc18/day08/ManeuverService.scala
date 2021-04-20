package savinien.aoc18
package day08

import common.*
import zio.*

import parser.TokenParsers.*

class ManeuverService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data   <- input.getData
      node <- ManeuverService.parseNodes(data)
    yield AdventIntResult(node.metaSum)

  override def part2 = 
    for
      data <- input.getData
      node <- ManeuverService.parseNodes(data)
    yield AdventIntResult(node.value)

object ManeuverService:
  private[day08] def parseNodes(input: String) =
    ZioParse.parseAllToZio(Node.pattern)(input)
end ManeuverService