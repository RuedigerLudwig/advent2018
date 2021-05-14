package savinien.aoc18
package day17

import common.*
import common.geometric.Point
import zio.*

case class ReservoirService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data    <- input.getData
      terrain <- ReservoirService.fromString(data)
      result = terrain.getReservoir(500)
    yield AdventNumResult(result.reach.size)

  override def part2 = 
    for
      data    <- input.getData
      terrain <- ReservoirService.fromString(data)
      result = terrain.getReservoir(500)
    yield AdventNumResult(result.filled.size)

object ReservoirService:
  def fromString(input: String) = ZioParse.parseAllToZio(Scanner.complete)(input)