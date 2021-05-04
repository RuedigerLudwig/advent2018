package savinien.aoc18
package day12

import common.*
import zio.*

case class SustainService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data     <- input.getData
      treeLine <- TreeLine.fromString(data)
      result   <- ZIO.succeed(treeLine.spreadTimes(20))
    yield AdventNumResult(result.value)

  override def part2 = 
    for
      data     <- input.getData
      treeLine <- TreeLine.fromString(data)
      result   <- ZIO.succeed(treeLine.spreadTimes(50_000_000_000))
      //result   <- ZIO.succeed(treeLine.spreadTimes(500))
    yield AdventNumResult(result.value)

object SustainService:
end SustainService