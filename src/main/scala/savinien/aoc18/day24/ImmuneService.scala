package savinien.aoc18
package day24

import common.*
import zio.*

case class ImmuneService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 =
    for
      data     <- input.getData
      reindeer <- ImmuneService.fromString(data)
    yield AdventNumResult(reindeer.finishBattle.units)
    
  override def part2 =
    for
      data     <- input.getData
      reindeer <- ImmuneService.fromString(data)
    yield AdventNumResult(reindeer.boostedBattle.units)

object ImmuneService:
  val fromString = ZioParse.parseAllToZio(ReindeerHealth.parser)