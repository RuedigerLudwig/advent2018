package savinien.aoc18
package day15

import common.*
import parsers.TokenParsers.*
import zio.*

case class BanditService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data        <- input.getData
      battlefield <- BanditService.fromString(data)
      result      <- ZIO.succeed(battlefield.calcWinner)
    yield AdventNumResult(result)

  override def part2 = 
    for
      data        <- input.getData
      battlefield <- BanditService.fromString(data)
      result      <- ZIO.succeed(battlefield.elvesWin)
    yield AdventNumResult(result)

object BanditService:
  var parser = Tile.parser.*.lines ^^ BattleField.apply
  def fromString(input: String) = ZioParse.parseAllToZio(parser)(input)

end BanditService