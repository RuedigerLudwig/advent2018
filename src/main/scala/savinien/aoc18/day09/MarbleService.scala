package savinien.aoc18
package day09

import common.*
import parsers.TokenParsers.*
import zio.*
import collection.Iterator

class MarbleService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data <- input.getData
      (players, marbels) <- ZioParse.parseAllToZio(MarbleService.pattern)(data)
    yield AdventNumResult(MarbleService.playGame(players, marbels)._2)

  override def part2 = 
    for
      data <- input.getData
      (players, marbels) <- ZioParse.parseAllToZio(MarbleService.pattern)(data)
    yield AdventNumResult(MarbleService.playGame(players, marbels * 100)._2)

object MarbleService:
  def num = unsignedInteger.token
  def pattern: Parser[(Int, Int)] = num ~: num.between(string("players; last marble is worth"), string ("points")) 

  private[day09] def calcPoints(players: Int, marbles: Int)(using list: MarbleIterator): Map[Int, Long] =
    list.takeWhile((_, marble) => marble < marbles).foldLeft(Map.empty) {
      case (map, (add, marble)) => 
        val player = ((marble - 1) % players) + 1
        map.updated(player, map.get(player).getOrElse(0L) + add.toLong + marble.toLong)
    }

  private[day09] def playGame(players: Int, marbles: Int)(using MarbleIterator): (Int, Long) =
    calcPoints(players, marbles).maxBy((_, p) => p)

end MarbleService