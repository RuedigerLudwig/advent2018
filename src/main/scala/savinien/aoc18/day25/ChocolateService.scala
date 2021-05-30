package savinien.aoc18
package day25

import common.*
import zio.*

import parsers.TokenParsers.*

case class ChocolateService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 =
    for
      data   <- input.getData
      points <- ChocolateService.fromString(data)
      constellations = Constellations.mergeCoordinates(points)
    yield AdventNumResult(constellations.length)

  override def part2 = ZIO.succeed(AdventStringResult("Congrats"))

object ChocolateService:
  val fromString = ZioParse.parseAllToZio(Point4D.parser[Int].lines)