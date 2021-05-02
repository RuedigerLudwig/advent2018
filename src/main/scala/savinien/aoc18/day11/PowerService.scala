package savinien.aoc18
package day11

import common.*
import geometric.Point
import geometric.Area

import parsers.TokenParsers.*

import zio.*

class PowerService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data   <- input.getData
      serial <- ZioParse.parseAllToZio(integer)(data)
      result <- PowerService.findMaxPowerFor(300)(serial)(3)
    yield AdventStringResult(s"${result.x},${result.y}")
  override def part2 = 
    for
      data   <- input.getData
      serial <- ZioParse.parseAllToZio(integer)(data)
      result <- PowerService.findMaxPower(300)(serial)
    yield AdventStringResult(s"${result._1.x},${result._1.y},${result._2}")

object PowerService:
  def findMaxPowerFor(size: Int)(serial: Int)(cellSize: Int): AdventTask[Point[Int]] =
    PowerGrid(size, serial).getMaxPowerFor(cellSize) match
      case singleMax :: Nil => ZIO.succeed(singleMax._1)
      case _                => ZIO.fail(NoMaxFound)

  def findMaxPower(size: Int)(serial: Int): AdventTask[(Point[Int], Int)] =
    PowerGrid(size, serial).getMaxPower match
      case Some((point, size, _)) => ZIO.succeed((point, size))
      case _                      => ZIO.fail(NoMaxFound)

end PowerService