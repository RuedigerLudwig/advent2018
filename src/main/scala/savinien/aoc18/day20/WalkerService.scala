package savinien.aoc18
package day20

import common.*
import zio.*

case class WalkerService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data <- input.getData
      path <- WalkerService.fromString(data)
    yield AdventNumResult(Walker.furthestRoom(path))

  override def part2 =
    for
      data     <- input.getData
      minDoors <- input.getIntSetting("MinDoors", 1_000)
      path     <- WalkerService.fromString(data)
    yield AdventNumResult(Walker.doorCount(path, minDoors))


object WalkerService:
  val fromString = ZioParse.parseToZio(Path.fullPath)