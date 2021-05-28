package savinien.aoc18
package day23

import common.*
import common.geometric.Point3D
import zio.*
import parsers.TokenParsers.*

case class TeleportService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data <- input.getData
      swarm <- TeleportService.fromString(data)
    yield AdventNumResult(swarm.inRangeOfStrongest)

  override def part2 = 
    for
      data <- input.getData
      swarm <- TeleportService.fromString(data)
    yield AdventGenResult(swarm.teleportPoint.absM)


object TeleportService:
  val fromString = ZioParse.parseAllToZio(Swarm.parser)