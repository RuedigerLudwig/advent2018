package savinien.aoc18
package day22

import common.*
import zio.*

case class CaveService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data <- input.getData
      map  <- CaveService.fromString(data)
    yield AdventNumResult(map.riskFactor)

  override def part2 = 
    for
      data <- input.getData
      map  <- CaveService.fromString(data)
    yield AdventNumResult(map.bestPath)

object CaveService:
  val fromString = ZioParse.parseAllToZio(Cave.parser)