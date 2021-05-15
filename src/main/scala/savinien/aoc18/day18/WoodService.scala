package savinien.aoc18
package day18

import common.*
import zio.*

import parsers.TokenParsers.*

case class WoodService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data    <- input.getData
      landscape <- WoodService.fromString(data)
      result = landscape.ticks(10)
    yield AdventNumResult(result.value)

  override def part2 = 
    for
      data    <- input.getData
      landscape <- WoodService.fromString(data)
      result = landscape.ticks(1_000_000_000)
    yield AdventNumResult(result.value)

object WoodService:
  val fromString = ZioParse.parseAllToZio(Landscape.parser)