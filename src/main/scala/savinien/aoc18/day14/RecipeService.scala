package savinien.aoc18
package day14

import common.*
import zio.*

import parsers.TokenParsers.*

case class RecipeService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data    <- input.getData
      optimum <- ZioParse.parseAllToZio(integer)(data)
      result  <- ZIO.Succeed(Scoreboard(List(3, 7)).findPart1(optimum, 10))
    yield AdventStringResult(result)

  override def part2 = 
    for
      data    <- input.getData
      result  <- ZIO.Succeed(Scoreboard(List(3, 7)).findPart2(data))
    yield AdventNumResult(result)