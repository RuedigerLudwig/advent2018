package savinien.aoc18
package day08

import common.*
import zio.*

import parser.TokenParsers.*

class ManeuverService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = calculate(ManeuverService.metaSumPattern)
  override def part2 = calculate(ManeuverService.selectPattern)

  def calculate(p: Parser[Int]): AdventTask[AdventResult] =
    for
      data   <- input.getData
      result <- ZioParse.parseAllToZio(p)(data)
    yield AdventIntResult(result)

object ManeuverService:
  def num    = unsignedInteger.token
  def posNum = num ^? { case n if n > 0 => n }
  def node(f: (List[Int], List[Int]) => Int): Parser[Int] =
    for
      numChild   <- num
      numMeta    <- posNum
      childNodes <- node(f).repeat(numChild)
      meta       <- num.repeat(numMeta)
    yield f(childNodes, meta)

  def metaSumPattern: Parser[Int] = node { _.sum + _.sum } <* space

  def calcSelectValue(childNodes: List[Int], meta: List[Int]): Int = 
    if childNodes.isEmpty then meta.sum
    else meta.map(_ - 1).collect(childNodes).sum

  def selectPattern: Parser[Int] = node { calcSelectValue } <* space