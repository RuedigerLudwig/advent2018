package savinien.aoc18
package day08

import common.*
import zio.*

import parser.TokenParsers.*

class ManeuverService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data <- input.getData
      meta <- ZioParse.parseAllToZio(ManeuverService.metaPattern)(data)
    yield AdventIntResult(meta)

  override def part2 = 
    for
      data <- input.getData
      value <- ZioParse.parseAllToZio(ManeuverService.valuePattern)(data)
    yield AdventIntResult(value)

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

  def metaPattern = node { _.sum + _.sum } <* space

  def calcValue(childNodes: List[Int], meta: List[Int]): Int = 
    if childNodes.isEmpty then meta.sum
    else meta.map(_ - 1).collect(childNodes).sum

  def valuePattern = node { calcValue } <* space