package savinien.aoc18
package day03

import parsers.TokenParsers.*
import common.geometric.Area
import common.ZioParse

case class Claim private(number: Int, area: Area[Int])

object Claim:
  def apply(number: Int, area: Area[Int]) =
    new Claim(number, area)

  def parser =
    (char('#') *> unsignedInteger) ~: (string(" @ ") *> Area.Parsers.areaSizeParser[Int]) ^^ {  case (number, area) => Claim(number, area) }
  
  def fromStringList = ZioParse.parseAllToZio(lines(Claim.parser))

  def fromString = ZioParse.parseAllToZio(Claim.parser)