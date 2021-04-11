package savinien.aoc18
package day03


import common.*
import parser.TokenParsers.*

case class Claim private(number: Int, area: Area[Int])

object Claim:
  def apply(number: Int, area: Area[Int]) =
    new Claim(number, area)

  def parser =
    (char('#') *> unsignedInteger) ~: (string(" @ ") *> areaParsers.areaSizeParser) ^^ {  case (number, area) => Claim(number, area) }
  
  def fromStringList = ZioParser.parseAllToZio(lines(Claim.parser))

  def fromString = ZioParser.parseAllToZio(Claim.parser)