package savinien.aoc18
package day03

import common.*
import parser.TokenParsers.*
import scala.language.implicitConversions

case class Claim private(number: Int, area: Area)

object Claim:
  def apply(number: Int, left: Int, top: Int, width: Int, height: Int) =
    new Claim(number, Area(Pos(left, top), Pos(left+width-1, top+height-1)))

  def parser =
    (char('#') *> unsignedInteger) ~: (string(" @ ") *> unsignedInteger) ~: (char(',') *> unsignedInteger) ~: (string(": ") *> unsignedInteger) ~: (char('x') *> unsignedInteger) ^^ {
      case (number, left, top, width, height) => Claim(number, left, top, width, height)
    }
  
  def fromStringList = ZioParser.parseAllToZio(lines(Claim.parser))

  def fromString = ZioParser.parseAllToZio(Claim.parser)