package savinien.aoc18
package day03

import common._
import parser.Parsers._
import parser.Conversions.given
import scala.language.implicitConversions

case class Claim private(number: Int, area: Area)

object Claim:
  def apply(number: Int, left: Int, top: Int, width: Int, height: Int) =
    new Claim(number, Area(Pos(left, top), Pos(left+width-1, top+height-1)))

  def parser =
    ('#' >~> unsignedInteger) ~ (" @ " >~> unsignedInteger) ~ (',' >~> unsignedInteger) ~ (": " >~> unsignedInteger) ~ ('x' >~> unsignedInteger) ^^ {
      case number ~ left ~ top ~ width ~ height => Claim(number, left, top, width, height)
    }
  
  def fromStringList = ZioParser.parseAllToZio(Claim.parser.lines)

  def fromString = ZioParser.parseAllToZio(Claim.parser)