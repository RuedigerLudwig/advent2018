package savinien.aoc18
package day03

import common._
import common.ZioParser._

case class Claim private(number: Int, area: Area)

object Claim:
  def apply(number: Int, left: Int, top: Int, width: Int, height: Int) =
    new Claim(number, Area(Pos(left, top), Pos(left+width-1, top+height-1)))

  def parser =
    lead("#", unsignedInteger) ~ lead("@", unsignedInteger) ~ lead(",", unsignedInteger) ~ lead(":", unsignedInteger) ~ lead("x", unsignedInteger) ^^ {
      case number ~ left ~ top ~ width ~ height => Claim(number, left, top, width, height)
    }
  
  def fromStringList = parseAllToZioList(Claim.parser)

  def fromString = parseAllToZio(Claim.parser)