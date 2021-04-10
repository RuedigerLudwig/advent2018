package savinien.aoc18
package common

import scala.annotation.targetName

import scala.math.Numeric
import scala.math.Numeric.Implicits.infixNumericOps

case class Pos[T: Numeric](x: T, y: T)

object Pos:
  extension [T: Numeric](p: Pos[T])
    @targetName("opAddPos")
    def `+`(p2: Pos[T]): Pos[T] =
      new Pos(p.x + p2.x, p.y + p2.y)

object posParsers:
  import parser.TokenParsers.*
  def posParser: Parser[Pos[Int]] = integer.tupSep2(char(',').token) ^^ { case (x, y) => Pos(x, y) }