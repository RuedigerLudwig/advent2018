package savinien.aoc18
package common

import scala.annotation.targetName

import scala.math.Numeric
import scala.math.Numeric.Implicits.infixNumericOps

object pos:
  case class Pos[T: Numeric](x: T, y: T):
    override def toString: String = s"Pos($x, $y)"

  object Pos:
    def origin[T: Numeric]: Pos[T] = 
      val zero = summon[Numeric[T]].zero
      Pos(zero, zero)

    extension [T: Numeric](p: Pos[T])
      @targetName("opAddPos")
      def `+`(p2: Pos[T]): Pos[T] = Pos(p.x + p2.x, p.y + p2.y)

      @targetName("opSubPos")
      def `-`(p2: Pos[T]): Pos[T] = Pos(p.x - p2.x, p.y - p2.y)

      /**
       * The Manhatten distance of this Pos from the origin
       */
      def absM: T = p.x.abs + p.y.abs

  object Parsers extends parser.TokenParsers:
    def posParser: Parser[Pos[Int]] = integer.tupSep2(char(',').token) ^^ { case (x, y) => Pos(x, y) }