package savinien.aoc18
package common

import scala.annotation.targetName

import math.Integral.Implicits.infixIntegralOps
import math.Ordering.Implicits.infixOrderingOps

object point:
  case class Point[T: Integral](_1: T, _2: T) extends Product2[T, T]:
    inline def x: T = _1
    inline def y: T = _2
    override def toString: String = s"Point($x, $y)"

  object Point:
    def origin[T: Integral]: Point[T] = 
      val zero = summon[Integral[T]].zero
      Point(zero, zero)

    extension [T: Integral](p: Point[T])
      @targetName("opAddPos")
      def `+`(p2: Product2[T, T]): Point[T] = Point(p.x + p2._1, p.y + p2._2)

      @targetName("opSubPos")
      def `-`(p2: Product2[T, T]): Point[T] = Point(p.x - p2._1, p.y - p2._2)

      /**
       * The Manhatten distance of this Pos from the origin
       */
      def absM: T = p.x.abs + p.y.abs

      def min(p2: Point[T]): Point[T] = Point(p.x min p2.x, p.y min p2.y)
      def max(p2: Point[T]): Point[T] = Point(p.x max p2.x, p.y max p2.y)


  object Parsers:
    import parser.TokenParsers.*
    def point: Parser[Point[Int]] = integer.tupSep2(char(',').token) ^^ { case (x, y) => Point(x, y) }