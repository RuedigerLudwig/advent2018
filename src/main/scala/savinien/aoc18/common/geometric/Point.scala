package savinien.aoc18
package common
package geometric

import scala.annotation.targetName

import math.Integral.Implicits.infixIntegralOps
import math.Ordering.Implicits.infixOrderingOps

case class Point[T: Integral](_1: T, _2: T) extends Product2[T, T]:
  inline def x: T = _1
  inline def y: T = _2
  override def toString: String = s"Point($x, $y)"

object Point:
  def origin[T: Integral]: Point[T] = 
    val zero = summon[Integral[T]].zero
    Point(zero, zero)

  export Parsers.parser

  extension [T: Integral](p: Point[T])
    def setX(x: T): Point[T] = Point(x, p.y)
    def setY(y: T): Point[T] = Point(p.x, y)

    def addPoint(p2: Product2[T, T]): Point[T] = Point(p.x + p2._1, p.y + p2._2)

    @targetName("opAddPos")
    inline def `+`(p2: Product2[T, T]): Point[T] = addPoint(p2)

    def subPoint(p2: Product2[T, T]): Point[T] = Point(p.x - p2._1, p.y - p2._2)

    @targetName("opSubPos")
    inline def `-`(p2: Product2[T, T]): Point[T] = subPoint(p2)

    @targetName("opTimes")
    def `*`(factor: T): Point[T] = Point(p.x * factor, p.y * factor)

    /**
     * The Manhatten distance of this Pos from the origin
     */
    def absM: T = p.x.abs + p.y.abs

    def min(p2: Point[T]): Point[T] = Point(p.x min p2.x, p.y min p2.y)
    def max(p2: Point[T]): Point[T] = Point(p.x max p2.x, p.y max p2.y)

    @targetName("opAddPointDir")
    def `+`(dir: Direction): Point[T] =
      val one = summon[Integral[T]].one
      val zero = summon[Integral[T]].zero
      dir match
        case Direction.East  => p + Point(one, zero)
        case Direction.North => p - Point(zero, one)
        case Direction.West  => p - Point(one, zero)
        case Direction.South => p + Point(zero, one)

  object Parsers:
    import parsers.TokenParsers.*
    def parser[T: Integral]: Parser[Point[T]] = integral[T].token.tupSep2(char(',')) ^^ Point.apply