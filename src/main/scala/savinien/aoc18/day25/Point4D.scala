package savinien.aoc18
package day25

import scala.annotation.targetName

import math.Integral.Implicits.infixIntegralOps
import math.Ordering.Implicits.infixOrderingOps

case class Point4D[T: Integral](_1: T, _2: T, _3: T, _4: T) extends Product4[T, T, T, T]:
  override def toString: String = s"Point3D($_1, $_2, $_3, $_4)"
  /**
   * The Manhatten distance of this Pos from the origin
   */
  lazy val absM: T = _1.abs + _2.abs + _3.abs + _4.abs

  def subPoint(p2: Product4[T, T, T, T]): Point4D[T] = Point4D(_1 - p2._1, _2 - p2._2, _3 - p2._3, _4 - p2._4)

  @targetName("opSubPos")
  inline def `-`(p2: Product4[T, T, T, T]): Point4D[T] = subPoint(p2)

object Point4D:
  def origin[T: Integral]: Point4D[T] = 
    val zero = summon[Integral[T]].zero
    Point4D(zero, zero, zero, zero)

  export Parsers.parser

  object Parsers:
    import parsers.TokenParsers.*
    def parser[T: Integral]: Parser[Point4D[T]] = integral[T].token.tupSep4(char(',')) ^^ Point4D.apply