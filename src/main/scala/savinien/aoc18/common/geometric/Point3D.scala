package savinien.aoc18
package common
package geometric

import scala.annotation.targetName

import math.Integral.Implicits.infixIntegralOps
import math.Ordering.Implicits.infixOrderingOps

case class Point3D[T: Integral](_1: T, _2: T, _3: T) extends Product3[T, T, T]:
  inline def x: T = _1
  inline def y: T = _2
  inline def z: T = _3
  override def toString: String = s"Point3D($x, $y, $z)"
  /**
   * The Manhatten distance of this Pos from the origin
   */
  lazy val absM: T = x.abs + y.abs + z.abs

  def setX(nX: T): Point3D[T] = Point3D(nX, y, z)
  def setY(nY: T): Point3D[T] = Point3D(x, nY, z)
  def setZ(nZ: T): Point3D[T] = Point3D(x, y, nZ)

  def addPoint(p2: Product3[T, T, T]): Point3D[T] = Point3D(x + p2._1, y + p2._2, z + p2._3)

  @targetName("opAddPos")
  inline def `+`(p2: Product3[T, T, T]): Point3D[T] = addPoint(p2)

  def subPoint(p2: Product3[T, T, T]): Point3D[T] = Point3D(x - p2._1, y - p2._2, z - p2._3)

  @targetName("opSubPos")
  inline def `-`(p2: Product3[T, T, T]): Point3D[T] = subPoint(p2)

  @targetName("opNegate")
  inline def unary_- : Point3D[T] = Point3D(-x, -y, -z)

  @targetName("opTimes")
  def `*`(factor: T): Point3D[T] = Point3D(x * factor, y * factor, z * factor)

  @targetName("opDivide")
  def `/`(factor: T): Point3D[T] = Point3D(x / factor, y / factor, z / factor)

  def min(p2: Point3D[T]): Point3D[T] = Point3D(x.min(p2.x), y.min(p2.y), z.min(p2.z))
  def max(p2: Point3D[T]): Point3D[T] = Point3D(x.max(p2.x), y.max(p2.y), z.max(p2.z))

object Point3D:
  def origin[T: Integral]: Point3D[T] = 
    val zero = summon[Integral[T]].zero
    Point3D(zero, zero, zero)

  def x[T: Integral](value: T): Point3D[T] = 
    val zero = summon[Integral[T]].zero
    Point3D(value, zero, zero)

  def y[T: Integral](value: T): Point3D[T] = 
    val zero = summon[Integral[T]].zero
    Point3D(zero, value, zero)

  def z[T: Integral](value: T): Point3D[T] = 
    val zero = summon[Integral[T]].zero
    Point3D(zero, zero, value)

  export Parsers.parser

  object Parsers:
    import parsers.TokenParsers.*
    def parser[T: Integral]: Parser[Point3D[T]] = integral[T].token.tupSep3(char(',')) ^^ Point3D.apply