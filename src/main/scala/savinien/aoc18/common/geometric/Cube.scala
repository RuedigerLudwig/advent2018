package savinien.aoc18
package common
package geometric

import scala.annotation.targetName

import scala.collection.AbstractIterator
import scala.math.Integral
import scala.math.Integral.Implicits.infixIntegralOps
import scala.math.Ordering.Implicits.infixOrderingOps
import common.ParticalHelper.checkedOption

case class Cube[T: Integral] private(val lowCorner: Point3D[T], val highCorner: Point3D[T]):
  override def toString: String = s"(${lowCorner.x},${lowCorner.y},${lowCorner.z}) - (${highCorner.x},${highCorner.y},${highCorner.z}) (${width} x ${height} x ${depth})"
  lazy val width: T  = highCorner.x - lowCorner.x + summon[Integral[T]].one
  lazy val height: T = highCorner.y - lowCorner.y + summon[Integral[T]].one
  lazy val depth: T = highCorner.z - lowCorner.z + summon[Integral[T]].one
  lazy val isPoint = 
    val one = summon[Integral[T]].one
    width == one && height == one && depth == one

  val lowAbsM: T = 
    val zero = summon[Integral[T]].zero
    val x = if width > highCorner.x && highCorner.x > summon[Integral[T]].zero then zero 
      else lowCorner.x.abs.min(highCorner.x.abs)
    val y = if height > highCorner.y && highCorner.y > summon[Integral[T]].zero then zero 
      else lowCorner.y.abs.min(highCorner.y.abs)
    val z = if depth > highCorner.z && highCorner.z > summon[Integral[T]].zero then zero 
      else lowCorner.z.abs.min(highCorner.z.abs)
    x + y + z

  def center: Point3D[T] =
    val one = summon[Integral[T]].one
    val two = one + one
    val x = (highCorner.x + lowCorner.x) / two
    val y = (highCorner.y + lowCorner.y) / two
    val z = (highCorner.z + lowCorner.z) / two

    Point3D(x, y, z)

  def shrink(by: T): Option[Cube[T]] =
    if by+by >= width && by+by >= height && by+by >= depth then None
    else Some(Cube(highCorner + Point3D(by, by, by), lowCorner - Point3D(by, by, by)))

  def grow(by: T): Cube[T] =
    Cube(highCorner - Point3D(by, by, by), lowCorner + Point3D(by, by, by))

  def grow(byX: T, byY: T, byZ: T): Cube[T] =
    Cube(highCorner - Point3D(byX, byY, byZ), lowCorner + Point3D(byX, byY, byZ))

  def merge(snd: Cube[T]): Cube[T] = 
    Cube(lowCorner.min(snd.lowCorner), highCorner.max(snd.highCorner))

  def splitLongest: Option[(Cube[T], Cube[T])] =
    val one = summon[Integral[T]].one
    val two = one + one
    if isPoint then None
    else if width >= height && width >= depth then 
      val half = width / two
      val cube1 = Cube.bySize(lowCorner, Point3D(half, height, depth)).get
      val cube2 = Cube(lowCorner.setX(cube1.highCorner.x + one), highCorner)
      Some((cube1, cube2))
    else if height >= depth then
      val half = height / two
      val cube1 = Cube.bySize(lowCorner, Point3D(width, half, depth)).get
      val cube2 = Cube(lowCorner.setY(cube1.highCorner.y + one), highCorner)
      Some((cube1, cube2))
    else
      val half = depth / two
      val cube1 = Cube.bySize(lowCorner, Point3D(width, height, half)).get
      val cube2 = Cube(lowCorner.setZ(cube1.highCorner.z + one), highCorner)
      Some((cube1, cube2))

  def contains(p: Point3D[T]): Boolean =
    lowCorner.x <= p.x && p.x <= highCorner.x &&
      lowCorner.y <= p.y && p.y <= highCorner.y &&
      lowCorner.z <= p.z && p.z <= highCorner.z

  @targetName("opExpand")
  def `+`(p: Point3D[T]): Cube[T] =
    if contains(p) then this
    else new Cube(highCorner.min(p), lowCorner.max(p))

  def moveBy(p: Point3D[T]): Cube[T] =
    Cube(lowCorner + p, highCorner + p)

  def cells: Iterable[Point3D[T]] = new Iterable[Point3D[T]]:
    def iterator: Iterator[Point3D[T]] = new AbstractIterator[Point3D[T]]:
      private var runX = highCorner.x
      private var runY = highCorner.y
      private var runZ = highCorner.z
      inline def one: T  = summon[Integral[T]].one

      override def hasNext = runZ <= lowCorner.z

      override def next: Point3D[T] =
        val result = Point3D(runX, runY, runZ)
        runX = runX + one
        if runX > lowCorner.x then
          runX = highCorner.x
          runY = runY + one
          if runY > lowCorner.y then
            runY = highCorner.y
            runZ = runZ + one
        result

object Cube:
  def apply[T: Integral](corner1: Point3D[T], corner2: Point3D[T]): Cube[T] =
    new Cube(corner1.min(corner2), corner1.max(corner2))
  def apply[T: Integral](p: Point3D[T]): Cube[T] = new Cube(p, p)

  def fromIterable[T: Integral](it: Iterable[Point3D[T]]): Option[Cube[T]] =
    if it.isEmpty then None
    else Some(it.tail.foldLeft(Cube(it.head))(_ + _))

  def bySize[T: Integral](corner: Point3D[T], size: Point3D[T]): Option[Cube[T]] =
    val zero = summon[Integral[T]].zero
    if size.x <= zero || size.y <= zero then None
    else
      val one = summon[Integral[T]].one
      Some(Cube(corner, corner - Point3D(one, one, one) + size))

  def square[T: Integral](corner: Point3D[T], size: T) = bySize(corner, Point3D(size, size, size))