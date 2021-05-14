package savinien.aoc18
package common
package geometric

import scala.annotation.targetName

import scala.collection.AbstractIterator
import scala.math.Integral
import scala.math.Integral.Implicits.infixIntegralOps
import scala.math.Ordering.Implicits.infixOrderingOps

case class Area[T: Integral] private(val topLeft: Point[T], val bottomRight: Point[T]):
  override def toString: String = s"Area($topLeft, $bottomRight)"

object Area:
  def apply[T: Integral](corner1: Point[T], corner2: Point[T]): Area[T] =
    new Area(corner1.min(corner2), corner1.max(corner2))
  def apply[T: Integral](p: Point[T]): Area[T] = new Area(p, p)

  def fromIterable[T: Integral](it: Iterable[Point[T]]): Option[Area[T]] =
    if it.isEmpty then None
    else Some(it.tail.foldLeft(Area(it.head))(_ + _))

  def bySize[T: Integral](corner: Point[T], size: Point[T]): Option[Area[T]] =
    val zero = summon[Integral[T]].zero
    if size.x <= zero || size.y <= zero then None
    else
      val one = summon[Integral[T]].one
      Some(Area(corner, corner - Point(one, one) + size))

  def square[T: Integral](corner: Point[T], size: T) = bySize(corner, Point(size, size))

  extension [T: Integral](area: Area[T])
    def width: T  = area.bottomRight.x - area.topLeft.x + summon[Integral[T]].one
    def height: T = area.bottomRight.y - area.topLeft.y + summon[Integral[T]].one
    def size: T   = width * height

    def center: Point[T] =
      val one = summon[Integral[T]].one
      val two = one + one
      val left   = (area.topLeft.x + area.bottomRight.x) / two
      val bottom = (area.topLeft.y + area.bottomRight.y) / two

      Point(left, bottom)

    def shrink(by: T): Option[Area[T]] =
      if by+by >= area.width && by+by >= area.height then None
      else Some(Area(area.topLeft + Point(by, by), area.bottomRight - Point(by, by)))

    def grow(by: T): Area[T] =
      Area(area.topLeft - Point(by, by), area.bottomRight + Point(by, by))

    def grow(byX: T, byY: T): Area[T] =
      Area(area.topLeft - Point(byX, byY), area.bottomRight + Point(byX, byY))

    def union(snd: Area[T]): Option[Area[T]] = 
      val newTop = area.bottomRight.y.min(snd.bottomRight.y)
      val newBtm = area.topLeft.y.max(snd.topLeft.y)
      if newTop < newBtm then None
      else
        val newRgt = area.bottomRight.x.min(snd.bottomRight.x)
        val newLft = area.topLeft.x.max(snd.topLeft.x)
        if newRgt < newLft then None
        else Some(new Area(Point(newLft, newBtm), Point(newRgt, newTop)))

    @targetName("opUnion")
    inline def `+`(snd: Area[T]): Option[Area[T]] = union(snd)

    def contains(p: Point[T]): Boolean =
      area.topLeft.x <= p.x && p.x <= area.bottomRight.x &&
        area.topLeft.y <= p.y && p.y <= area.bottomRight.y

    @targetName("opExpand")
    def `+`(p: Point[T]): Area[T] =
      if contains(p) then area
      else new Area(area.topLeft.min(p), area.bottomRight.max(p))

    def cells: Iterable[Point[T]] = new Iterable[Point[T]]:
      def iterator: Iterator[Point[T]] = new AbstractIterator[Point[T]]:
        private var runX = area.topLeft.x
        private var runY = area.topLeft.y
        inline def one: T  = summon[Integral[T]].one

        override def hasNext = runY <= area.bottomRight.y

        override def next: Point[T] =
          val result = Point(runX, runY)
          runX = runX + one
          if runX > area.bottomRight.x then
            runX = area.topLeft.x
            runY = runY + one
          result

    def rows: Iterator[Iterator[Point[T]]] = new AbstractIterator[Iterator[Point[T]]]:
      inline def one: T  = summon[Integral[T]].one
      private var runY = area.topLeft.y
      
      override def hasNext = runY <= area.bottomRight.y

      override def next: Iterator[Point[T]] =
        val row = new ColIterator(runY)
        runY = runY + one 
        row

      private class ColIterator(row: T) extends AbstractIterator[Point[T]]:
        private var runX = area.topLeft.x
        override def hasNext = runX <= area.bottomRight.x

        override def next: Point[T] =
          val point = Point(runX, row)
          runX = runX + one
          point

  object Parsers:
    import parsers.TokenParsers.*

    private def sizeParser[T: Integral]: Parser[Point[T]] = integral[T].token.tupSep2(char('x')) ^^ Point.apply

    def areaSizeParser[T: Integral]: Parser[Area[T]] = (Point.parser[T] <* char(':')) ~: sizeParser ^? checkedOption(Area.bySize)