package savinien.aoc18
package common

import scala.annotation.targetName

import scala.collection.AbstractIterator
import scala.math.Integral
import scala.math.Integral.Implicits.infixIntegralOps
import scala.math.Ordering.Implicits.infixOrderingOps

import point.Point

object area:
  case class Area[T: Integral] private(val bottomLeft: Point[T], val topRight: Point[T]):
    override def toString: String = s"Area($bottomLeft, $topRight)"

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

    extension [T: Integral](area: Area[T])
      def width: T  = area.topRight.x - area.bottomLeft.x + summon[Integral[T]].one
      def height: T = area.topRight.y - area.bottomLeft.y + summon[Integral[T]].one
      def size: T   = width * height

      def center: Point[T] =
        val one = summon[Integral[T]].one
        val two = one + one
        val left   = (area.bottomLeft.x + area.topRight.x) / two
        val bottom = (area.bottomLeft.y + area.topRight.y) / two

        Point(left, bottom)

      def shrink(by: T): Option[Area[T]] =
        if by+by >= area.width && by+by >= area.height then None
        else Some(Area(area.bottomLeft + Point(by, by), area.topRight - Point(by, by)))

      def grow(by: T): Area[T] =
        Area(area.bottomLeft - Point(by, by), area.topRight + Point(by, by))

      def union(snd: Area[T]): Option[Area[T]] = 
        val newTop = area.topRight.y.min(snd.topRight.y)
        val newBtm = area.bottomLeft.y.max(snd.bottomLeft.y)
        if newTop < newBtm then None
        else
          val newRgt = area.topRight.x.min(snd.topRight.x)
          val newLft = area.bottomLeft.x.max(snd.bottomLeft.x)
          if newRgt < newLft then None
          else Some(new Area(Point(newLft, newBtm), Point(newRgt, newTop)))

      @targetName("opUnion")
      inline def `+`(snd: Area[T]): Option[Area[T]] = union(snd)

      def contains(p: Point[T]): Boolean =
        area.bottomLeft.x <= p.x && p.x <= area.topRight.x &&
          area.bottomLeft.y <= p.y && p.y <= area.topRight.y

      @targetName("opExpand")
      def `+`(p: Point[T]): Area[T] =
        if contains(p) then area
        else new Area(area.bottomLeft.min(p), area.topRight.max(p))


      def cells: Iterable[Point[T]] = new Iterable[Point[T]]:
        def iterator: Iterator[Point[T]] = new AbstractIterator[Point[T]]:
          private var runX = area.bottomLeft.x
          private var runY = area.bottomLeft.y
          inline def one: T  = summon[Integral[T]].one

          override def hasNext = runY <= area.topRight.y

          override def next: Point[T] =
            val result = Point(runX, runY)
            runX = runX + one
            if runX > area.topRight.x then
              runX = area.bottomLeft.x
              runY = runY + one
            result

  object Parsers:
    import parser.TokenParsers.*

    private def sizeParser: Parser[Point[Int]] = integer.tupSep2(char('x').token).token ^^ { Point(_, _) }

    def areaSizeParser: Parser[Area[Int]] = (point.Parsers.point <* char(':').token) ~: sizeParser ^? checkedOption { case (pos, size) => Area.bySize(pos, size) }