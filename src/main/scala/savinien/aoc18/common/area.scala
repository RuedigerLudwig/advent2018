package savinien.aoc18
package common

import scala.annotation.targetName

import scala.collection.*
import scala.math.Numeric
import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Ordering.Implicits.infixOrderingOps

case class Area[T: Numeric] private(val bottomLeft: Pos[T], val topRight: Pos[T]):
  lazy val size: T              = (topRight.x - bottomLeft.x) * (topRight.y - bottomLeft.y)
  override def toString: String = s"Area($bottomLeft, $topRight)"

object Area:
  def apply[T: Numeric](corner1: Pos[T], corner2: Pos[T]): Area[T] =
    val bl = Pos(corner1.x.min(corner2.x), corner1.y.min(corner2.y))
    val tr = Pos(corner1.x.max(corner2.x), corner1.y.max(corner2.y))
    new Area(bl, tr)

  inline def one[T: Numeric]: T  = summon[Numeric[T]].one

  extension [T: Numeric](area: Area[T])
    def union(snd: Area[T]): Option[Area[T]] = 
      val newTop = area.topRight.y.min(snd.topRight.y)
      val newBtm = area.bottomLeft.y.max(snd.bottomLeft.y)
      if newTop <= newBtm then None
      else
        val newRgt = area.topRight.x.min(snd.topRight.x)
        val newLft = area.bottomLeft.x.max(snd.bottomLeft.x)
        if newRgt <= newLft then None
        else Some(new Area(Pos(newLft, newBtm), Pos(newRgt, newTop)))

    def contains(p: Pos[T]): Boolean =
      area.bottomLeft.x <= p.x && p.x < area.topRight.x &&
        area.bottomLeft.y <= p.y && p.y < area.bottomLeft.y

    def cells: Iterable[Pos[T]]   = new Iterable[Pos[T]]:
      def iterator: Iterator[Pos[T]] = new AbstractIterator[Pos[T]]:
        private var runX = area.bottomLeft.x
        private var runY = area.bottomLeft.y

        override def hasNext = runY < area.topRight.y

        override def next: Pos[T] =
          val result = Pos(runX, runY)
          runX = runX + one
          if runX >= area.topRight.x then
            runX = area.bottomLeft.x
            runY = runY + one
          result

object areaParsers extends parser.TokenParsers:
  private def sizeParser: Parser[Pos[Int]] = integer.tupSep2(char('x').token).token ^^ { case (w, h) => Pos(w, h)}

  def areaSizeParser = (posParsers.posParser <* char(':').token) ~: sizeParser ^^ { case (pos, size) => Area(pos, pos + size) } 