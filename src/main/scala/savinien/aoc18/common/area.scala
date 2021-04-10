package savinien.aoc18
package common

import scala.collection.*
import scala.math.Numeric
import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Ordering.Implicits.infixOrderingOps

case class Area[T: Numeric] private(val bottomLeft: Pos[T], val topRight: Pos[T]):
  inline def one: T             = summon[Numeric[T]].one
  lazy val size: T              = (topRight.x - bottomLeft.x) * (topRight.y - bottomLeft.y)
  override def toString: String = s"Area $bottomLeft $topRight"

  def cells: Iterable[Pos[T]]   = new Iterable[Pos[T]]:
    def iterator: Iterator[Pos[T]] = new AbstractIterator[Pos[T]]:
      private var runX = bottomLeft.x
      private var runY = bottomLeft.y

      override def hasNext = runY < topRight.y

      override def next: Pos[T] =
        if runY >= topRight.y then
          throw IndexOutOfBoundsException("Iterator has finished")

        val result = Pos(runX, runY)
        runX = runX + one
        if runX >= topRight.x then
          runX = bottomLeft.x
          runY = runY + one
        result

object Area:
  def apply[T: Numeric](corner1: Pos[T], corner2: Pos[T]): Area[T] =
    val bl = Pos(corner1.x.min(corner2.x), corner1.y.min(corner2.y))
    val tr = Pos(corner1.x.max(corner2.x), corner1.y.max(corner2.y))
    new Area(bl, tr)

object areaParsers:
  import parser.TokenParsers.*
  def sizeParser: Parser[Pos[Int]] = integer.tupSep2(char('x').token).token ^^ { case (w, h) => Pos(w, h)}

  def areaWidthParser = (posParsers.posParser <* char(':').token) ~: sizeParser ^^ { case (pos, size) => Area(pos, pos + size) } 