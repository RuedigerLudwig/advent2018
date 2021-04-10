package savinien.aoc18.common

import scala.collection.*
import scala.math.Numeric
import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Ordering.Implicits.infixOrderingOps

case class Pos[T: Numeric](x: T, y: T)

case class Area[T: Numeric] private(val bottomLeft: Pos[T], val topRight: Pos[T]):
  inline def one: T             = summon[Numeric[T]].one
  def cells: Iterable[Pos[T]]   = AreaIterator(this)
  def size: T                   = (topRight.x - bottomLeft.x + one) * (topRight.y - bottomLeft.y + one)
  override def toString: String = s"Area $bottomLeft $topRight"
end Area

object Area:
  def apply[T: Numeric](corner1: Pos[T], corner2: Pos[T]): Area[T] =
    val bl = Pos(corner1.x.min(corner2.x), corner1.y.min(corner2.y))
    val tr = Pos(corner1.x.max(corner2.x), corner1.y.max(corner2.y))
    new Area(bl, tr)
end Area

class AreaIterator[T: Numeric](area: Area[T]) extends Iterable[Pos[T]]:
  def iterator: Iterator[Pos[T]] = new AbstractIterator[Pos[T]]:
    private var runX = area.bottomLeft.x
    private var runY = area.bottomLeft.y

    override def hasNext = runY <= area.topRight.y

    override def next: Pos[T] =
      if runY > area.topRight.y then
        throw IndexOutOfBoundsException("Iterator has finished")

      val result = Pos(runX, runY)
      runX = runX + area.one
      if runX > area.topRight.x then
        runX = area.bottomLeft.x
        runY = runY + area.one
      result