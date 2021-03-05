package savinien.aoc18.common

import scala.collection._

case class Pos(x: Int, y: Int)

case class Area private(val bottomLeft: Pos, val topRight: Pos):
  def cells: Iterable[Pos]      = AreaIterator(bottomLeft, topRight)
  def size: Int                 = (topRight.x - bottomLeft.x + 1) * (topRight.y - bottomLeft.y + 1)
  override def toString: String = s"Area $bottomLeft $topRight"
end Area

object Area:
  def apply(corner1: Pos, corner2: Pos): Area =
    val bl = Pos(corner1.x.min(corner2.x), corner1.y.min(corner2.y))
    val tr = Pos(corner1.x.max(corner2.x), corner1.y.max(corner2.y))
    new Area(bl, tr)
end Area

class AreaIterator(val bottomLeft: Pos, topRight:Pos) extends Iterable[Pos]:
  def iterator: Iterator[Pos] = new AbstractIterator[Pos]:
    private var runX = bottomLeft.x
    private var runY = bottomLeft.y

    override def hasNext = runY <= topRight.y

    override def next: Pos =
      if runY > topRight.y then
        throw IndexOutOfBoundsException("Iterator has finished")

      val result = Pos(runX, runY)
      runX += 1
      if runX > topRight.x then
        runX = bottomLeft.x
        runY += 1
      result