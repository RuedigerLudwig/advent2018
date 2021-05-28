package savinien.aoc18
package day23

import parsers.TokenParsers.*
import common.geometric.{Point3D, Cube}
import common.ParticalHelper.*
import scala.annotation.targetName
import scala.collection.AbstractIterator

case class Nanobot(val pos: Point3D[Long], val radius: Long):
  override def toString = s"pos=$pos, r=$radius"

  def isInRange(point: Point3D[Long]): Boolean =
    (pos - point).absM <= radius

  def isInRange(cube: Cube[Long]): Boolean = cube.moveBy(-pos).lowAbsM <= radius

  val box: Cube[Long] =
    val diag = Point3D(radius, radius, radius)
    Cube(pos - diag, pos + diag)

object Nanobot:
  def fromValues(pos: Point3D[Long], radius: Long): Option[Nanobot] =
    if radius < 1 then None
    else Some(new Nanobot(pos, radius))
  val pos = configValue("pos", Point3D.parser[Long].inAngles)
  val radius = configValue("r", unsignedLong)
  val parser = (pos <* char(',').token) ~: radius ^? checkedOption(fromValues)

class Swarm(val bots: List[Nanobot]):
  def strongest: Nanobot =
    bots.maxBy(_.radius)

  def inRange(bot: Nanobot): Int =
    bots.count(other => bot.isInRange(other.pos))

  def inRangeOfStrongest: Int =
    inRange(strongest)

  def botsInRange(pos: Point3D[Long]): Int =
    bots.count(_.isInRange(pos))

  def botsInRange(cube: Cube[Long]): Int =
    bots.count(_.isInRange(cube))

  def box: Cube[Long] =
    bots.tail.foldLeft (bots.head.box) { (box, bot) => box.merge(bot.box) }

  def teleportPoint: Point3D[Long] =
    def loop(currentBox: Cube[Long], bestInRange: Int, bestPoint: Point3D[Long], stashed: Map[Int, List[Cube[Long]]]): Point3D[Long] =
      currentBox.splitLongest match
        case None =>
          val currentPoint = currentBox.lowCorner
          val currentInRange = botsInRange(currentPoint)
          val (nextInRange, nextPoint) = 
            if currentInRange > bestInRange then (currentInRange, currentPoint)
            else if currentInRange == bestInRange && bestPoint.absM > currentPoint.absM then (bestInRange, currentPoint)
            else (bestInRange, bestPoint)

          val firstClearedStashed = 
            if nextInRange != bestInRange then stashed.filter((inRange, _) => inRange >= nextInRange)
            else stashed
          val clearedStashed = 
            if bestPoint == nextPoint then firstClearedStashed
            else
              val list = firstClearedStashed.get(nextInRange) match
                case None => List.empty
                case Some(list) => list.filter(_.lowAbsM <= bestPoint.absM)
              if list.isEmpty then firstClearedStashed.removed(nextInRange) 
              else firstClearedStashed.updated(nextInRange, list)

          if clearedStashed.isEmpty then nextPoint
          else
            val max = clearedStashed.keys.max
            val nextList = clearedStashed(max)
            val nextStashed = if nextList.tail.isEmpty then clearedStashed.removed(max)
            else clearedStashed.updated(max, nextList.tail)

            loop(nextList.head, nextInRange, nextPoint, nextStashed)
      
        case Some(cube1, cube2) =>
          val inRange1 = botsInRange(cube1)
          val inRange2 = botsInRange(cube2)

          if inRange1 < bestInRange && inRange2 < bestInRange then
            if stashed.isEmpty then bestPoint
            else
              val max = stashed.keys.max
              val nextList = stashed(max)
              val nextStashed = 
                if nextList.tail.isEmpty then stashed.removed(max)
                else stashed.updated(max, nextList.tail)
              loop(nextList.head, bestInRange, bestPoint, nextStashed)

          else if inRange1 >= inRange2 then 
            val nextStashed = 
              if inRange2 > bestInRange || (inRange2 == bestInRange && cube2.lowAbsM < bestPoint.absM) then
                stashed.updated(inRange2, stashed.get(inRange2).map(cube2 :: _).getOrElse(List(cube2)))
              else stashed
            loop(cube1, bestInRange, bestPoint, nextStashed)
          else 
            val nextStashed = 
              if inRange1 > bestInRange || (inRange1 == bestInRange && cube1.lowAbsM < bestPoint.absM) then
                stashed.updated(inRange1, stashed.get(inRange1).map(cube1 :: _).getOrElse(List(cube1)))
              else stashed
            loop(cube2, bestInRange, bestPoint, nextStashed)
        
    loop(box, botsInRange(Point3D.origin[Long]), Point3D.origin, Map.empty)

object Swarm:
  def fromValues(bots: List[Nanobot]): Option[Swarm] =
    if bots.isEmpty then None
    else Some(new Swarm(bots))

  def parser = Nanobot.parser.token.lines ^? checkedOption(fromValues)