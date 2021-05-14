package savinien.aoc18
package day17


import common.geometric.{Point, Area, Direction}
import common.geometric.CoordinateHelper.*

case class Terrain private(clay: Set[Point[Int]]):
  def size: Int = clay.size
  lazy val area: Area[Int] = Area.fromIterable(clay).get.grow(1, 0)

  override def toString: String =
    area.rows.map { 
      _.map { 
        point => 
          if clay.contains(point) then '#'
          else '.'
      }.mkString
    }.mkString("\n")

  def printWater(water: Water): String =
    area.rows.map { 
      _.map { 
        point => 
          if clay.contains(point) then '#'
          else if water.filled.contains(point) then '~'
          else if water.driedUp.contains(point) then '|'
          else '.'
      }.mkString
    }.mkString("\n")

  private def trickleDown(from: Point[Int], water: Water): (Point[Int], Boolean) =
    val next = from + Direction.Down
    if clay.contains(next) || water.filled.contains(next) then (from, true)
    else if !area.contains(next) then (from, false)
    else trickleDown(next, water)

  private def flowSideways(from: Point[Int], filled: Set[Point[Int]]): (Set[Point[Int]], List[Point[Int]]) =
    def oneDirection(point: Point[Int], dir: Direction, found: Set[Point[Int]]): (Set[Point[Int]], List[Point[Int]]) =
      val next = point + dir
      if clay.contains(next) then (found, List.empty)
      else
        val down = next + Direction.Down
        if !clay.contains(down) && !filled.contains(down) then (found, List(next))
        else oneDirection(next, dir, found + next)

    val (allLeft, hangingLeft) = oneDirection(from, Direction.Left, Set.empty)
    val (allRight, hangingRight) = oneDirection(from, Direction.Right, Set.empty)
    (allLeft ++ allRight + from, hangingLeft ::: hangingRight)

  private def fillUpward(start: Point[Int], water: Water): (Water, List[Point[Int]]) =
    var (nextLevel, hanging) = flowSideways(start, water.filled)
    if !hanging.isEmpty then (water + Water.asFlow(nextLevel), hanging)
    else
      var up = start + Direction.Up
      if !area.contains(up) then (water + Water.asFilled(nextLevel), List.empty)
      else fillUpward(up, water + Water(nextLevel, Set.empty))

  private def singleReservoir(start: Point[Int], found: Water): (Water, List[Point[Int]]) =
    val (bottom, onClay) = trickleDown(start, found)
    val water = found + Water.asFlow(start.vertPathTo(bottom.y))
    if !onClay then (water, List.empty)
    else fillUpward(bottom, water)

  def getReservoir(spring: Int): Water =
    def loop(toDo: Set[Point[Int]], found: Water): Water =
      if toDo.isEmpty then found
      else
        val current = toDo.head
        val rest = toDo.excl(current)
        if found.filled.contains(current) then loop(rest, found)
        else
          val (water, hanging) = singleReservoir(current, found)
          loop(rest ++ hanging, water)

    loop(Set(area.topLeft.setX(spring)), Water.empty)


object Terrain:
  def apply(clay: Set[Point[Int]]): Option[Terrain] =
    if clay.isEmpty then None
    else Some(new Terrain(clay))