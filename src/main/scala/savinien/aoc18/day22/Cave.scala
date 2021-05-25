package savinien.aoc18
package day22

import common.geometric.{Point, Area, Direction}
import common.ParticalHelper.*
import parsers.TokenParsers.*
import savinien.aoc18.day22.Equipment
import collection.mutable.PriorityQueue

enum Terrain:
  case Rocky, Wet, Narrow

  def singleCommonEquipment(other: Terrain): Option[Equipment] =
    (this, other) match
      case (Rocky, Rocky) | (Wet, Wet) | (Narrow, Narrow) => None
      case (Rocky, Wet) | (Wet, Rocky)                    => Some(Equipment.ClimbingGear)
      case (Rocky, Narrow) | (Narrow, Rocky)              => Some(Equipment.Torch)
      case (Wet, Narrow) | (Narrow, Wet)                  => Some(Equipment.Neither)

enum Equipment:
  case ClimbingGear, Torch, Neither

case class Step(val from: Point[Int], val direction: Option[Direction], val equipped: Equipment):
  override def equals(other: Any): Boolean =
    if !other.isInstanceOf[Step] then false
    else
      val o = other.asInstanceOf[Step]
      from.equals(o.from) && direction.equals(o.direction) && equipped.equals(o.equipped)

case class Path(val lastStep: Step, val time: Int, val visited: List[Point[Int]]):
  val equipped = lastStep.equipped
  val position = lastStep.direction.map(_ + lastStep.from).getOrElse(lastStep.from)

case class Cave(val depth: Int, target: Point[Int]):
  given Ordering[Point[Int]] = new Ordering[Point[Int]]:
    def compare(p1: Point[Int], p2: Point[Int]): Int =
      val m1 = (target - p1).absM
      val m2 = (target - p2).absM
      if m1 != m2 then m1 - m2 
      else if p1.y == p2.y then p1.x - p2.x
      else p1.y - p2.y

  given Ordering[Path] = new Ordering[Path]:
    def compare(pd1: Path, pd2: Path): Int =
      def pointCmp = summon[Ordering[Point[Int]]]
      if pd1.time != pd2.time then pd2.time - pd1.time
      else
        val position = pointCmp.compare(pd2.position, pd1.position)
        if position != 0 then position
        else if pd1.visited.length != pd2.visited.length then pd1.visited.length - pd2.visited.length
        else
          pd1.visited.zip(pd2.visited)
            .collectFirst(
              checkedOption { (p1, p2) =>
                pointCmp.compare(p1, p2) match
                  case 0 => None
                  case n => Some(n)
            }).getOrElse(0)

  private var terrainMap = Map.empty[Point[Int], Int]

  val geoIndexMod = 20183
  val horizontalMul = 16807 % geoIndexMod
  val verticalMul = 48271 % geoIndexMod
  val riskFactor = 
    getErosionLevel(target)
    terrainMap.values.map(_ % 3).sum

  def getErosionLevel(position: Point[Int]): Int = 
    if position.x < 0 || position.y < 0 then 0
    else terrainMap.get(position) match
      case Some(erosionLevel) => erosionLevel
      case None =>
        val geoIndex = 
          if position.y == 0 then Cave.safeMulModulo(position.x, horizontalMul, geoIndexMod)
          else if position.x == 0 then Cave.safeMulModulo(position.y, verticalMul, geoIndexMod)
          else 
            val erosionNorth = getErosionLevel(position + Direction.North)
            val erosionWest = getErosionLevel(position + Direction.West)
            if position == target then 0
            else Cave.safeMulModulo(erosionWest, erosionNorth, geoIndexMod)
        val erosionLevel = (geoIndex + depth) % geoIndexMod
        terrainMap = terrainMap + (position -> erosionLevel)
        erosionLevel

  def getTerrain(position: Point[Int]) = Terrain.fromOrdinal(getErosionLevel(position) % 3)

  def bestPath: Int =
    def loop(possibilities: PriorityQueue[Path], earliestExits: Map[Step, Int], closest: Int): Int = 
      val current = possibilities.dequeue
      val distance = (target - current.position).absM
      if earliestExits.get(current.lastStep).map(_ <= current.time).getOrElse(false) |
            distance > 7.max(closest * 2) then 
        loop(possibilities, earliestExits, closest)

      else
        var nextEarliest = earliestExits + (current.lastStep -> current.time)
        var nextClosest = closest.min(distance)

        if current.position == target then
          if current.equipped == Equipment.Torch then current.time
          else 
            val lastStep = Step(current.position, None, Equipment.Torch)
            val path = Path(lastStep, current.time + 7, current.visited)
            loop(possibilities.addOne(path), nextEarliest, nextClosest)
        else
          var terrain = getTerrain(current.position)
          var paths = Direction.values
            .map(dir => (current.position + dir ,dir))
            .filter((position, _) => position.x >= 0 && position.y >= 0)
            .filter((position, _) => !current.visited.contains(position))
            .collect(checkedOption((pos, dir) => 
              val (step, time) = terrain.singleCommonEquipment(getTerrain(pos)) match
                case None => 
                  (Step(current.position, Some(dir), current.equipped), current.time + 1)
                case Some(required) => 
                  (Step(current.position, Some(dir), required), current.time + (if current.equipped == required then 1 else 8))

              if nextEarliest.get(step).map(_ <= time).getOrElse(false) then None
              else Some(Path(step, time, current.position :: current.visited))
            ))
          loop(possibilities.addAll(paths), nextEarliest, nextClosest)

    val takeTorch = Step(Point.origin, None, Equipment.Torch)
    loop(PriorityQueue(Path(takeTorch, 0, List.empty)), Map.empty, target.absM)

object Cave:
  def fromData(depth: Int, target: Point[Int]): Option[Cave] =
    if depth > 0 && target.x >= 0 && target.y >= 0 && target != Point.origin[Int] then Some(new Cave(depth, target))
    else None

  def depth = configValue2("depth", unsignedInteger).token.line
  def target = configValue2("target", Point.parser[Int]).token.line
  def parser = depth ~: target ^? checkedOption(Cave.fromData)

  def safeMulModulo(factor1: Int, factor2: Int, modulo: Int): Int =
    if factor1 == 0 || factor2 == 0 then 0
    else
      val upper = Int.MaxValue / factor1
      if upper > factor2 then (factor1 * factor2) % modulo
      else if factor2 % 2 == 0 then safeMulModulo((factor1 * 2) % modulo, factor2 / 2, modulo)
      else safeMulModulo((factor1 * 2) % modulo, factor2 / 2, modulo) + 1