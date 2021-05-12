package savinien.aoc18
package day15

import collection.mutable.PriorityQueue

import common.geometric.{Point, Area, Direction}
import Direction.*

enum Outcome:
  case Undecided(battleField: BattleField)
  case Winner(battleField: BattleField, race: Race, hitpoints: Int)

case class PathDesc(first: Point[Int], last: Point[Int], length: Int)

given Ordering[Point[Int]] = new Ordering[Point[Int]] {
  def compare(p1: Point[Int], p2: Point[Int]): Int =
    if p1.y == p2.y then p1.x - p2.x
    else p1.y - p2.y
}
given Ordering[PathDesc] = new Ordering[PathDesc] {
  def compare(pd1: PathDesc, pd2: PathDesc): Int =
    def pointCmp = summon[Ordering[Point[Int]]]
    if pd1.length != pd2.length then pd2.length - pd1.length
    else
      val last = pointCmp.compare(pd2.last, pd1.last)
      if last != 0 then last
      else pointCmp.compare(pd2.first, pd1.first)
}

class BattleField private (val field: Set[Point[Int]], elfPower: Int, val actors: Map[Int, Actor]):
  private lazy val getAwake = actors.values.filter(_.isAwake).toList
  lazy val getOccupants = getAwake.map(actor => actor.position -> actor).toMap
  def getMoveOrder = getAwake.sortBy(_.position).map(_.id)
  def getAllOf(race: Race) = getAwake.filter(_.race == race)
  def hasEnemies(actor: Actor): Boolean = getAwake.exists(_.race != actor.race)

  def getAdjacentEmpty(position: Point[Int]): Iterable[Point[Int]] =
    Direction.values
      .map(dir => position + dir)
      .filter(field.contains(_))
      .filter(!getOccupants.contains(_))

  override def toString = 
    "\n" + Area.fromIterable(field).get.grow(1).rows.map { 
      _.map { 
        point => 
          if field.contains(point) then
            getOccupants.get(point) match
              case Some(Actor(_, Race.Gnome, _, _)) => 'G'
              case Some(Actor(_, Race.Elf, _, _))   => 'E'
              case None => '.'
          else '#'
      }.mkString
    }.mkString("\n")

  def attackPower(race: Race): Int =
    race match
      case Race.Gnome => 3
      case Race.Elf   => elfPower

  def raiseAttack: BattleField = new BattleField(field, elfPower + 1, actors)

  def getAdjacentTargets(actor: Actor): Iterable[Actor] =
    Direction.values
      .map(dir => actor.position + dir)
      .map(getOccupants.get(_))
      .collect {
        case Some(enemy) if actor.race != enemy.race => enemy
      }

  def targetSpots(race: Race): Set[Point[Int]] =
    getAllOf(race).flatMap(actor => getAdjacentEmpty(actor.position)).toSet

  private def shortestPath(from: Point[Int], to: Set[Point[Int]]): Option[Point[Int]] =
    def walkPath(queue: PriorityQueue[PathDesc], visited: Set[Point[Int]]): Option[Point[Int]] =
      if queue.isEmpty then None
      else
        val path = queue.dequeue
        if to.contains(path.last) then Some(path.first)
        else if visited.contains(path.last) then walkPath(queue, visited)
        else
          val nextSteps = getAdjacentEmpty(path.last)
          queue.addAll(nextSteps.map(step => PathDesc(path.first, step, path.length + 1)))
          walkPath(queue, visited + path.last)

    if to.isEmpty then None
    else
      var firstSteps = getAdjacentEmpty(from)
      var queue = PriorityQueue.from(firstSteps.map(step => PathDesc(step, step, 1)))
      walkPath(queue, Set(from))

  private[day15] def move(id: Int): BattleField =
    val actor = actors(id)
    if !getAdjacentTargets(actor).isEmpty then this
    else shortestPath(actor.position, targetSpots(actor.race.other)) match
      case None => this
      case Some(position) => new BattleField(field, elfPower, actors.updated(actor.id, actor.setPosition(position)))

  private[day15] def attack(id: Int): BattleField =
    val actor = actors(id)
    val targets = getAdjacentTargets(actor)
    if targets.isEmpty then this
    else 
      val attackedTarget = targets.toList.sortBy(_.position).minBy(_.hitPoints)
      new BattleField(field, elfPower, actors.updated(attackedTarget.id, attackedTarget.reduceHitpoints(attackPower(actor.race))))
      
  def hitpointsOf(race: Race): Int = getAllOf(race).map(_.hitPoints).sum

  private[day15] def doOneActor(id: Int): Outcome =
    val actor = actors(id)
    if !actor.isAwake then Outcome.Undecided(this)
    else if !hasEnemies(actor) then
      Outcome.Winner(this, actor.race, hitpointsOf(actor.race))
    else
      Outcome.Undecided(move(id).attack(id))

  private[day15] def doSingleRound(order: List[Int]): Outcome =
    order match
      case Nil => Outcome.Undecided(this)
      case actor :: rest => doOneActor(actor) match
        case Outcome.Undecided(battleField) => battleField.doSingleRound(rest)
        case winner => winner

  private[day15] def doRounds(times: Int): Outcome =
    if times <= 0 then Outcome.Undecided(this)
    else doSingleRound(getMoveOrder) match
      case Outcome.Undecided(battleField) => battleField.doRounds(times - 1)
      case winner => winner

  private[day15] def nextRound(round: Int): (Int, Long) =
    doSingleRound(getMoveOrder) match
      case Outcome.Undecided(battleField) => battleField.nextRound(round + 1)
      case Outcome.Winner(battleField, race, hitpoints) => 
        val knockedOutElves = battleField.actors.values.filter(_.race == Race.Elf).filter(!_.isAwake).size
        (knockedOutElves, round.toLong * hitpoints.toLong)

  def calcWinner: Long = nextRound(0)._2

  def elvesWin: Long =
    val battleField = raiseAttack
    val (knockedOutElves, score) = battleField.nextRound(0)
    if knockedOutElves == 0 then score
    else battleField.elvesWin

object BattleField:
  def apply(list: List[List[Tile]]): BattleField =
    new BattleField(createField(list), 3, createActors(list))

  def createField(list: List[List[Tile]]): Set[Point[Int]] =
    list.zipWithIndex.map { (line, row) => 
      line.zipWithIndex.collect { 
        case (tile, col) if tile != Tile.Wall => Point(col, row)
      }
    }.flatten.toSet

  def createActors(list: List[List[Tile]]) = 
    list.zipWithIndex.map { (line, row) => 
      line.zipWithIndex.collect { 
        case (tile, col) if tile == Tile.Elf   => (Race.Elf, Point(col, row))
        case (tile, col) if tile == Tile.Gnome => (Race.Gnome, Point(col, row))
      }
    }.flatten.zipWithIndex.map { case ((race, position), id) => 
        id -> Actor(id, race, position, 200)
    }.toMap