package savinien.aoc18
package day24

import parsers.TokenParsers.*
import common.ParticalHelper.*
import scala.annotation.tailrec
import scala.compiletime.ops.int

enum Result:
  case ImmuneSystem(system: System)
  case Infection(system: System)
  case Tie

case class System(val name: String, groups: Map[Int, Group]):
  override def toString = 
    groups.values.foldLeft (s"$name:") { (s, group) => s"$s\n$group"}

  val size = groups.size
  def activeSize = groups.values.count(_.isActive)
  def units = groups.values.map(_.units).sum

  def activeGroups = groups.values.filter(_.isActive).toList

  def getGroup(idx: Int): Option[Group] = groups.get(idx)

  def targetOpponents(opponents: List[Group]): Map[Int, Int] =
    def singleSelect(groups: List[Group], opponents: List[Group], result: Map[Int, Int]): Map[Int, Int] =
      groups match
        case Nil => result
        case group :: rest => group.selectTarget(opponents) match
          case None => singleSelect(rest, opponents, result)
          case Some(target) => 
            singleSelect(rest, 
              opponents.filter(_.initiative != target.initiative), 
              result + (group.initiative -> target.initiative))
    singleSelect(activeGroups.sortBy(group => (-group.effectivePower, -group.initiative)), opponents, Map.empty)

  def attack(target: Int, attacker: Group): System =
    val targetGroup = getGroup(target).get
    val groupAfterAttack = targetGroup.dealDamage(attacker.effectivePower, attacker.damageType)
    new System(name, groups + (target -> groupAfterAttack))

  def boost(booster: Integer): System =
    val boostedGroups = groups.map((key, group) => (key, group.boostBy(booster)))
    new System(name, boostedGroups)

object System:
  def fromValues(name: String, groups: List[Group]): Option[System] =
    if groups.isEmpty then None
    else
      groups.foldLeft (Option(Map.empty[Int, Group])) {
        (groupMap, group) => groupMap.flatMap(map =>
          if map.contains(group.initiative) then None
          else Some(map + (group.initiative -> group))
        )
      }.map(new System(name, _))

  val name = oneOf(string("Immune System"), string("Infection")) <* (char(':') ~: space)
  val groups = Group.parser.lines
  val parser = name ~: groups ^?  checkedOption(fromValues)

class ReindeerHealth(val immuneSystem: System, val infection: System):
  override def toString = s"---\n$immuneSystem\n\n$infection\n"

  def units = immuneSystem.units + infection.units

  def fight: ReindeerHealth =
    val targets = immuneSystem.targetOpponents(infection.activeGroups) ++ infection.targetOpponents(immuneSystem.activeGroups)
    val (nextImmuneSystem, nextInfection) = targets.keys.toList.sortWith(_ > _).foldLeft ((immuneSystem, infection)) {
      case ((immuneSystem, infection), nextAttacker) =>
        if immuneSystem.groups.contains(nextAttacker) then
          val attacker = immuneSystem.getGroup(nextAttacker).get
          if attacker.isActive then (immuneSystem, infection.attack(targets(nextAttacker), attacker))
          else (immuneSystem, infection)
        else
          val attacker = infection.getGroup(nextAttacker).get
          if attacker.isActive then (immuneSystem.attack(targets(nextAttacker), attacker), infection)
          else (immuneSystem, infection)
    }
    new ReindeerHealth(nextImmuneSystem, nextInfection)


  @tailrec
  private def checkedBattle: Result =
    if immuneSystem.activeSize == 0 then Result.Infection(infection)
    else if infection.activeSize == 0 then Result.ImmuneSystem(immuneSystem)
    else 
      val nextReindeer = fight
      if nextReindeer.units == units then Result.Tie
      else nextReindeer.checkedBattle

  def finishBattle: System = 
    checkedBattle match
      case Result.ImmuneSystem(system) => system
      case Result.Infection(system) => system
      case Result.Tie => ???

  def boostedBattle: System = 
    def buildUpBoost(knownFailingBoost: Int, nextBooster: Int): (Int, Int, System) =
      val testReindeer = ReindeerHealth(immuneSystem.boost(nextBooster), infection)
      testReindeer.checkedBattle match
        case Result.ImmuneSystem(winningSystem) => (knownFailingBoost, nextBooster, winningSystem)
        case Result.Infection(_) => buildUpBoost(nextBooster, nextBooster * 2)
        case Result.Tie => buildUpBoost(knownFailingBoost, nextBooster + 1)

    def stepUpTied(booster: Int): (Result, Int) =
      val testReindeer = ReindeerHealth(immuneSystem.boost(booster), infection)
      testReindeer.checkedBattle match
        case Result.Tie => stepUpTied(booster + 1)
        case result => (result, booster)

    def intervalBoost(minBooster: Int, maxBooster: Int, knownWinningSystem: System): System =
      val nextBooster = (minBooster + maxBooster) / 2
      val testReindeer = ReindeerHealth(immuneSystem.boost(nextBooster), infection)
      testReindeer.checkedBattle match
        case Result.ImmuneSystem(system) =>
          if nextBooster == minBooster + 1 then system
          else intervalBoost(minBooster, nextBooster, system)
        case Result.Infection(_) =>
          if nextBooster == maxBooster - 1 then knownWinningSystem
          else intervalBoost(nextBooster, maxBooster, knownWinningSystem)
        case Result.Tie => 
          stepUpTied(minBooster + 1) match
            case (Result.ImmuneSystem(system), _) => system
            case (Result.Infection(_), newMin)    => intervalBoost(newMin, maxBooster,knownWinningSystem)
            case (Result.Tie, _)                  => ???

    val (minBoost, maxBoost, knownWinningSystem) = buildUpBoost(0, 1)
    intervalBoost(minBoost, maxBoost, knownWinningSystem)
    
object ReindeerHealth:
  def fromValues(immuneSystem: System, infection: System): Option[ReindeerHealth] =
    if !immuneSystem.groups.keySet.intersect(infection.groups.keySet).isEmpty then None
    else Some(new ReindeerHealth(immuneSystem, infection))

  val parser = (System.parser <* space) ~: System.parser ^? checkedOption(fromValues)