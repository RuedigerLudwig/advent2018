package savinien.aoc18
package day24

import parsers.TokenParsers.*
import common.ParticalHelper.*

case class GroupDescription(
  val hitPoints: Int, 
  val damageType: String, 
  val damageStrength: Int, 
  val initiative: Int, 
  val weakness: Set[String], 
  val immunity: Set[String])

class Group(val units: Int, val boost: Int, val desciption: GroupDescription):
  def hitPoints = desciption.hitPoints
  def damageType = desciption.damageType
  def damageStrength = (desciption.damageStrength + boost)
  def initiative = desciption.initiative
  def weakness = desciption.weakness
  def immunity = desciption.immunity
  val effectivePower: Long = units.toLong * damageStrength
  val isActive = units > 0

  override def toString = s"Group $initiative contains $units units effectivepower: $effectivePower"

  def possibleDamage(attackStrength: Long, attackType: String): Long =
    if immunity.contains(attackType) then 0L
    else if weakness.contains(attackType) then attackStrength * 2
    else attackStrength

  def dealDamage(attackStrength: Long, attackType: String): Group =
    val damageTaken = possibleDamage(attackStrength, attackType)
    val unitsLost = (damageTaken / hitPoints).toInt
    val newUnits = 0.max(units - unitsLost)
    Group(newUnits, boost, desciption)

  def selectTarget(possibleTargets: List[Group]): Option[Group] =
    val targets = possibleTargets.filter(_.possibleDamage(effectivePower, damageType) > 0)
    if targets.isEmpty then None
    else Some(targets.maxBy(group => (group.possibleDamage(effectivePower, damageType), group.effectivePower, group.initiative)))

  def boostBy(booster: Int): Group =
    new Group(units, booster, desciption)
  
object Group:
  val units = (unsignedInteger <* string(" units each with")).token
  val hitPoints = (unsignedInteger <* string(" hit points")).token
  val damageStrengh = (string("with an attack that does ") *> unsignedInteger).token
  val damageType = (word <* string(" damage")).token
  val initiavtive = (string("at initiative ") *> unsignedInteger).token

  val types = word.sepMany1(char(',').token) ^^ { _.toList.toSet }
  val propertyList = sepBy(oneOf(string("weak"), string("immune")), string("to").token, types)
  val props = propertyList.sepMany1(char(';').token) ^^ { _.toList.toMap }
  val properties = (props.inParens ^^ { map => (map.get("weak").getOrElse(Set.empty), map.get("immune").getOrElse(Set.empty)) }) | 
    pure(Set.empty, Set.empty)

  val parser = units.token ~: hitPoints.token ~: properties.token ~: damageStrengh.token ~: damageType.token ~: initiavtive ^^
    { case (units, hitPoints, (weakness, immunity), damageStrength, damageType, initiative) => 
      Group(units, 0, GroupDescription(hitPoints, damageType, damageStrength, initiative, weakness, immunity))}