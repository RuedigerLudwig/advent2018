package savinien.aoc18
package day15

import common.geometric.Point

enum Race:
  case Elf, Gnome

  def other: Race =
    this match
      case Elf => Gnome
      case Gnome => Elf

case class Actor(val id: Int, val race: Race, position: Point[Int], hitPoints: Int):
  def isAwake = hitPoints > 0
  def setPosition(newPosition: Point[Int]) = Actor(id, race, newPosition, hitPoints)
  def reduceHitpoints(enemyAttack: Int) = Actor(id, race, position, 0.max(hitPoints - enemyAttack))