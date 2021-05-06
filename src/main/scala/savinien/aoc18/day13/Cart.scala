package savinien.aoc18
package day13

import common.geometric.{Point, Direction, Turn}

case class Cart(val position: Point[Int], val facing: Direction, val nextTurn: Turn):
  val nextPos = position + facing
  override def toString = s"@$position, facing $facing, going to turn $nextTurn"

object Cart:
  given Ordering[Cart] = Ordering.by(cart => (cart.position.y, cart.position.x, cart.facing.ordinal, cart.nextTurn.ordinal))