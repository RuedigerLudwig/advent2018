package savinien.aoc18
package day13

import common.*

sealed trait MineException extends AdventException

case object NoCrashHappened extends MineException:
  override def toString() = "No crash Happened"

case object ALlCartsCrashed extends MineException:
  override def toString() = "No carts left after crashes"

case class TooManyCarts(num: Int) extends MineException:
  override def toString() = s"Stil too many carts: $num"
