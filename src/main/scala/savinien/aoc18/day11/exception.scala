package savinien.aoc18
package day11

import common.*

sealed trait ChronalException extends AdventException

case object NoMaxFound extends ChronalException:
  override def toString() = "No maximum was found"
