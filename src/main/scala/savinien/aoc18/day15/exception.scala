package savinien.aoc18
package day15

import common.*

sealed trait BanditException extends AdventException

case object UnexpectedWinner extends BanditException:
  override def toString() = "Did not expected to have a winner yet"
