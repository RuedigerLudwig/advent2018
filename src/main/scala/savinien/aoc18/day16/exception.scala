package savinien.aoc18
package day16

import common.*

sealed trait OpCodeException extends AdventException

case object NotReducable extends OpCodeException:
  override def toString() = "The samples can't be reduced"
