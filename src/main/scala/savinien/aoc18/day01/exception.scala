package savinien.aoc18
package day01

import common._

sealed trait ChronalException extends AdventException

case class NumberFormatWrong(value: String) extends ChronalException:
  override def toString() = f"Not a valid number: $value"
