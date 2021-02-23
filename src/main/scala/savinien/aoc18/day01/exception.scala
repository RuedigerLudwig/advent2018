package savinien.aoc18
package day01

import advent._

sealed trait ChronalException extends AdventException { self =>
  override def toString() = self match
    case MultiError(l) => l.length match
      case 0 => "Empty Error list"
      case 1 => l(0).toString()
      case _ => f"MultiError: $l"
    case NumberFormatWrong(s) => f"NumberFormatWrong: $s"
}
case class MultiError(l: List[ChronalException]) extends ChronalException
case class NumberFormatWrong(s: String) extends ChronalException
