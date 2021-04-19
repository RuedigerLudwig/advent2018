package savinien.aoc18
package day07

import common.*

sealed trait PartsException extends AdventException

case object NoLowestFound extends PartsException:
  override def toString() = "No lowest char found"

case object PrematureFinish extends PartsException:
  override def toString() = "Finished too early"