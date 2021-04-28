package savinien.aoc18
package day10

import common.*

sealed trait StarsException extends AdventException

case class DummyException(message: String) extends StarsException:
  override def toString() = s"Dummy: $message"
