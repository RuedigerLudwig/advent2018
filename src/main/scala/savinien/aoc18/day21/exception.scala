package savinien.aoc18
package day21

import common.*

sealed trait UnderflowException extends AdventException

case class DummyException(message: String) extends UnderflowException:
  override def toString() = s"Dummy: $message"
