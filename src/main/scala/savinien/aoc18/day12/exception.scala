package savinien.aoc18
package day12

import common.*

sealed trait SustainException extends AdventException

case class DummyException(message: String) extends SustainException:
  override def toString() = s"Dummy: $message"
