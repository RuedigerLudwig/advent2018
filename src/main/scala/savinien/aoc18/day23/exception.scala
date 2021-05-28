package savinien.aoc18
package day23

import common.*

sealed trait TeleportException extends AdventException

case class DummyException(message: String) extends TeleportException:
  override def toString() = s"Dummy: $message"
