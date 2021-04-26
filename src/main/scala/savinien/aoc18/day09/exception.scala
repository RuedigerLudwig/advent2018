package savinien.aoc18
package day09

import common.*

sealed trait MarbleException extends AdventException

case class DummyException(message: String) extends MarbleException:
  override def toString() = s"Dummy: $message"
