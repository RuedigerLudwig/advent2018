package savinien.aoc18
package day14

import common.*

sealed trait ChocolateException extends AdventException

case class DummyException(message: String) extends ChocolateException:
  override def toString() = s"Dummy: $message"
