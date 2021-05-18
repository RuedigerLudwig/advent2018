package savinien.aoc18
package day19

import common.*

sealed trait JumpCPUException extends AdventException

case class DummyException(message: String) extends JumpCPUException:
  override def toString() = s"Dummy: $message"
