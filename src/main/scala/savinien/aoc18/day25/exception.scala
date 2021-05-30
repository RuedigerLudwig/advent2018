package savinien.aoc18
package day25

import common.*

sealed trait TemplateException extends AdventException

case class DummyException(message: String) extends TemplateException:
  override def toString() = s"Dummy: $message"
