package savinien.aoc18
package _template

import common._

sealed trait TemplateException extends AdventException

case class DummyException(message: String) extends TemplateException:
  override def toString() = f"Dummy: $message"
