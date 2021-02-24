package savinien.aoc18
package _template

import advent._

sealed trait TemplateException extends AdventException { self =>
  override def toString() = self match
    case DummyException(s) => f"Dummy: $s"
}
case class DummyException(s: String) extends TemplateException
