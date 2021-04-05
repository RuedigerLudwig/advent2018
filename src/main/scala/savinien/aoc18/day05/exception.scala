package savinien.aoc18
package day05

import common.*

sealed trait PolymerException extends AdventException

case class ParseException(message: String) extends PolymerException {
  override def toString() = s"Error while Parsing Polymer: $message"
}
