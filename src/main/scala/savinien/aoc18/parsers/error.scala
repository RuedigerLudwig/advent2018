package savinien.aoc18
package parsers

final case class ParserError(msg: String):
  def setMessage(msg: String): ParserError = ParserError(msg)