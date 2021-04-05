package savinien.aoc18
package parser

final case class ParserError(msg: String):
  def setMessage(msg: String): ParserError = ParserError(msg)