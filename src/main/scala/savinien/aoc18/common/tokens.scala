package savinien.aoc18.common

import scala.util.parsing.combinator._

object TokenParser extends RegexParsers:
  def unsignedInteger = raw"(0|[1-9]\d*)".r      ^^ { _.toInt }
  def signedInteger   = raw"[+-]?(0|[1-9]\d*)".r ^^ { _.toInt }

  def lowerCaseStrings = raw"[a-z]+".r ^^ { _.toString }