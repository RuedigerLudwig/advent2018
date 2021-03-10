package savinien.aoc18.parser

import scala.util.matching.Regex

object Conversions:
  given Conversion[Char, Parser[Char]] with
    def apply(c: Char): Parser[Char] = Parsers.char(c)

  given Conversion[String, Parser[String]] with
    def apply(str: String): Parser[String] = Parsers.string(str)

  given Conversion[Regex, Parser[String]] with
    def apply(re: Regex): Parser[String] = Parsers.regex(re)