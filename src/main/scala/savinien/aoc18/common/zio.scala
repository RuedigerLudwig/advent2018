package savinien.aoc18
package common

import zio._
import common.{AdventException, ParseFailure}
import parser._

object ZioParser:
  def parseToZio[A](parser: Parser[A])(word: String): IO[AdventException, (A, String)] =
    Parsers.parse(parser)(word) match
      case ParseResult.Failure(error)         => IO.fail(ParseFailure(error.toString, word))
      case ParseResult.Success(value, parsed) => IO.succeed((value, word.drop(parsed)))

  def parseAllToZio[A](parser: Parser[A])(word: String): IO[AdventException, A] =
    parseToZio(parser.all)(word).map(_._1)