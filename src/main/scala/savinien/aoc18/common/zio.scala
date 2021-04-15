package savinien.aoc18
package common

import zio.*
import common.{AdventException, ParseFailure}
import parser.*
import StringParsers.Parser

object ZioParse:
  def parseToZio[A](parser: Parser[A])(word: String): IO[AdventException, A] =
    StringParsers.parse(parser)(word) match
      case Success(value) => IO.succeed(value)
      case Failure(error) => IO.fail(ParseFailure(error.toString, word))
      case Error(error) => IO.fail(ParseFailure(error.toString, word))
      case Fatal(error) => IO.fail(ParseFailure(error.toString, word))

  def parseAllToZio[A](parser: Parser[A])(word: String): IO[AdventException, A] =
    StringParsers.parseAll(parser)(word) match
      case Success(value) => IO.succeed(value)
      case Failure(error) => IO.fail(ParseFailure(error.toString, word))
      case Error(error) => IO.fail(ParseFailure(error.toString, word))
      case Fatal(error) => IO.fail(ParseFailure(error.toString, word))