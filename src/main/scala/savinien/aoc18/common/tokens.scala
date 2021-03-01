package savinien.aoc18.common

import scala.util.parsing.combinator._
import zio._

object TokenParser extends RegexParsers:
  def unsignedInteger = raw"(0|[1-9]\d*)".r      ^^ { _.toInt }
  def signedInteger   = raw"[+-]?(0|[1-9]\d*)".r ^^ { _.toInt }

  def lowerCaseStrings = raw"[a-z]+".r ^^ { _.toString }

  def lead[A, B](ignore: Parser[A], result: Parser[B]) = ignore ~> result

  def parseZIO[A](parser: Parser[A])(word: String): IO[AdventException, A] =
    parse(parser, word) match
      case Success(value, _) => IO.succeed(value)
      case Failure(msg, _) => IO.fail(ParseFailure(msg, word))
      case Error(msg, _) => IO.fail(ParseError(msg, word))

  def parseAllZIO[A](parser: Parser[A])(word: String): IO[AdventException, A] =
    parseAll(parser, word) match
      case Success(value, _) => IO.succeed(value)
      case Failure(msg, _) => IO.fail(ParseFailure(msg, word))
      case Error(msg, _) => IO.fail(ParseError(msg, word))