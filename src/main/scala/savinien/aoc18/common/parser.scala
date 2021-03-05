package savinien.aoc18.common

import scala.util.parsing.combinator._
import zio._
import scala.annotation.targetName
import scala.util.Try

object ZioParser extends RegexParsers:
  def toInt(s: String): Parser[Int] = 
    Try(s.toInt)
      .fold(_ => failure("Not an integer"), success)

  def unsignedInteger = raw"(0|[1-9]\d*)".r      >>  toInt 
  def signedInteger   = raw"[+-]?(0|[1-9]\d*)".r >>  toInt 
  def leadingZero     = raw"(\d*)".r             >>  toInt

  def lowerCaseStrings = raw"[a-z]+".r ^^ { _.toString }

  def lead[A, B](ignore: Parser[A], result: Parser[B]) = ignore ~> result
  def trail[A, B](result: Parser[B], ignore: Parser[A]) = result <~ ignore

  def parseToZio[A](parser: Parser[A])(word: String): IO[AdventException, (A, String)] =
    parse(parser, word) match
      case Failure(msg, _)       => IO.fail(ParseFailure(msg, word))
      case Error(msg, _)         => IO.fail(ParseError(msg, word))
      case Success(value, input) => IO.succeed((value, word.drop(input.offset)))

  def parseToZIOList[A](parser: Parser[A])(word: String): IO[AdventException, (List[A], String)] =
    parseToZio(parser.*)(word)

  def parseAllToZio[A](parser: Parser[A])(word: String): IO[AdventException, A] =
    parseToZio(phrase(parser))(word).map(_._1)

  def parseAllToZioList[A](parser: Parser[A])(word: String): IO[AdventException, List[A]] =
    parseToZio(phrase(parser.*))(word).map(_._1)