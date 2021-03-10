package savinien.aoc18.parser

import Parser._

import scala.annotation.targetName
import scala.util.matching.Regex
import scala.util.Try

object Parsers:
  @targetName("wrapper")
  case class `~`[+A, +B](a: A, b: B) 

  def parse[A](p: => Parser[A])(word: String): ParseResult[A] =
    p.run(Input(word, 0))

  def debugParse[A](p: => Parser[A])(word: String): ParseResult[A] =
    p.scope(p.show).run(Input(word, 0))

  def char(c: Char): Parser[Char] = StringParser(c.toString).map(_.charAt(0))

  def endOfLine: Parser[Unit] = RegexParser(raw"\r?\n".r).ignore

  def endOfInput: Parser[Unit] = RegexParser(raw"\z".r).ignore

  def fail(error: => ParseError): Parser[Nothing] = Parser.Fail(error)

  def regex(re: Regex): Parser[String] = Parser.RegexParser(re)

  def signedInteger: Parser[Int] = RegexParser(raw"[+-]?(0|[1-9]\d*)".r) ^^ { 
    s => Try(s.toInt).fold(_ => Left("Can't convert $s to Int"), v => Right(v)) 
  }

  def string(str: String): Parser[String] = Parser.StringParser(str)

  def succeed[A](a: => A): Parser[A] = Parser.Succeed(a)

  def unsignedInteger: Parser[Int] = RegexParser(raw"0|[1-9]\d*".r) ^^ { 
    s => Try(s.toInt).fold(_ => Left("Can't convert $s to Int"), v => Right(v)) 
  }

  def whiteSpace: Parser[Unit] = RegexParser(raw"\s+".r).ignore