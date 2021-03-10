package savinien.aoc18.parser

import org.scalatest.funsuite.AnyFunSuite

import Parsers._

class ParserTest extends AnyFunSuite:
  test("simple string test"):
    val input = "abc"
    val parser: Parser[String] = string("abc")
    val expected = ParseResult.Success("abc", 3)
    val result = parse(parser)(input)
    assert(result == expected)

  test("parse integer"):
    val input = "123"
    val parser: Parser[Int] = unsignedInteger
    val expected = ParseResult.Success(123, 3)
    val result = parse(parser)(input)
    assert(result == expected)

  test("parse left trimed integer"):
    val input = "   123"
    val parser: Parser[Int] = unsignedInteger.trimLeft
    val expected = ParseResult.Success(123, 6)
    val result = parse(parser)(input)
    assert(result == expected)

  test("parse lined integers"):
    val input = List("123", "456", "789").mkString("\n")
    val parser: Parser[List[Int]] = unsignedInteger.lines
    val expected = ParseResult.Success(List(123, 456, 789), 11)
    val result = parse(parser)(input)
    assert(result == expected)

  test("parse lined integers + last eol"):
    val input = List("123", "456", "789").mkString("\n") + "\n"
    val parser: Parser[List[Int]] = unsignedInteger.lines
    val expected = ParseResult.Success(List(123, 456, 789), 12)
    val result = parse(parser)(input)
    assert(result == expected)

  test("parse all lined integers"):
    val input = List("123", "456", "789").mkString("\n")
    val parser: Parser[List[Int]] = unsignedInteger.lines.all
    val expected = ParseResult.Success(List(123, 456, 789), 11)
    val result = parse(parser)(input)
    assert(result == expected)

  test("parse all lined integers + last eol"):
    val input = List("123", "456", "789").mkString("\n") + "\n"
    val parser: Parser[List[Int]] = unsignedInteger.lines.all
    val expected = ParseResult.Success(List(123, 456, 789), 12)
    val result = parse(parser)(input)
    assert(result == expected)

  test("parse all lined signed integers"):
    val input = List("+123", "-456", "789").mkString("\n")
    val parser: Parser[List[Int]] = signedInteger.lines.all
    val expected = ParseResult.Success(List(123, -456, 789), 13)
    val result = parse(parser)(input)
    assert(result == expected)

  test("parse and assign"):
    val input = "abcd"
    val parser1 = string("ab")
    val parser2 = string("cd")
    val parser = parser1 ~ parser2 ^^ { case s1 ~ s2 => s2 + s1 }
    val expected = ParseResult.Success("cdab", 4)
    val result = parse(parser)(input)
    assert(result == expected)

