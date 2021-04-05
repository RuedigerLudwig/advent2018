package savinien.aoc18
package parser

import scala.language.adhocExtensions

import org.scalatest.*
import flatspec.AnyFlatSpec
import scala.util.Random
import Nel.NonEmptyList

import StringParsers.*

class ParserTest extends AnyFlatSpec:
  "a string parser".should("be matched") in:
    val input = "abc!"
    val parser = string("abc")
    val expected = Success("abc")
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("be matched at end of input") in:
    val input = "abc"
    val parser = string("abc")
    val expected = Success("abc")
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("fail for wrong capitalization") in:
    val input = "Abc!"
    val parser = string("abc")
    val result = parseAll(parser)(input)
    assert(!result.isSuccess)

  it.should("fail for end of input") in:
    val input = "ab"
    val parser = string("abc")
    val result = parseAll(parser)(input)
    assert(!result.isSuccess)

  "a word parser".should("parse a simple word correctly") in:
    val input = "Abc!"
    val parser = word
    val expected = Success("Abc")
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("be able to parse all of a string") in:
    val input = "Abc"
    val parser = word
    val expected = Success("Abc")
    val result = parseAll(parser)(input)
    assert(result == expected)

  it.should("fail when not able to parse all") in:
    val input = "Abc!"
    val parser = word
    val result = parseAll(parser)(input)
    assert(!result.isSuccess)

  "an integer".should("be extracted") in:
    val input = "345!"
    val parser = unsignedInteger
    val expected = Success(345)
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("be found when separated") in:
    val input = "50,42,2"
    val parser = sepby1(unsignedInteger)(char(','))
    val expected = Success(List(50, 42, 2))
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("be found when bracketed") in:
    val input = "[50,42,2]"
    val parser = unsignedInteger.sepby1(char(',')).bracket(char('['), char(']'))
    val expected = Success(List(50, 42, 2))
    val result = parse(parser)(input)
    assert(result == expected)

  "repeat exact".should("be found if actually more exist") in:
    val input = "ababababc"
    val raw = (string("a") ~: char('b')).mkString
    val parser = repeat(raw)(3).mkString
    val expected = Success("ababab")
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("be found in exact number") in:
    val input = "ababababc"
    val raw = (char('a') ~: char('b')).mkString
    val parser = repeat(raw)(4).mkString
    val expected = Success("abababab")
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("fail for too few items") in:
    val input = "ababababc"
    val raw = (char('a') ~: char('b')).mkString
    val parser = repeat(raw)(5).mkString
    val result = parse(parser)(input)
    assert(!result.isSuccess)

  "repeat max".should("be found if actually more exist") in:
    val input = "ababababc"
    val raw = (char('a') ~: char('b')).mkString
    val parser = repeatMax(raw)(3).mkString
    val expected = Success("ababab")
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("be found in exact number") in:
    val input = "ababababc"
    val raw = (char('a') ~: char('b')).mkString
    val parser = repeatMax(raw)(4).mkString
    val expected = Success("abababab")
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("cope with more items") in:
    val input = "ababababc"
    val raw = (char('a') ~: char('b')).mkString
    val parser = repeatMax(raw)(5).mkString
    val expected = Success("abababab")
    val result = parse(parser)(input)
    assert(result == expected)

  "repeat beween".should("find any number in between") in:
    val input = "abababab!"
    val raw = (char('a') ~: char('b')).mkString
    val parser = repeatBetween(raw)(3)(5).mkString
    val expected = Success("abababab")
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("accept too many patterns") in:
    val input = "abababababababab!"
    val raw = (char('a') ~: char('b')).mkString
    val parser = repeatBetween(raw)(3)(5).mkString
    val expected = Success("ababababab")
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("fail on too few tests") in:
    val input = "abab!"
    val raw = (char('a') ~: char('b')).mkString
    val parser = repeatBetween(raw)(3)(5).mkString
    val result = parse(parser)(input)
    assert(!result.isSuccess)
  
  "products".should("be created as expected") in:
    val input = "1aY!"
    val parser = digit ~: lower
    val expected = Success(('1', 'a'))
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("also works with chaining") in:
    val input = "1aY!"
    val parser = digit ~: lower ~: upper
    val expected = Success(('1', 'a', 'Y'))
    val result = parse(parser)(input)
    assert(result == expected)

  "map assign".should("work as expected") in:
    val input = "1aY!"
    val parser = digit ~: lower ^^ { (d, l) => s"$l$d" }
    val expected = Success("a1")
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("also work when chained") in:
    val input = "1aY!"
    val parser = digit ~: lower ~: upper ^^ { (d, l, u) => s"$u$d$l" }
    val expected = Success("Y1a")
    val result = parse(parser)(input)
    assert(result == expected)

  "partial map assign".should("work as expected") in:
    val input = "112"
    val parser = many(digit ^? { case c if c == '1' => c - '0'  })
    val expected = Success(List(1, 1))
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("fail when partial failes") in:
    val input = "112"
    val parser = digit ^? { case c if c == '2' => c - '0'  }
    val result = parse(parser)(input)
    assert(!result.isSuccess)

  "lines".should("correctly split the input into lines") in:
    val input = List("1", "2").mkString("\n")
    val parser = lines(digit)
    val expected = Success(List('1', '2'))
    val result = parseAll(parser)(input)
    assert(result == expected)

  it.should("work with input that end on a newline") in:
    val input = List("1", "2").mkString("\n") + "\n"
    val parser = lines(digit)
    val expected = Success(List('1', '2'))
    val result = parseAll(parser)(input)
    assert(result == expected)

  "lazy opt".should("correctly ignore leading optional input") in:
    val input = "ab"
    val parser = char('a') ~?: string("ab")
    val expected = Success((None, "ab"))
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("use that input as needed though") in:
    val input = "aab"
    val parser = char('a') ~?: string("ab")
    val expected = Success((Some('a'), "ab"))
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("should fail if even the optional part doesn't help") in:
    val input = "aaab"
    val parser = char('a') ~?: string("ab")
    val result = parse(parser)(input)
    assert(!result.isSuccess)

  it.should("fail on a premature end") in:
    val input = "a"
    val parser = char('a') ~?: char('A')
    val result = parse(parser)(input)
    assert(!result.isSuccess)

  "lazy many".should("return zero if there was nothing to find lazily") in:
    val input = "ab"
    val parser = char('a') ~*: string("ab")
    val expected = Success((Nil, "ab"))
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("find a list if there is one") in:
    val input = "aab"
    val parser = char('a') ~*: string("ab")
    val expected = Success((List('a'), "ab"))
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("find a list if there is more than one item one") in:
    val input = "aaab"
    val parser = char('a') ~*: string("ab")
    val expected = Success((List('a', 'a'), "ab"))
    val result = parse(parser)(input)
    assert(result == expected)

  "lazy many1".should("fail if there isn't at least one matche") in:
    val input = "ab"
    val parser = char('a') ~+: string("ab")
    val result = parse(parser)(input)
    assert(!result.isSuccess)

  it.should("succeed if there is one match") in:
    val input = "aab"
    val parser = char('a') ~+: string("ab")
    val expected = Success((List('a'), "ab"))
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("succeed if there is more than one match") in:
    val input = "aaab"
    val parser = char('a') ~+: string("ab")
    val expected = Success((List('a', 'a'), "ab"))
    val result = parse(parser)(input)
    assert(result == expected)

  "oneOf".should("succeed on first choice") in:
    val input = "12abY"
    val parser = oneOf(NonEmptyList(merge(digit), merge(lower), word))
    val expected = Success("12")
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("succeed on second choice") in:
    val input = "12abY"
    val parser = oneOf(NonEmptyList(merge(lower), merge(digit), word))
    val expected = Success("12")
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("succeed on third choice") in:
    val input = "12abY"
    val parser = oneOf(NonEmptyList(merge(lower), word, merge(digit)))
    val expected = Success("12")
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("fail if no correct parser") in:
    val input = "12abY"
    val parser = oneOf(NonEmptyList(merge(lower), word))
    val result = parse(parser)(input)
    assert(!result.isSuccess)

  "commit".should("make an or fail, even though the second part would be possible") in:
    val input = "12"
    val parser = (digit ~: commit(lower)) | (digit ~: digit)
    val result = parse(parser)(input)
    assert(!result.isSuccess)

  it.should("have succeeeded without the commit") in:
    val input = "12"
    val parser = (digit ~: lower) | (digit ~: digit)
    val expected = Success(('1', '2'))
    val result = parse(parser)(input)
    assert(result == expected)

  it.should("should work if we do not reach the commited part") in:
    val input = "12"
    val parser = (lower ~: commit(lower)) | (digit ~: digit)
    val expected = Success(('1', '2'))
    val result = parse(parser)(input)
    assert(result == expected)

  "recursive patterns".should("work fine") in:
    val input = "aA"
    lazy val matchLU: Parser[Any] = lower ~: (parser ~?: upper)
    lazy val matchUL: Parser[Any] = upper ~: (parser ~?: lower)
    lazy val parser : Parser[Any] = matchLU | matchUL
    val expected = Success(('a', None, 'A'))
    val result = parse(parser)(input)
    assert(result == expected)

  "trampolines".should("never produce a stack overflow") in:
    val list = (1 to 100_000).map(_ => "0123456789"(Random.nextInt(10)))
    val input = list.mkString + '!'
    val parser = digit.* ~: char('!')
    val expected = Success((list, '!'))
    val result = parse(parser)(input)
    assert(result == expected)
end ParserTest