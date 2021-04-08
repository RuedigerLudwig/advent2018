package savinien.aoc18
package day05

import scala.language.adhocExtensions

import org.scalatest.*
import flatspec.AnyFlatSpec

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

import common.*
import parser.StringParsers.*
import parser.*

class PolymerTest extends AnyFlatSpec:
  "a polymer test".should("accept a single letter") in {
    val input    = "a"
    val parser   = PolyParsers.polymer
    val expected = Success(1)
    val result   = parse(parser)(input)
    assert(result == expected)
  }

  it.should("accept a matching letters") in {
    val input    = "aA"
    val parser   = PolyParsers.polymer
    val expected = Success(0)
    val result   = parse(parser)(input)
    assert(result === expected)
  }

  it.should("accept two merged pairs of matching letters") in {
    val input    = "abBA"
    val parser   = PolyParsers.polymer
    val expected = Success(0)
    val result   = parse(parser)(input)
    assert(result === expected)
  }

  it.should("accept two pairs of matching letters") in {
    val input    = "aAbB"
    val parser   = PolyParsers.polymer
    val expected = Success(0)
    val result   = parse(parser)(input)
    assert(result == expected)
  }

  it.should("accept two pairs of non matching letters") in {
    val input    = "abAB"
    val parser   = PolyParsers.polymer
    val expected = Success(4)
    val result   = parse(parser)(input)
    assert(result == expected)
  }

  it.should("accept three pairs of non matching letters") in {
    val input    = "aabAAB"
    val parser   = PolyParsers.polymer
    val expected = Success(6)
    val result   = parse(parser)(input)
    assert(result == expected)
  }

  it.should("accept work with a complex example") in {
    val input    = "dabAcCaCBAcCcaDA"
    val parser   = PolyParsers.polymer
    val expected = Success(10)
    val result   = parse(parser)(input)
    assert(result == expected)
  }

  "the filtered test".should("filter out all a's") in {
    val input    = "dabAcCaCBAcCcaDA"
    val parser   = PolyParsers.filteredPolymer('a')
    val expected = Success(6)
    val result   = parse(parser)(input)
    assert(result == expected)
  }

  it.should("filter out all b's") in {
    val input    = "dabAcCaCBAcCcaDA"
    val parser   = PolyParsers.filteredPolymer('b')
    val expected = Success(8)
    val result   = parse(parser)(input)
    assert(result == expected)
  }

  it.should("filter out all c's") in {
    val input    = "dabAcCaCBAcCcaDA"
    val parser   = PolyParsers.filteredPolymer('c')
    val expected = Success(4)
    val result   = parse(parser)(input)
    assert(result == expected)
  }

  it.should("filter out all d's") in {
    val input    = "dabAcCaCBAcCcaDA"
    val parser   = PolyParsers.filteredPolymer('d')
    val expected = Success(6)
    val result   = parse(parser)(input)
    assert(result == expected)
  }

object Day05Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day05Part1")(
    testM("day05 part1") {
      val input = AdventInputMock.GetData(
        value("dabAcCaCBAcCcaDA")
      )
      val result = SingleDay.part1.provideLayer(input >>> day05.live)
      assertM(result)(equalTo(AdventIntResult(10)))
    }
  )

object Day05Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day05Part2")(
    testM("day05 part2") {
      val input = AdventInputMock.GetData(
        value("dabAcCaCBAcCcaDA")
      )
      val result = SingleDay.part2.provideLayer(input >>> day05.live)
      assertM(result)(equalTo(AdventIntResult(4)))
    }
  )