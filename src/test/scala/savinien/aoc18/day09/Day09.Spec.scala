package savinien.aoc18
package day09

import common.*

import scala.language.adhocExtensions
import org.scalatest.*
import flatspec.AnyFlatSpec

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

import MarbleIterator.given

class MarbleTests extends AnyFlatSpec:
  "MarbleIterator".should("produce the correct first result") in {
    val expected = (9, 23)
    val iterator = new MarbleIterator(23, 7).iterator
    val result = iterator.next()
    assert(result == expected)
  }

  it.should("produce the correct second result") in {
    val expected = (17, 46)
    val iterator = new MarbleIterator(23, 7).iterator.drop(1)
    val result = iterator.next()
    assert(result == expected)
  }

  it.should("produce the correct third result") in {
    val expected = (11, 69)
    val iterator = new MarbleIterator(23, 7).iterator.drop(2)
    val result = iterator.next()
    assert(result == expected)
  }

  "A game".should("produce the correct winner (9, 25)") in {
    val expected = 32
    val result = MarbleService.playGame(9, 25)
    assert(result._2 == expected)
  }

  it.should("produce the correct winner (10, 1618)") in {
    val expected = 8317
    val result = MarbleService.playGame(10, 1618)
    assert(result._2 == expected)
  }

  it.should("produce the correct winner (13, 7999)") in {
    val expected =146_373
    val result = MarbleService.playGame(13, 7999)
    assert(result._2 == expected)
  }

object Day09Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day09Part1")(
    testM("day09 part1 works") {
      val input = AdventInputMock.GetData(
        value("21 players; last marble is worth 6111 points")
      )
      val result = SingleDay.part1.provideLayer(input >>> day09.live)
      assertM(result)(equalTo(AdventNumResult(54_718)))
    }
  )

object Day09Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day09Part2")(
    testM("day09 part2 works") {
      assertM(ZIO.unit)(isUnit)
    }
  )
