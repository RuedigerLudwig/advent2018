package savinien.aoc18
package day14

import common.*

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

import scala.language.adhocExtensions
import org.scalatest.*
import flatspec.AnyFlatSpec


class ChocolateTest extends AnyFlatSpec:
  "sumToList".should("have the correct order") in {
    val input = 1230
    val expected = List(1, 2, 3, 0)
    val result = Scoreboard.sumToList(input, Nil)
    assert(result == expected)
  }

  "firstRound".should("be calculated correctly (3, 7)") in {
    val input = List(3, 7)
    val expected = List(3, 7, 1, 0)
    val result = Scoreboard(input).createNewRecipes.toList
    assert(result == expected)
  }

  it.should("also produce the correct elves") in {
    val input = List(3, 7)
    val expected = List(3, 7)
    val result = Scoreboard(input).createNewRecipes.elves
    assert(result == expected)
  }

  it.should("also produce the correct size") in {
    val input = List(3, 7)
    val expected = 4
    val result = Scoreboard(input).createNewRecipes.size
    assert(result == expected)
  }

  "secondRound".should("be calculated correctly (3, 7)") in {
    val input = List(3, 7)
    val expected = List(3, 7, 1, 0, 1, 0)
    val result = Scoreboard(input).createNewRecipes.createNewRecipes.toList
    assert(result == expected)
  }

  it.should("also produce the correct elves") in {
    val input = List(3, 7)
    val expected = List(1, 0)
    val result = Scoreboard(input).createNewRecipes.createNewRecipes.elves
    assert(result == expected)
  }

  it.should("also produce the correct size") in {
    val input = List(3, 7)
    val expected = 6
    val result = Scoreboard(input).createNewRecipes.createNewRecipes.size
    assert(result == expected)
  }

  "complete game".should("find the expected solution (9)") in {
    val input = List(3, 7)
    val expected = "5158916779"
    val result = Scoreboard(input).findPart1(9, 10)
    assert(result == expected)
  }

  it.should("find the expected solution (18)") in {
    val input = List(3, 7)
    val expected = "9251071085"
    val result = Scoreboard(input).findPart1(18, 10)
    assert(result == expected)
  }

  it.should("find the expected solution (2018)") in {
    val input = List(3, 7)
    val expected = "5941429882"
    val result = Scoreboard(input).findPart1(2018, 10)
    assert(result == expected)
  }

  "reverse game".should("find the expected solution (9)") in {
    val input = List(3, 7)
    val expected = 9
    val result = Scoreboard(input).findPart2("51589")
    assert(result == expected)
  }

  it.should("find the expected solution (18)") in {
    val input = List(3, 7)
    val expected = 18
    val result = Scoreboard(input).findPart2("92510")
    assert(result == expected)
  }

  it.should("find the expected solution (2018)") in {
    val input = List(3, 7)
    val expected = 2018
    val result = Scoreboard(input).findPart2("59414")
    assert(result == expected)
  }

object Day14Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day14Part1")(
    testM("day14 part1 works") {
      val input = AdventInputMock.GetData(
        value("9")
      )
      val result = SingleDay.part1.provideLayer(input >>> day14.live)
      assertM(result)(equalTo(AdventStringResult("5158916779")))
    },
  )

object Day14Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day14Part2")(
    testM("day14 part2 works") {
      val input = AdventInputMock.GetData(
        value("59414")
      )
      val result = SingleDay.part2.provideLayer(input >>> day14.live)
      assertM(result)(equalTo(AdventNumResult(2018)))
    },
  )
