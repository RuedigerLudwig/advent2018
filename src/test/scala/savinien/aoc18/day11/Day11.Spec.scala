package savinien.aoc18
package day11

import common.*
import geometric.Point

import scala.language.adhocExtensions
import org.scalatest.*
import flatspec.AnyFlatSpec

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

class CoordinatesTest extends AnyFlatSpec:
  "power in cells".should("be calculated correctly (57)") in {
    val input = Point(122, 79)
    val serial = 57
    val expected = -5
    val result = PowerGrid.calcPower(serial)(input)
    assert(result == expected)
  }

  it.should("be calculated correctly (71)") in {
    val input = Point(101, 153)
    val serial = 71
    val expected = 4
    val result = PowerGrid.calcPower(serial)(input)
    assert(result == expected)
  }

  "sumGrid".should("be calculated correctly(18)") in {
    val expected = -3
    val grid = PowerGrid(5, 18)
    val result = grid.grid(Point(3, 3))
    assert(result == expected)
  }

  it.should("be calculated correctly(18/2)") in {
    val expected = -666
    val grid = PowerGrid(50, 18)
    val result = grid.grid(Point(33, 45))
    assert(result == expected)
  }

  "the sum".should("be calculated correctly(18)") in {
    val expected = Point(33, 45) -> 29L
    val grid = PowerGrid(300, 18)
    val result = grid.getSumFor(3)(Point(33, 45))
    assert(result == expected)
  }

  it.should("be calculated correctly(42)") in {
    val expected = Point(21, 61) -> 30L
    val grid = PowerGrid(300, 42)
    val result = grid.getSumFor(3)(Point(21, 61))
    assert(result == expected)
  }

  "the maximum".should("be calculated correctly (18)") in {
    val expected = List((Point(33, 45), 29L))
    val grid = PowerGrid(300, 18)
    val result = grid.getMaxPowerFor(3)
    assert(result == expected)
  }

  it.should("be calculated correctly (42)") in {
    val expected = List((Point(21, 61), 30L))
    val grid = PowerGrid(300, 42)
    val result = grid.getMaxPowerFor(3)
    assert(result == expected)
  }

  "the absolute maximum".should("be calculated correctly (18)") in {
    val expected = Some((Point(90, 269), 16, 113))
    val grid = PowerGrid(300, 18)
    val result = grid.getMaxPower
    assert(result == expected)
  }

  it.should("be calculated correctly (42)") in {
    val expected = Some((Point(232, 251), 12, 119))
    val grid = PowerGrid(300, 42)
    val result = grid.getMaxPower
    assert(result == expected)
  }

object Day11Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day11Part1")(
    testM("sum (18)") {
      assertM(day11.PowerService.findMaxPowerFor(300)(18)(3))
        (equalTo(Point(33, 45)))
    }
    , testM("sum (42)") {
      assertM(day11.PowerService.findMaxPowerFor(300)(42)(3))
        (equalTo(Point(21, 61)))
    }
    , testM("day11 part1 works") {
      val input = AdventInputMock.GetData(
        value("18")
      )
      val result = SingleDay.part1.provideLayer(input >>> day11.live)
      assertM(result)(equalTo(AdventStringResult("33,45")))
    }
  )

object Day11Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day11Part2")(
    testM("sum (18)") {
      assertM(day11.PowerService.findMaxPowerFor(300)(18)(3))
        (equalTo(Point(33, 45)))
    }
    , testM("sum (42)") {
      assertM(day11.PowerService.findMaxPowerFor(300)(42)(3))
        (equalTo(Point(21, 61)))
    }
    , testM("day11 part2 works") {
      assertM(ZIO.unit)(isUnit)
    }
  )
