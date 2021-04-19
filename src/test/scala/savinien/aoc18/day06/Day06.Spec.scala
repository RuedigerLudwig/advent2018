package savinien.aoc18
package day06

import common.*
import common.point.Point
import common.area.Area

import CoordinateHelpers.*

import scala.language.adhocExtensions
import org.scalatest.*
import flatspec.AnyFlatSpec

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

class CoordinatesTest extends AnyFlatSpec:
  "infinites".should("be found correctly") in {
    val input = List(Point(1, 1), Point(1, 6), Point(8, 3), Point(3, 4), Point(5, 5), Point(8, 9))
    val expected = Option(Set(Point(1, 1), Point(1, 6), Point(8, 3), Point(8, 9)))
    val result = CoordinatesService.getInfinites(input)
    assert(result == expected)
  }

  "finite Sizes".should("be found correctly") in {
    val input = List(Point(1, 1), Point(1, 6), Point(8, 3), Point(3, 4), Point(5, 5), Point(8, 9))
    val expected = Map(Point(3, 4) -> 9, Point(5, 5) -> 17)
    val result = CoordinatesService.getFiniteSizes(input)
    assert(result == expected)
  }

  "perimeter".should("return singleton list for a single point") in {
    val area = Area(Point(3, 4))
    val expected = List(Point(3, 4))
    val result = area.perimeter.toList
    assert(result == expected)
  }

  it.should("return a line for a zero height area") in {
    val area = Area(Point(3, 3), Point(5, 3))
    val expected = List(Point(3, 3), Point(4, 3), Point(5, 3))
    val result = area.perimeter.toList
    assert(result == expected)
  }

  it.should("return a line for a zero width area") in {
    val area = Area(Point(3, 3), Point(3, 5))
    val expected = List(Point(3, 3), Point(3, 4), Point(3, 5))
    val result = area.perimeter.toList
    assert(result == expected)
  }

  it.should("return the outline of a proper area") in {
    val area = Area(Point(3, 4), Point(5, 6))
    val expected = List(Point(3, 4), Point(4, 4), Point(5, 4), Point(5, 5), Point(5, 6), Point(4, 6), Point(3, 6), Point(3, 5))
    val result = area.perimeter.toList
    assert(result == expected)
  }

  "diamond".should("return singleton list for steps = 0") in {
    val start = Point(3, 1)
    val expected = List(Point(3, 1))
    val result = start.diamond(0).toList
    assert(result == expected)
  }

  it.should("return a small diamond for steps = 1") in {
    val start = Point(3, 1)
    val expected = List(Point(3, 1), Point(2, 2), Point(1, 1), Point(2, 0))
    val result = start.diamond(1).toList
    assert(result == expected)
  }

  it.should("return a large diamond for steps = 3") in {
    val start = Point(3, 1)
    val expected = List(Point( 3,  1), Point( 2,  2), Point( 1,  3), 
                        Point( 0,  4), Point(-1,  3), Point(-2,  2), 
                        Point(-3,  1), Point(-2,  0), Point(-1, -1), 
                        Point( 0, -2), Point( 1, -1), Point( 2,  0))
    val result = start.diamond(3).toList
    assert(result == expected)
  }


end CoordinatesTest

object Day06Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day06Part1")(
    testM("get single largest area") {
      val input = List(Point(1, 1), Point(1, 6), Point(8, 3), Point(3, 4), Point(5, 5), Point(8, 9))
      val expected = 17
      val result = CoordinatesService.largestFiniteArea(input)
      assertM(result) (equalTo(expected))
    }

    , testM("day06 part1") {
      for
        data <- FileReader.getContent("input/day06/example1.txt")
        input = AdventInputMock.GetData(value(data))
        result = SingleDay.part1.provideLayer(input >>> day06.live)
        testResult <- assertM(result)(equalTo(AdventIntResult(17)))
      yield testResult
    }
  )

object Day06Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day06Part2")(
    testM("get single largest area") {
      val input = List(Point(1, 1), Point(1, 6), Point(8, 3), Point(3, 4), Point(5, 5), Point(8, 9))
      val expected = 16
      val result = CoordinatesService.getSumDistanceAreaSize(input, 32)
      assertM(result) (equalTo(expected))
    }
    , testM("day06 part2") {
      for
        data <- FileReader.getContent("input/day06/example1.txt")
        input = AdventInputMock.GetData(value(data)) && AdventInputMock.provideIntSetting("MaxDist", 32)
        result = SingleDay.part2.provideLayer(input >>> day06.live)
        testResult <- assertM(result)(equalTo(AdventIntResult(16)))
      yield testResult
    }
  )