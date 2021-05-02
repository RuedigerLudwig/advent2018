package savinien.aoc18
package day10

import common.*
import geometric.*

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

object Day10Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day10Part1")(
    testM("parse simple line") {
      assertM(day10.Star.fromString[Int]("position=< 7,  0> velocity=<-1,  0>"))
        (equalTo(Star(Point(7, 0), Point(-1, 0))))
    }
    , testM("day10 part1 works") {
      for
        data <- FileReader.getContent("input/day10/example1.txt")
        input = AdventInputMock.GetData(value(data))
        result = SingleDay.part1.provideLayer(input >>> day10.live)
        expected <- FileReader.getContent("input/day10/result1.txt")
        testResult <- assertM(result)(equalTo(AdventStringResult(expected)))
      yield testResult
    }
  )

object Day10Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day10Part2")(
    testM("day10 part2 works") {
      for
        data <- FileReader.getContent("input/day10/example1.txt")
        input = AdventInputMock.GetData(value(data))
        result = SingleDay.part2.provideLayer(input >>> day10.live)
        testResult <- assertM(result)(equalTo(AdventNumResult(3)))
      yield testResult
    }
  )
