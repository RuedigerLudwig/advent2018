package savinien.aoc18
package day08

import common.*

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

object Day08Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day08Part1")(
    testM("day08 part1 works") {
      for
        data <- FileReader.getContent("input/day08/example1.txt")
        input = AdventInputMock.GetData(value(data))
        result = SingleDay.part1.provideLayer(input >>> day08.live)
        testResult <- assertM(result)(equalTo(AdventNumResult(138)))
      yield testResult
    }
  )

object Day08Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day08Part2")(
    testM("day08 part2 works") {
      for
        data <- FileReader.getContent("input/day08/example1.txt")
        input = AdventInputMock.GetData(value(data))
        result = SingleDay.part2.provideLayer(input >>> day08.live)
        testResult <- assertM(result)(equalTo(AdventNumResult(66)))
      yield testResult
    }
  )
