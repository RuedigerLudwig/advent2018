package savinien.aoc18
package day07

import common.*

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

object Day07Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day07Part1")(
    testM("day07 part1 works") {
      for
        data <- FileReader.getContent("input/day07/example1.txt")
        input = AdventInputMock.GetData(value(data))
        result = SingleDay.part1.provideLayer(input >>> day07.live)
        testResult <- assertM(result)(equalTo(AdventStringResult("CABDFE")))
      yield testResult
    }
  )

object Day07Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day07Part2")(
    testM("day07 part2 works") {
      for
        data <- FileReader.getContent("input/day07/example1.txt")
        input = AdventInputMock.GetData(value(data)) && 
          AdventInputMock.provideIntSetting("Workers", 2) &&
          AdventInputMock.provideIntSetting("Delay", 0)
        result = SingleDay.part2.provideLayer(input >>> day07.live)
        testResult <- assertM(result)(equalTo(AdventNumResult(15)))
      yield testResult
    }
  )
