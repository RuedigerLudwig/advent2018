package savinien.aoc18
package day18

import common.*
import common.geometric.Point

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

object Day18Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day18Part1")(
    testM("parse example map") {
      for
        data        <- FileReader.getContent("input/day18/example1.txt")
        landscape   <- WoodService.fromString(data)
        testResult  <- assertM(ZIO.succeed(landscape.toString))(equalTo(data))
      yield testResult
    },
    testM("get middle square") {
      for
        data        <- FileReader.getContent("input/day18/example1.txt")
        landscape   <- WoodService.fromString(data)
        testResult  <- assertM(ZIO.succeed(landscape.getNeighborhood(Point(4, 4))))(equalTo(Acre(4, 2)))
      yield testResult
    },
    testM("get upper square") {
      for
        data        <- FileReader.getContent("input/day18/example1.txt")
        landscape   <- WoodService.fromString(data)
        testResult  <- assertM(ZIO.succeed(landscape.getNeighborhood(Point(7, 0))))(equalTo(Acre(2, 3)))
      yield testResult
    },
    testM("get corner") {
      for
        data        <- FileReader.getContent("input/day18/example1.txt")
        landscape   <- WoodService.fromString(data)
        testResult  <- assertM(ZIO.succeed(landscape.getNeighborhood(Point(9, 9))))(equalTo(Acre(2, 0)))
      yield testResult
    },
    testM("1 tick") {
      for
        expected    <- FileReader.getContent("input/day18/expected1_1.txt")
        data        <- FileReader.getContent("input/day18/example1.txt")
        landscape   <- WoodService.fromString(data)
        testResult  <- assertM(ZIO.succeed(landscape.tick.toString))(equalTo(expected))
      yield testResult
    },
    testM("10 ticks") {
      for
        expected    <- FileReader.getContent("input/day18/expected1_10.txt")
        data        <- FileReader.getContent("input/day18/example1.txt")
        landscape   <- WoodService.fromString(data)
        testResult  <- assertM(ZIO.succeed(landscape.ticks(10).toString))(equalTo(expected))
      yield testResult
    },
    testM("10 ticks result") {
      for
        data        <- FileReader.getContent("input/day18/example1.txt")
        landscape   <- WoodService.fromString(data)
        testResult  <- assertM(ZIO.succeed(landscape.ticks(10).value))(equalTo(1147))
      yield testResult
    },
    testM("day18 part1 works") {
      val expected = 1147
      for
        data       <- FileReader.getContent("input/day18/example1.txt")
        input      = AdventInputMock.GetData(value(data))
        result     = SingleDay.part1.provideLayer(input >>> day18.live)
        testResult <- assertM(result)(equalTo(AdventNumResult(expected)))
      yield testResult
    },
  )

object Day18Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day18Part2")(
    testM("day18 part2 works") {
      val expected = 0
      for
        data       <- FileReader.getContent("input/day18/example1.txt")
        input      = AdventInputMock.GetData(value(data))
        result     = SingleDay.part2.provideLayer(input >>> day18.live)
        testResult <- assertM(result)(equalTo(AdventNumResult(expected)))
      yield testResult
    },
  )
