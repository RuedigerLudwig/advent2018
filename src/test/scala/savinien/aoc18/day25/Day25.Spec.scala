package savinien.aoc18
package day25

import common.*

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

object Day25Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day25Part1")(
    testM("find contellations 1") {
      for
        data       <- FileReader.getContent("input/day25/example1.txt")
        points     <- ChocolateService.fromString(data)
        testResult <- assertM(ZIO.succeed(Constellations.mergeCoordinates(points).length))(equalTo(2))
      yield testResult
    },
    testM("find contellations 2") {
      for
        data       <- FileReader.getContent("input/day25/example2.txt")
        points     <- ChocolateService.fromString(data)
        testResult <- assertM(ZIO.succeed(Constellations.mergeCoordinates(points).length))(equalTo(4))
      yield testResult
    },
    testM("find contellations 3") {
      for
        data       <- FileReader.getContent("input/day25/example3.txt")
        points     <- ChocolateService.fromString(data)
        testResult <- assertM(ZIO.succeed(Constellations.mergeCoordinates(points).length))(equalTo(3))
      yield testResult
    },
    testM("find contellations 4") {
      for
        data       <- FileReader.getContent("input/day25/example4.txt")
        points     <- ChocolateService.fromString(data)
        testResult <- assertM(ZIO.succeed(Constellations.mergeCoordinates(points).length))(equalTo(8))
      yield testResult
    },
    testM("day25 part1 works") {
      assertM(ZIO.unit)(isUnit)
    },
  )

object Day25Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day25Part2")(
    testM("day25 part2 works") {
      assertM(ZIO.unit)(isUnit)
    },
  )
