package savinien.aoc18
package day17

import common.*
import common.geometric.Point

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

object Day17Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day17Part1")(
    testM("parse example map") {
      for
        expected    <- FileReader.getContent("input/day17/expected1.txt")
        data        <- FileReader.getContent("input/day17/example1.txt")
        terrain     <- ReservoirService.fromString(data)
        testResult  <- assertM(ZIO.succeed(terrain.toString))(equalTo(expected))
      yield testResult
    },
    testM("reach of example") {
      for
        data        <- FileReader.getContent("input/day17/example1.txt")
        terrain     <- ReservoirService.fromString(data)
        testResult  <- assertM(ZIO.succeed(terrain.getReservoir(500).reach.size))(equalTo(57))
      yield testResult
    },
    testM("picture of example") {
      for
        expected    <- FileReader.getContent("input/day17/expected1_2.txt")
        data        <- FileReader.getContent("input/day17/example1.txt")
        terrain     <- ReservoirService.fromString(data)
        water = terrain.getReservoir(500)
        testResult  <- assertM(ZIO.succeed(terrain.printWater(water)))(equalTo(expected))
      yield testResult
    },
    testM("reach of example 2") {
      for
        data        <- FileReader.getContent("input/day17/example2.txt")
        terrain     <- ReservoirService.fromString(data)
        testResult  <- assertM(ZIO.succeed(terrain.getReservoir(500).reach.size))(equalTo(84))
      yield testResult
    },
    testM("day17 part1 works") {
      val expected = 57
      for
        data       <- FileReader.getContent("input/day17/example1.txt")
        input      = AdventInputMock.GetData(value(data))
        result     = SingleDay.part1.provideLayer(input >>> day17.live)
        testResult <- assertM(result)(equalTo(AdventNumResult(expected)))
      yield testResult
    },
  )

object Day17Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day17Part2")(
    testM("day17 part2 works") {
      assertM(ZIO.unit)(isUnit)
    },
  )
