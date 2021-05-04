package savinien.aoc18
package day12

import common.*

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

object Day12Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day12Part1")(
    testM("parse treeline") {
      val expected = "(0) - #..#.#..##......###...###"
      for
        data       <- FileReader.getContent("input/day12/example1.txt")
        treeLine   <- TreeLine.fromString(data)
        testResult <- assertM(ZIO.succeed(treeLine.toString))(equalTo(expected))
      yield testResult
    }
    , testM("spread once") {
      val expected = "(0) - #...#....#.....#..#..#..#"
      for
        data       <- FileReader.getContent("input/day12/example1.txt")
        treeLine   <- TreeLine.fromString(data)
        testResult <- assertM(ZIO.succeed(treeLine.spread.toString))(equalTo(expected))
      yield testResult
    }
    , testM("spread three times") {
      val expected = "(-1) - #.#...#..#.#....#..#..#...#"
      for
        data       <- FileReader.getContent("input/day12/example1.txt")
        treeLine   <- TreeLine.fromString(data)
        testResult <- assertM(ZIO.succeed(treeLine.spreadTimes(3).toString))(equalTo(expected))
      yield testResult
    }
    , testM("spread 20 times") {
      val expected = 325
      for
        data       <- FileReader.getContent("input/day12/example1.txt")
        treeLine   <- TreeLine.fromString(data)
        testResult <- assertM(ZIO.succeed(treeLine.spreadTimes(20).value))(equalTo(expected))
      yield testResult
    }
    , testM("day12 part1 works") {
      val expected = 325
      for
        data       <- FileReader.getContent("input/day12/example1.txt")
        input      = AdventInputMock.GetData(value(data))
        result     = SingleDay.part1.provideLayer(input >>> day12.live)
        testResult <- assertM(result)(equalTo(AdventNumResult(expected)))
      yield testResult
    }
  )

object Day12Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day12Part2")(
    testM("day12 part2 works") {
      assertM(ZIO.unit)(isUnit)
    }
  )
