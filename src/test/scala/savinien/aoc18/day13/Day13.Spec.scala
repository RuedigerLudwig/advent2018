package savinien.aoc18
package day13

import common.*
import common.geometric.{Point, Direction, Turn}


import collection.immutable.TreeSet

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

object Day13Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day13Part1")(
    testM("parse example map") {
      for
        data       <- FileReader.getContent("input/day13/example1.txt")
        mineMap    <- MineMap.fromString(data)
        testResult <- assertM(ZIO.succeed(mineMap.carts.size))(equalTo(2))
      yield testResult
    },
    testM("do one tick") {
      val expected = TreeSet(Cart(Point(3, 0), Direction.East, Turn.Left), Cart(Point(9, 4), Direction.East, Turn.Forward))
      for
        data       <- FileReader.getContent("input/day13/example1.txt")
        mineMap    <- MineMap.fromString(data)
        nextMap    <- ZIO.succeed(mineMap.oneTick)
        testResult <- assertM(ZIO.succeed(nextMap.carts))(equalTo(expected))
      yield testResult
    },
    testM("do four ticks") {
      val expected = TreeSet(Cart(Point(4, 2), Direction.East, Turn.Forward), Cart(Point(12, 4), Direction.North, Turn.Forward))
      for
        data       <- FileReader.getContent("input/day13/example1.txt")
        mineMap    <- MineMap.fromString(data)
        nextMap    <- ZIO.succeed(mineMap.oneTick.oneTick.oneTick.oneTick)
        testResult <- assertM(ZIO.succeed(nextMap.carts))(equalTo(expected))
      yield testResult
    },
    testM("tick to crash") {
      val expected = Point(7, 3)
      for
        data       <- FileReader.getContent("input/day13/example1.txt")
        mineMap    <- MineMap.fromString(data)
        crashSite  <- MineCartService.moveToCrash(mineMap, 15)
        testResult <- assertM(ZIO.succeed(crashSite))(equalTo(expected))
      yield testResult
    },
    testM("day13 part1 works") {
      val expected = "7,3"
      for
        data       <- FileReader.getContent("input/day13/example1.txt")
        input      = AdventInputMock.GetData(value(data))
        result     = SingleDay.part1.provideLayer(input >>> day13.live)
        testResult <- assertM(result)(equalTo(AdventStringResult(expected)))
      yield testResult
    },
  )

object Day13Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day13Part2")(
    testM("day13 part2 works") {
      val expected = "6,4"
      for
        data       <- FileReader.getContent("input/day13/example2.txt")
        input      = AdventInputMock.GetData(value(data))
        result     = SingleDay.part2.provideLayer(input >>> day13.live)
        testResult <- assertM(result)(equalTo(AdventStringResult(expected)))
      yield testResult
    },
  )
