package savinien.aoc18
package day22

import common.*

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

object Day22Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day22Part1")(
    testM("example1") {
      for
        data       <- FileReader.getContent("input/day22/example1.txt")
        cave       <- CaveService.fromString(data)
        testResult <- assertM(ZIO.succeed(cave.riskFactor))(equalTo(114))
      yield testResult
    },
  )

object Day22Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day22Part2")(
    testM("example1") {
      for
        data       <- FileReader.getContent("input/day22/example1.txt")
        cave       <- CaveService.fromString(data)
        testResult <- assertM(ZIO.succeed(cave.bestPath))(equalTo(45))
      yield testResult
    },
  )