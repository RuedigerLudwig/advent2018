package savinien.aoc18
package day16

import common.*

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

object Day16Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day16Part1")(
    testM("parse example map") {
      for
        data        <- FileReader.getContent("input/day16/example1.txt")
        sample      <- OpCodeService.readSample(data)
        testResult  <- assertM(ZIO.succeed(sample(0).countPossibleOpCodes(Operation.allOperations)))(equalTo(3))
      yield testResult
    },
  )