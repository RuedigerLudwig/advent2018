package savinien.aoc18
package day19

import common.*

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

object Day19Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day19Part1")(
    testM("parse example codes") {
      for
        data       <- FileReader.getContent("input/day19/example1.txt")
        cpu        <- JumpCPUService.fromString(data)
        testResult <- assertM(ZIO.succeed(cpu.instructions.size))(equalTo(7))
      yield testResult
    },
    testM("tick") {
      for
        data       <- FileReader.getContent("input/day19/example1.txt")
        cpu        <- JumpCPUService.fromString(data)
        testResult <- assertM(ZIO.succeed(cpu.tick.get.register))(equalTo(Register(1, 5, 0, 0 , 0 ,0)))
      yield testResult
    },
    testM("run till end") {
      for
        data       <- FileReader.getContent("input/day19/example1.txt")
        cpu        <- JumpCPUService.fromString(data)
        testResult <- assertM(ZIO.succeed(cpu.runTillEnd.register))(equalTo(Register(7, 5, 6, 0 , 0 ,9)))
      yield testResult
    },
    testM("day19 part1 works") {
      val expected = 7
      for
        data       <- FileReader.getContent("input/day19/example1.txt")
        input      = AdventInputMock.GetData(value(data))
        result     = SingleDay.part1.provideLayer(input >>> day19.live)
        testResult <- assertM(result)(equalTo(AdventNumResult(expected)))
      yield testResult
    },
  )