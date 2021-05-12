package savinien.aoc18
package day15

import common.*

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

object Day15Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day15Part1")(
    testM("parse example map") {
      for
        data        <- FileReader.getContent("input/day15/example1.txt")
        battlefield <- BanditService.fromString(data)
        testResult  <- assertM(ZIO.succeed(battlefield.hitpointsOf(Race.Elf)))(equalTo(400))
      yield testResult
    },
    testM("after 1 round") {
      for
        expected_data <- FileReader.getContent("input/day15/expected1_1.txt")
        expected    <- BanditService.fromString(expected_data)
        data        <- FileReader.getContent("input/day15/example1.txt")
        battlefield <- BanditService.fromString(data)
        result      <- battlefield.doRounds(1) match
                          case Outcome.Undecided(bf) => ZIO.succeed(bf)
                          case _ => ZIO.fail(UnexpectedWinner)
        testResult  <- assertM(ZIO.succeed(result.toString))(equalTo(expected.toString))
      yield testResult
    },
    testM("after 1 round (Elf)") {
      for
        data        <- FileReader.getContent("input/day15/example1.txt")
        battlefield <- BanditService.fromString(data)
        result      <- battlefield.doRounds(1) match
                          case Outcome.Undecided(bf) => ZIO.succeed(bf)
                          case _ => ZIO.fail(UnexpectedWinner)
        testResult  <- assertM(ZIO.succeed(result.hitpointsOf(Race.Elf)))(equalTo(394))
      yield testResult
    },
    testM("after 1 round (Gnome)") {
      for
        data        <- FileReader.getContent("input/day15/example1.txt")
        battlefield <- BanditService.fromString(data)
        result      <- battlefield.doRounds(1) match
                          case Outcome.Undecided(bf) => ZIO.succeed(bf)
                          case _ => ZIO.fail(UnexpectedWinner)
        testResult  <- assertM(ZIO.succeed(result.hitpointsOf(Race.Gnome)))(equalTo(794))
      yield testResult
    },
    testM("after 23 rounds") {
      for
        expected_data <- FileReader.getContent("input/day15/expected1_23.txt")
        expected    <- BanditService.fromString(expected_data)
        data        <- FileReader.getContent("input/day15/example1.txt")
        battlefield <- BanditService.fromString(data)
        result      <- battlefield.doRounds(23) match
                          case Outcome.Undecided(bf) => ZIO.succeed(bf)
                          case _ => ZIO.fail(UnexpectedWinner)
        testResult  <- assertM(ZIO.succeed(result.toString))(equalTo(expected.toString))
      yield testResult
    },
    testM("after 23 rounds (Elf)") {
      for
        data        <- FileReader.getContent("input/day15/example1.txt")
        battlefield <- BanditService.fromString(data)
        result      <- battlefield.doRounds(23) match
                          case Outcome.Undecided(bf) => ZIO.succeed(bf)
                          case _ => ZIO.fail(UnexpectedWinner)
        testResult  <- assertM(ZIO.succeed(result.hitpointsOf(Race.Elf)))(equalTo(131))
      yield testResult
    },
    testM("after 23 rounds (Gnome)") {
      for
        data        <- FileReader.getContent("input/day15/example1.txt")
        battlefield <- BanditService.fromString(data)
        result      <- battlefield.doRounds(23) match
                          case Outcome.Undecided(bf) => ZIO.succeed(bf)
                          case _ => ZIO.fail(UnexpectedWinner)
        testResult  <- assertM(ZIO.succeed(result.hitpointsOf(Race.Gnome)))(equalTo(662))
      yield testResult
    },
    testM("after 47 rounds") {
      for
        expected_data <- FileReader.getContent("input/day15/expected1_47.txt")
        expected    <- BanditService.fromString(expected_data)
        data        <- FileReader.getContent("input/day15/example1.txt")
        battlefield <- BanditService.fromString(data)
        result      <- battlefield.doRounds(47) match
                          case Outcome.Undecided(bf) => ZIO.succeed(bf)
                          case _ => ZIO.fail(UnexpectedWinner)
        testResult  <- assertM(ZIO.succeed(result.toString))(equalTo(expected.toString))
      yield testResult
    },
    testM("after 47 rounds (Elf)") {
      for
        data        <- FileReader.getContent("input/day15/example1.txt")
        battlefield <- BanditService.fromString(data)
        result      <- battlefield.doRounds(47) match
                          case Outcome.Undecided(bf) => ZIO.succeed(bf)
                          case _ => ZIO.fail(UnexpectedWinner)
        testResult  <- assertM(ZIO.succeed(result.hitpointsOf(Race.Elf)))(equalTo(0))
      yield testResult
    },
    testM("after 47 rounds (Gnome)") {
      for
        data        <- FileReader.getContent("input/day15/example1.txt")
        battlefield <- BanditService.fromString(data)
        result      <- battlefield.doRounds(47) match
                          case Outcome.Undecided(bf) => ZIO.succeed(bf)
                          case _ => ZIO.fail(UnexpectedWinner)
        testResult  <- assertM(ZIO.succeed(result.hitpointsOf(Race.Gnome)))(equalTo(590))
      yield testResult
    },
    testM("example 1 final score") {
      for
        data        <- FileReader.getContent("input/day15/example1.txt")
        battlefield <- BanditService.fromString(data)
        result      <- ZIO.succeed(battlefield.calcWinner)
        testResult  <- assertM(ZIO.succeed(result))(equalTo(27730))
      yield testResult
    },
    testM("example 2 final score") {
      for
        data        <- FileReader.getContent("input/day15/example2.txt")
        battlefield <- BanditService.fromString(data)
        result      <- ZIO.succeed(battlefield.calcWinner)
        testResult  <- assertM(ZIO.succeed(result))(equalTo(36334))
      yield testResult
    },
    testM("example 3 final score") {
      for
        data        <- FileReader.getContent("input/day15/example3.txt")
        battlefield <- BanditService.fromString(data)
        result      <- ZIO.succeed(battlefield.calcWinner)
        testResult  <- assertM(ZIO.succeed(result))(equalTo(39514))
      yield testResult
    },
    testM("example 4 final score") {
      for
        data        <- FileReader.getContent("input/day15/example4.txt")
        battlefield <- BanditService.fromString(data)
        result      <- ZIO.succeed(battlefield.calcWinner)
        testResult  <- assertM(ZIO.succeed(result))(equalTo(27755))
      yield testResult
    },
    testM("example 5 final score") {
      for
        data        <- FileReader.getContent("input/day15/example5.txt")
        battlefield <- BanditService.fromString(data)
        result      <- ZIO.succeed(battlefield.calcWinner)
        testResult  <- assertM(ZIO.succeed(result))(equalTo(28944))
      yield testResult
    },
    testM("example 6 final score") {
      for
        data        <- FileReader.getContent("input/day15/example6.txt")
        battlefield <- BanditService.fromString(data)
        result      <- ZIO.succeed(battlefield.calcWinner)
        testResult  <- assertM(ZIO.succeed(result))(equalTo(18740))
      yield testResult
    },
    testM("day15 part1 works") {
      val expected = 27730
      for
        data       <- FileReader.getContent("input/day15/example1.txt")
        input      = AdventInputMock.GetData(value(data))
        result     = SingleDay.part1.provideLayer(input >>> day15.live)
        testResult <- assertM(result)(equalTo(AdventNumResult(expected)))
      yield testResult
    },
  )

object Day15Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day15Part2")(
    testM("example 1 final score") {
      for
        data        <- FileReader.getContent("input/day15/example1.txt")
        battlefield <- BanditService.fromString(data)
        result      <- ZIO.succeed(battlefield.elvesWin)
        testResult  <- assertM(ZIO.succeed(result))(equalTo(4988))
      yield testResult
    },
    testM("example 3 final score") {
      for
        data        <- FileReader.getContent("input/day15/example3.txt")
        battlefield <- BanditService.fromString(data)
        result      <- ZIO.succeed(battlefield.elvesWin)
        testResult  <- assertM(ZIO.succeed(result))(equalTo(31284))
      yield testResult
    },
    testM("example 4 final score") {
      for
        data        <- FileReader.getContent("input/day15/example4.txt")
        battlefield <- BanditService.fromString(data)
        result      <- ZIO.succeed(battlefield.elvesWin)
        testResult  <- assertM(ZIO.succeed(result))(equalTo(3478))
      yield testResult
    },
    testM("example 5 final score") {
      for
        data        <- FileReader.getContent("input/day15/example5.txt")
        battlefield <- BanditService.fromString(data)
        result      <- ZIO.succeed(battlefield.elvesWin)
        testResult  <- assertM(ZIO.succeed(result))(equalTo(6474))
      yield testResult
    },
    testM("example 6 final score") {
      for
        data        <- FileReader.getContent("input/day15/example6.txt")
        battlefield <- BanditService.fromString(data)
        result      <- ZIO.succeed(battlefield.elvesWin)
        testResult  <- assertM(ZIO.succeed(result))(equalTo(1140))
      yield testResult
    },
    testM("day15 part2 works") {
      val expected = 4988
      for
        data       <- FileReader.getContent("input/day15/example1.txt")
        input      = AdventInputMock.GetData(value(data))
        result     = SingleDay.part2.provideLayer(input >>> day15.live)
        testResult <- assertM(result)(equalTo(AdventNumResult(expected)))
      yield testResult
    },
  )
