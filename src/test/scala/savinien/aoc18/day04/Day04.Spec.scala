package savinien.aoc18
package day04

import common.*
import Types.GuardNum

import java.time.LocalDateTime
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*

object Day04Part1Spec extends DefaultRunnableSpec:
  private def date(year: Int, month: Int, day: Int, hour: Int, minute: Int): LocalDateTime =
    LocalDateTime.of(year, month, day, hour, minute).nn

  def spec = suite("Day04Part1")(
    testM("parse shift start") {
      assertM(day04.GuardEntry.fromString("[1518-11-01 00:00] Guard #10 begins shift"))
        (equalTo(GuardEntry.ShiftStarts(date(1518,11,1,0,0), GuardNum(10))))
    }
    , testM("parse falls asleep") {
      assertM(day04.GuardEntry.fromString("[1518-11-01 00:05] falls asleep"))
        (equalTo(GuardEntry.FallsAsleep(date(1518,11,1,0,5))))
    }
    , testM("parse wakes up") {
      assertM(day04.GuardEntry.fromString("[1518-11-01 00:25] wakes up"))
        (equalTo(GuardEntry.WakesUp(date(1518,11,1,0,25))))
    }
    , testM("sort entries") {
      val entry1 =GuardEntry.ShiftStarts(date(1518,11,1,0,0), GuardNum(10))
      val entry2 =GuardEntry.FallsAsleep(date(1518,11,1,0,5))
      val entry3 =GuardEntry.WakesUp(date(1518,11,1,0,25))
      val entries = List(entry3, entry1, entry2)
      val expected = List(entry1, entry2, entry3)
      ZIO.succeed(assert(entries.sorted)(equalTo(expected)))
    }
    , testM("optimal minute") {
      for
        data   <- FileReader.getContent("input/day04/example1.txt")
        entries   <- GuardEntry.fromStringList(data)
        collected <- GuardService.collectEntries(entries)
        guards    <- GuardService.compressGuards(collected)
        opt       <- GuardService.optimalGuard(guards)
        (guard, sleepMap) = opt
        testResult <- assertM(GuardService.sleepingMinute(sleepMap))(equalTo((24, 2)))
      yield testResult
    }
    , testM("day04 part1") {
      for
        data <- FileReader.getContent("input/day04/example1.txt")
        input = AdventInputMock.GetData(value(data))
        result = SingleDay.part1.provideLayer(input >>> day04.live)
        testResult <- assertM(result)(equalTo(AdventNumResult(240)))
      yield testResult
    }
  )

object Day04Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day04Part2")(
    testM("day04 part2") {
      for
        data <- FileReader.getContent("input/day04/example1.txt")
        input = AdventInputMock.GetData(value(data))
        result = SingleDay.part2.provideLayer(input >>> day04.live)
        testResult <- assertM(result)(equalTo(AdventNumResult(4455)))
      yield testResult
    }
  )
