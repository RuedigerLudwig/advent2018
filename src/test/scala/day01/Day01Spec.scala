package day01

import advent.AdventInputMock
import advent.AdventOutputMock
import savinien.aoc18.Day01
import savinien.aoc18.advent.days.SingleDay
import zio.test.Assertion._
import zio.test._
import zio.test.mock.Expectation._

object Day01Part1Spec extends DefaultRunnableSpec {
  def spec = suite("Day01Part1")(
      testM("day01 summes correctly 1") {
      val input = AdventInputMock.GetData(
          value(List("+1", "+1", "+1"))
      )
      val expected = AdventOutputMock.Output._0(
          equalTo((1, 3))
      )

      val result = SingleDay.part1.provideLayer(input ++ expected >>> Day01.live)
      assertM(result)(isUnit)
    }
    , testM("day01 summes correctly 2") {
      val input = AdventInputMock.GetData(
          value(List("+1", "+1", "-2"))
      )
      val expected = AdventOutputMock.Output._0(
          equalTo((1, 0))
      )

      val result = SingleDay.part1.provideLayer(input ++ expected >>> Day01.live)
      assertM(result)(isUnit)
    }
    , testM("day01 summes correctly 3") {
      val input = AdventInputMock.GetData(
          value(List("-1", "-2", "-3"))
      )
      val expected = AdventOutputMock.Output._0(
          equalTo((1, -6))
      )

      val result = SingleDay.part1.provideLayer(input ++ expected >>> Day01.live)
      assertM(result)(isUnit)
    }
  )
}

object Day01Part2Spec extends DefaultRunnableSpec {
  def spec = suite("Day01Part2")(
      testM("day01 finds correct repeat 1") {
      val input = AdventInputMock.GetData(
          value(List("1", "-1"))
      )
      val expected = AdventOutputMock.Output._0(
          equalTo((2, 0))
      )

      val result = SingleDay.part2.provideLayer(input ++ expected >>> Day01.live)
      assertM(result)(isUnit)
    }
    , testM("day01 finds correct repeat 2") {
      val input = AdventInputMock.GetData(
          value(List("+3", "+3", "+4", "-2", "-4"))
      )
      val expected = AdventOutputMock.Output._0(
          equalTo((2, 10))
      )

      val result = SingleDay.part2.provideLayer(input ++ expected >>> Day01.live)
      assertM(result)(isUnit)
    }
    , testM("day01 finds correct repeat 3") {
      val input = AdventInputMock.GetData(
          value(List("-6", "+3", "+8", "+5", "-6"))
      )
      val expected = AdventOutputMock.Output._0(
          equalTo((2, 5))
      )

      val result = SingleDay.part2.provideLayer(input ++ expected >>> Day01.live)
      assertM(result)(isUnit)
    }
    , testM("day01 finds correct repeat 4") {
      val input = AdventInputMock.GetData(
          value(List("+7", "+7", "-2", "-7", "-4"))
      )
      val expected = AdventOutputMock.Output._0(
          equalTo((2, 14))
      )

      val result = SingleDay.part2.provideLayer(input ++ expected >>> Day01.live)
      assertM(result)(isUnit)
    }
  )
}
