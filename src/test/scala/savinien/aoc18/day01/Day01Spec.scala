package savinien.aoc18
package day01

import zio.test._
import zio.test.Assertion._
import zio.test.mock.Expectation._

import common._

object Day01Part1Spec extends DefaultRunnableSpec:
  def spec = Tests.part1

object Day01Part2Spec extends DefaultRunnableSpec:
  def spec = Tests.part2

object Tests:
  def all = suite("Day01All")(
    part1,
    part2
  )

  def part1 = suite("Day01Part1")(
      testM("day01 summes correctly 1") {
      val input = AdventInputMock.GetData(
          value(List("+1", "+1", "+1").mkString("\n"))
      )

      val result = SingleDay.part1.provideLayer(input >>> day01.live)
      assertM(result)(equalTo(AdventIntResult(3)))
    }
    , testM("day01 summes correctly 2") {
      val input = AdventInputMock.GetData(
          value(List("+1", "+1", "-2").mkString("\n"))
      )

      val result = SingleDay.part1.provideLayer(input >>> day01.live)
      assertM(result)(equalTo(AdventIntResult(0)))
    }
    , testM("day01 summes correctly 3") {
      val input = AdventInputMock.GetData(
          value(List("-1", "-2", "-3").mkString("\n"))
      )

      val result = SingleDay.part1.provideLayer(input >>> day01.live)
      assertM(result)(equalTo(AdventIntResult(-6)))
    }
    , testM("day01 can handle wrong input") {
      val input = AdventInputMock.GetData(
          value(List("-1", "xxx", "-3").mkString("\n"))
      )

      val result = SingleDay.part1.provideLayer(input >>> day01.live)
      assertM(result.run)(fails(isSubtype[AdventException](anything)))
    }
  )

  def part2 = suite("Day01Part2")(
      testM("day01 finds correct repeat 1") {
      val input = AdventInputMock.GetData(
          value(List("1", "-1").mkString("\n"))
      )

      val result = SingleDay.part2.provideLayer(input >>> day01.live)
      assertM(result)(equalTo(AdventIntResult(0)))
    }
    , testM("day01 finds correct repeat 2") {
      val input = AdventInputMock.GetData(
          value(List("+3", "+3", "+4", "-2", "-4").mkString("\n"))
      )

      val result = SingleDay.part2.provideLayer(input >>> day01.live)
      assertM(result)(equalTo(AdventIntResult(10)))
    }
    , testM("day01 finds correct repeat 3") {
      val input = AdventInputMock.GetData(
          value(List("-6", "+3", "+8", "+5", "-6").mkString("\n"))
      )

      val result = SingleDay.part2.provideLayer(input >>> day01.live)
      assertM(result)(equalTo(AdventIntResult(5)))
    }
    , testM("day01 finds correct repeat 4") {
      val input = AdventInputMock.GetData(
          value(List("+7", "+7", "-2", "-7", "-4").mkString("\n"))
      )

      val result = SingleDay.part2.provideLayer(input >>> day01.live)
      assertM(result)(equalTo(AdventIntResult(14)))
    }
  )

end Tests