package savinien.aoc18
package day01

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*

import common.*

object Day01Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day01Part1")(
      testM("day01 summes correctly 1") {
      val input = AdventInputMock.GetData(
          value(List("+1", "+1", "+1").mkString("\n"))
      )

      val result = SingleDay.part1.provideLayer(input >>> day01.live)
      assertM(result)(equalTo(AdventNumResult(3)))
    }
    , testM("day01 summes correctly 2") {
      val input = AdventInputMock.GetData(
          value(List("+1", "+1", "-2").mkString("\n"))
      )

      val result = SingleDay.part1.provideLayer(input >>> day01.live)
      assertM(result)(equalTo(AdventNumResult(0)))
    }
    , testM("day01 summes correctly 3") {
      val input = AdventInputMock.GetData(
          value(List("-1", "-2", "-3").mkString("\n"))
      )

      val result = SingleDay.part1.provideLayer(input >>> day01.live)
      assertM(result)(equalTo(AdventNumResult(-6)))
    }
    , testM("day01 can handle wrong input") {
      val input = AdventInputMock.GetData(
          value(List("-1", "xxx", "-3").mkString("\n"))
      )

      val result = SingleDay.part1.provideLayer(input >>> day01.live)
      assertM(result.run)(fails(isSubtype[AdventException](anything)))
    }
  )

object Day01Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day01Part2")(
      testM("day01 finds correct repeat 1") {
      val input = AdventInputMock.GetData(
          value(List("1", "-1").mkString("\n"))
      )

      val result = SingleDay.part2.provideLayer(input >>> day01.live)
      assertM(result)(equalTo(AdventNumResult(0)))
    }
    , testM("day01 finds correct repeat 2") {
      val input = AdventInputMock.GetData(
          value(List("+3", "+3", "+4", "-2", "-4").mkString("\n"))
      )

      val result = SingleDay.part2.provideLayer(input >>> day01.live)
      assertM(result)(equalTo(AdventNumResult(10)))
    }
    , testM("day01 finds correct repeat 3") {
      val input = AdventInputMock.GetData(
          value(List("-6", "+3", "+8", "+5", "-6").mkString("\n"))
      )

      val result = SingleDay.part2.provideLayer(input >>> day01.live)
      assertM(result)(equalTo(AdventNumResult(5)))
    }
    , testM("day01 finds correct repeat 4") {
      val input = AdventInputMock.GetData(
          value(List("+7", "+7", "-2", "-7", "-4").mkString("\n"))
      )

      val result = SingleDay.part2.provideLayer(input >>> day01.live)
      assertM(result)(equalTo(AdventNumResult(14)))
    }
  )