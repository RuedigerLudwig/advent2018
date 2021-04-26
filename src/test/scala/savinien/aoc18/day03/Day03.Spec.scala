package savinien.aoc18
package day03

import scala.language.adhocExtensions

import common.*
import common.point.Point
import common.area.Area

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*


object Day03Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day03Part1") (
    testM("extract claim") {
      assertM(day03.Claim.fromString("#123 @ 3,2: 5x4"))(equalTo(Claim(123, Area(Point(3, 2), Point(7, 5)))))
    }
    , testM("only crossover") {
      val claims = List(Claim(1, Area(Point(1, 3), Point(4, 6))), Claim(2, Area(Point(3, 1), Point(6, 4))), Claim(3, Area(Point(5, 5), Point(6, 6))))
      val expected = 4
      assertM(day03.MatterService.getMultiClaimCount(claims))(equalTo(expected))
    }
    , testM("day03 part1") {
      val input = AdventInputMock.GetData(
        value(List("#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2").mkString("\n"))
      )
      val result = SingleDay.part1.provideLayer(input >>> day03.live)
      assertM(result)(equalTo(AdventNumResult(4)))
    }
  )

object Day03Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day03Part2") (
    testM("only crossover") {
      val claims = List(Claim(1, Area(Point(1, 3), Point(4, 6))), Claim(2, Area(Point(6, 4), Point(3, 1))), Claim(3, Area(Point(5, 5), Point(6, 6))))
      val expected = Claim(3, Area(Point(5, 5), Point(6, 6)))
      assertM(day03.MatterService.findSolitaireClaim(claims))(equalTo(expected))
    }
    , testM("day03 part2") {
      val input = AdventInputMock.GetData(
        value(List("#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2").mkString("\n"))
      )
      val result = SingleDay.part2.provideLayer(input >>> day03.live)
      assertM(result)(equalTo(AdventNumResult(3)))
    }
  )