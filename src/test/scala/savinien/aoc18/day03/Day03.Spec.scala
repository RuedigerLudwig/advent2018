package savinien.aoc18
package day03

import scala.language.adhocExtensions

import common.*
import parser.Success
import areaParsers.*

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*

import org.scalatest.*
import flatspec.AnyFlatSpec

class AreaTest extends AnyFlatSpec:

  "an area string".should("be parsed correctly") in {
    val input    = "3,2: 5x4"
    val parser   = areaSizeParser
    val expected = Success(Area(Pos(3, 2), Pos(8, 6)))
    val result   = parse(parser)(input)
    assert(result == expected)
  }

  "the size of an area".should("be calculated correctly") in {
    val area = Area(Pos(3, 2), Pos(8, 6))
    val expected = 20
    val result = area.size
    assert(result == expected)
  }

  "an area union".should("equal itself") in {
    val area = Area(Pos(3, 2), Pos(8, 6))
    val expected = Some(area)
    val result = area.union(area)
    assert(result == expected)
  }

  it.should("return an actual crossover") in {
    val area1 = Area(Pos(1, 3), Pos(5, 7))
    val area2 = Area(Pos(3, 1), Pos(7, 5))
    val expected = Some(Area(Pos(3, 3), Pos(5, 5)))
    val result = area1.union(area2)
    assert(result == expected)
  }
  
  it.should("return the same crossover the other way round") in {
    val area1 = Area(Pos(1, 3), Pos(5, 7))
    val area2 = Area(Pos(3, 1), Pos(7, 5))
    val expected = Some(Area(Pos(3, 3), Pos(5, 5)))
    val result = area2.union(area1)
    assert(result == expected)
  }
  
  it.should("be none for unrelated") in {
    val area1 = Area(Pos(1, 3), Pos(5, 7))
    val area3 = Area(Pos(5, 5), Pos(7, 7))
    val expected = None
    val result = area1.union(area3)
    assert(result == expected)
  }

  it.should("be none for unrelated 2") in {
    val area2 = Area(Pos(3, 1), Pos(7, 5))
    val area3 = Area(Pos(5, 5), Pos(7, 7))
    val expected = None
    val result = area2.union(area3)
    assert(result == expected)
  }

  it.should("find touching areas correctly") in {
    val area2 = Area(Pos(3, 1), Pos(7, 5))
    val area3 = Area(Pos(5, 4), Pos(7, 7))
    val expected = Some(Area(Pos(5, 4), Pos(7, 5)))
    val result = area2.union(area3)
    assert(result == expected)
  }

object Day03Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day03Part1") (
    testM("extract claim") {
      assertM(day03.Claim.fromString("#123 @ 3,2: 5x4"))(equalTo(Claim(123, Area(Pos(3, 2), Pos(8, 6)))))
    }
    , testM("only crossover") {
      val claims = List(Claim(1, Area(Pos(1, 3), Pos(5, 7))), Claim(2, Area(Pos(3, 1), Pos(7, 5))), Claim(3, Area(Pos(5, 5), Pos(7, 7))))
      val expected = 4
      assertM(day03.MatterService.getMultiClaimCount(claims))(equalTo(expected))
    }
    , testM("day03 part1") {
      val input = AdventInputMock.GetData(
        value(List("#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2").mkString("\n"))
      )
      val result = SingleDay.part1.provideLayer(input >>> day03.live)
      assertM(result)(equalTo(AdventIntResult(4)))
    }
  )

object Day03Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day03Part2") (
    testM("only crossover") {
      val claims = List(Claim(1, Area(Pos(1, 3), Pos(5, 7))), Claim(2, Area(Pos(7, 5), Pos(4, 4))), Claim(3, Area(Pos(5, 5), Pos(7, 7))))
      val expected = Claim(3, Area(Pos(5, 5), Pos(7, 7)))
      assertM(day03.MatterService.findSolitaireClaim(claims))(equalTo(expected))
    }
    , testM("day03 part2") {
      val input = AdventInputMock.GetData(
        value(List("#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2").mkString("\n"))
      )
      val result = SingleDay.part2.provideLayer(input >>> day03.live)
      assertM(result)(equalTo(AdventIntResult(3)))
    }
  )