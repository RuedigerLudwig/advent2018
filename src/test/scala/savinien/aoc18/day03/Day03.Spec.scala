package savinien.aoc18
package day03

import common.*

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*

object Day03Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day03Part1") (
    testM("extract claim") {
      assertM(day03.Claim.fromString("#123 @ 3,2: 5x4"))(equalTo(Claim(123, 3, 2, 5, 4)))
    }
    , testM("simple crossover") {
      val claim = Claim(1, 3, 2, 5, 4)
      val claims = List(claim)
      val sc = SingleClaim(claim)
      val expected = Map ( 
        Pos(3, 2) -> sc, Pos(4, 2) -> sc, Pos(5, 2) -> sc, Pos(6, 2) -> sc, Pos(7, 2) -> sc,
        Pos(3, 3) -> sc, Pos(4, 3) -> sc, Pos(5, 3) -> sc, Pos(6, 3) -> sc, Pos(7, 3) -> sc,
        Pos(3, 4) -> sc, Pos(4, 4) -> sc, Pos(5, 4) -> sc, Pos(6, 4) -> sc, Pos(7, 4) -> sc,
        Pos(3, 5) -> sc, Pos(4, 5) -> sc, Pos(5, 5) -> sc, Pos(6, 5) -> sc, Pos(7, 5) -> sc
      )
      assertM(day03.MatterService.getFabric(claims))(equalTo(expected))
    }
    , testM("only crossover") {
      val claims = List(Claim(1, 1, 3, 4, 4), Claim(2,3,1,4,4), Claim(3,5,5,2,2))
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
    testM("all single claims") {
      val claim1 = Claim(1, 1, 3, 4, 4)
      val claim2 = Claim(2,3,1,4,4)
      val claim3 = Claim(3,5,5,2,2)
      val claims = List(claim1, claim2, claim3)
      val expected = Map(claim1 -> 12, claim2 -> 12, claim3 -> 4)
      assertM(
        for
          fabric <- day03.MatterService.getFabric(claims)
          result <- day03.MatterService.findAllSingleClaims(fabric.values)
        yield result
      )(equalTo(expected))
    }
    , testM("only crossover") {
      val claims = List(Claim(1, 1, 3, 4, 4), Claim(2,3,1,4,4), Claim(3,5,5,2,2))
      val expected = Claim(3,5,5,2,2)
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