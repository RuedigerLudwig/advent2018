package savinien.aoc18
package day24

import common.*

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

import parsers.TokenParsers.*

import scala.language.adhocExtensions
import org.scalatest.*
import flatspec.AnyFlatSpec


class GroupTests extends AnyFlatSpec:
  "parser".should("parse a group correctly") in {
    val input = "18 units each with 729 hit points (weak to fire; immune to cold, slashing) with an attack that does 8 radiation damage at initiative 10"
    val result = parse(Group.parser)(input)
    assert(result.isSuccess)
  }
  it.should("parse a group wthout properties correctly") in {
    val input = "18 units each with 729 hit points with an attack that does 8 radiation damage at initiative 10"
    val result = parse(Group.parser)(input)
    assert(result.isSuccess)
  }

object Day24Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day24Part1")(
    testM("check one round") {
      for
        data       <- FileReader.getContent("input/day24/example1.txt")
        reindeer   <- ImmuneService.fromString(data)
        testResult <- assertM(ZIO.succeed(reindeer.fight.immuneSystem.units))(equalTo(905))
      yield testResult
    },
    testM("check one round part II") {
      for
        data       <- FileReader.getContent("input/day24/example1.txt")
        reindeer   <- ImmuneService.fromString(data)
        testResult <- assertM(ZIO.succeed(reindeer.fight.infection.units))(equalTo(797+4434))
      yield testResult
    },
    testM("check two rounds") {
      for
        data       <- FileReader.getContent("input/day24/example1.txt")
        reindeer   <- ImmuneService.fromString(data)
        testResult <- assertM(ZIO.succeed(reindeer.fight.fight.immuneSystem.units))(equalTo(761))
      yield testResult
    },
    testM("check two rounds part II") {
      for
        data       <- FileReader.getContent("input/day24/example1.txt")
        reindeer   <- ImmuneService.fromString(data)
        testResult <- assertM(ZIO.succeed(reindeer.fight.fight.infection.units))(equalTo(793+4434))
      yield testResult
    },
    testM("check finished battle") {
      for
        data       <- FileReader.getContent("input/day24/example1.txt")
        reindeer   <- ImmuneService.fromString(data)
        testResult <- assertM(ZIO.succeed(reindeer.finishBattle.units))(equalTo(5216))
      yield testResult
    },
    testM("day24 part1 works") {
      assertM(ZIO.unit)(isUnit)
    },
  )

object Day24Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day24Part2")(
    testM("check boosted battle") {
      for
        data       <- FileReader.getContent("input/day24/example1.txt")
        reindeer   <- ImmuneService.fromString(data)
        testResult <- assertM(ZIO.succeed(reindeer.boostedBattle.units))(equalTo(51))
      yield testResult
    },
    testM("day24 part2 works") {
      assertM(ZIO.unit)(isUnit)
    },
  )
