package savinien.aoc18
package day20

import common.*
import parsers.TokenParsers.*
import parsers.*

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

import scala.language.adhocExtensions
import org.scalatest.*
import flatspec.AnyFlatSpec
import java.util.AbstractMap.SimpleEntry
import savinien.aoc18.common.geometric.Direction.*

class PathTest extends AnyFlatSpec:
  "parsing".should("be parsed be correct for simple path") in {
    val input = "^WNE$"
    val expected = "WNE"
    val result = parse(Path.fullPath)(input)
    assert(result.get.show == expected)
  }

  it.should("should also work for alternatives") in {
    val input = "^(E|W)$"
    val expected = "(E|W)"
    val result = parse(Path.fullPath)(input)
    assert(result.get.show == expected)
  }

  it.should("should also work for alternatives with empty option") in {
    val input = "^(E|W|)$"
    val expected = "(E|W|!)"
    val result = parse(Path.fullPath)(input)
    assert(result.get.show == expected)
  }

  it.should("should also work for connected items") in {
    val input = "^N(E|W)S$"
    val expected = "[N+(E|W)+S]"
    val result = parse(Path.fullPath)(input)
    assert(result.get.show == expected)
  }

  it.should("should also work for deeper levels") in {
    val input = "^N(E|W(N|S))S$"
    val expected = "[N+(E|[W+(N|S)])+S]"
    val result = parse(Path.fullPath)(input)
    assert(result.get.show == expected)
  }

  it.should("parse a complex map").in {
    val input = "^ENWWW(NEEE|SSE(EE|N))$"
    val expected = "[ENWWW+(NEEE|[SSE+(EE|N)])]"
    val path = parse(Path.fullPath)(input)
    val result = parse(Path.fullPath)(input)
    assert(result.get.show == expected)
  }

  "the furthest room".should("walk through the expected number of doors 1").in {
    val input = "^WNE$"
    val expected = 3
    val path = parse(Path.fullPath)(input)
    val result = Walker.furthestRoom(path.get)
    assert(result == expected)
  }

  it.should("walk through the expected number of doors 2").in {
    val input = "^ENWWW(NEEE|SSE(EE|N))$"
    val expected = 10
    val path = parse(Path.fullPath)(input)
    val result = Walker.furthestRoom(path.get)
    assert(result == expected)
  }

  it.should("walk through the expected number of doors 3").in {
    val input = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
    val expected = 18
    val path = parse(Path.fullPath)(input)
    val result = Walker.furthestRoom(path.get)
    assert(result == expected)
  }

  it.should("walk through the expected number of doors 4").in {
    val input = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
    val expected = 23
    val path = parse(Path.fullPath)(input)
    val result = Walker.furthestRoom(path.get)
    assert(result == expected)
  }

  it.should("walk through the expected number of doors 5").in {
    val input = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
    val expected = 31
    val path = parse(Path.fullPath)(input)
    val result = Walker.furthestRoom(path.get)
    assert(result == expected)
  }

object Day20Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day20Part1")(
    testM("day20 part1 works") {
      val expected = 31
      val input = AdventInputMock.GetData(
        value("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")
      )
      val result = SingleDay.part1.provideLayer(input >>> day20.live)
      assertM(result)(equalTo(AdventNumResult(expected)))
    },
  )

object Day20Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day20Part2")(
    testM("day20 part2 works") {
      val expected = 28
      val input = AdventInputMock.GetData(
        value("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")
      ) && AdventInputMock.provideIntSetting("MinDoors", 20)
      val result = SingleDay.part2.provideLayer(input >>> day20.live)
      assertM(result)(equalTo(AdventNumResult(expected)))
    },
  )
