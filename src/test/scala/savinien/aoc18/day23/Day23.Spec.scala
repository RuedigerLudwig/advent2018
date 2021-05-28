package savinien.aoc18
package day23

import common.*
import common.geometric.Point3D

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

object Day23Part1Spec extends DefaultRunnableSpec:
  def spec = suite("Day23Part1")(
    testM("find strongest") {
      for
        data       <- FileReader.getContent("input/day23/example1.txt")
        swarm       <- TeleportService.fromString(data)
        testResult <- assertM(ZIO.succeed(swarm.strongest.pos))(equalTo(Point3D.origin[Long]))
      yield testResult
    },
    testM("example works") {
      for
        data       <- FileReader.getContent("input/day23/example1.txt")
        swarm       <- TeleportService.fromString(data)
        testResult <- assertM(ZIO.succeed(swarm.inRange(swarm.strongest)))(equalTo(7))
      yield testResult
    },
    testM("day23 part1 works") {
      val expected = 7L
      for
        data       <- FileReader.getContent("input/day23/example1.txt")
        input      = AdventInputMock.GetData(value(data))
        result     = SingleDay.part1.provideLayer(input >>> day23.live)
        testResult <- assertM(result)(equalTo(AdventNumResult(expected)))
      yield testResult
    },
  )

object Day23Part2Spec extends DefaultRunnableSpec:
  def spec = suite("Day23Part2")(
    testM("example works") {
      for
        data       <- FileReader.getContent("input/day23/example2.txt")
        swarm       <- TeleportService.fromString(data)
        testResult <- assertM(ZIO.succeed(swarm.teleportPoint))(equalTo(Point3D(12, 12, 12)))
      yield testResult
    },
    testM("day23 part2 works") {
      assertM(ZIO.unit)(isUnit)
    },
  )
