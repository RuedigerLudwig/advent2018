package savinien.aoc18
package dayXX

import common._

import zio.test._
import zio.test.Assertion._
import zio.test.mock.Expectation._
import zio._

object DayXXPart1Spec extends DefaultRunnableSpec:
  def spec = Tests.part1

object DayXXPart2Spec extends DefaultRunnableSpec:
  def spec = Tests.part2

object Tests:
  def all = suite("DayXXAll")(
    part1, part2
  )

  def part1 =
    suite("DayXXPart1") {
      testM("it works") {
        assertM(ZIO.succeed(()))(isUnit)
      }
    }

  def part2 =
    suite("DayXXPart2") {
      testM("it works") {
        assertM(ZIO.succeed(()))(isUnit)
      }
    }