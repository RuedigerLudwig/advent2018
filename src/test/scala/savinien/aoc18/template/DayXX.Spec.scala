package savinien.aoc18
package dayXX

import common._

import zio.test._
import zio.test.Assertion._
import zio.test.mock.Expectation._
import zio._

object DayXXPart1Spec extends DefaultRunnableSpec:
  def spec = suite("DayXXPart1")(
    testM("dayxx part1 works") {
      assertM(ZIO.unit)(isUnit)
    }
  )

object DayXXPart2Spec extends DefaultRunnableSpec:
  def spec = suite("DayXXPart2")(
    testM("dayxx part2 works") {
      assertM(ZIO.unit)(isUnit)
    }
  )
