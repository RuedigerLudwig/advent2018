package savinien.aoc18
package dayXX

import common.*

import zio.test.*
import zio.test.Assertion.*
import zio.test.mock.Expectation.*
import zio.*

object DayXXPart1Spec extends DefaultRunnableSpec:
  def spec = suite("DayXXPart1")(
    testM("dayxx part1 works") {
      assertM(ZIO.unit)(isUnit)
    },
  )

object DayXXPart2Spec extends DefaultRunnableSpec:
  def spec = suite("DayXXPart2")(
    testM("dayxx part2 works") {
      assertM(ZIO.unit)(isUnit)
    },
  )
