package savinien.aoc18
package advent

import zio.test._

object Day01Part1Spec extends DefaultRunnableSpec:
  def spec = suite("All Tests")(
      day01.Tests.all
      , day02.Tests.all
  )
