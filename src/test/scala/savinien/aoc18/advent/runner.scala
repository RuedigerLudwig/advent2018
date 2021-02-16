package savinien.aoc18
package advent

import zio.test._

object Day01Part1Spec extends DefaultRunnableSpec {
  def spec = suite("All Tests")(
      day01.Tests.part1
    , day01.Tests.part2
  )
}
