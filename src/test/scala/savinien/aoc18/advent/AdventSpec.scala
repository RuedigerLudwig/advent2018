package savinien.aoc18
package advent

import zio.test._

object AdventSpec extends DefaultRunnableSpec:
  def spec = suite("All Tests")(
      day01.Tests.all
      , day02.Tests.all
  )
