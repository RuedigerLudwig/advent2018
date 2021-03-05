package savinien.aoc18

import zio._

package object common {
  type AdventTask[A] = IO[AdventException, A]
}
