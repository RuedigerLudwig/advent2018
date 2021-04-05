package savinien.aoc18

import zio.*

package object common {
  type AdventTask[A] = IO[AdventException, A]
}
