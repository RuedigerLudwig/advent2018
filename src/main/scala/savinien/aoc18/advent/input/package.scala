package savinien.aoc18.advent

import savinien.aoc18.day01.Common
import zio._
import zio.macros.accessible

package object input {
  type AdventInput = Has[AdventInput.Service]

  @accessible
  object AdventInput {
    trait Service {
      def getData: Task[List[String]];
    }

    val live = (day: Int) =>
      ZLayer.succeed {
        new Service {
          override def getData: Task[List[String]] =
            Common.getLines(f"input/day$day%02d.txt")
        }
      }
  }
}
