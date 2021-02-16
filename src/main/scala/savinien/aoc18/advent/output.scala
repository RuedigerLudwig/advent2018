package savinien.aoc18.advent

import zio._
import zio.console.Console
import zio.macros.accessible

object output {
  type AdventOutput = Has[AdventOutput.Service]

  @accessible
  object AdventOutput {
    trait Service {
      def output(part: Int, result: String): UIO[Unit]
      def output(part: Int, result: Int): UIO[Unit]
    }

    def live(day: Int): URLayer[Console, AdventOutput] = ZLayer.fromService { console =>
      new Service {
        override def output(part: Int, result: String): UIO[Unit] =
          console.putStrLn(f"Result of Day $day%02d Part $part:\n$result")
        override def output(part: Int, result: Int): UIO[Unit] =
          console.putStrLn(f"Result of Day $day%02d Part $part: $result")
      }
    }
  }
}
