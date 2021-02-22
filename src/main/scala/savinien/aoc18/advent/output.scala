package savinien.aoc18.advent

import zio._
import zio.console.Console

object output {
  type AdventOutput = Has[AdventOutput.Service]

  object AdventOutput {
    trait Service {
      def outputString(part: Int, result: String): UIO[Unit]
      def outputInt(part: Int, result: Int): UIO[Unit]
      //def error(part: Int, result: String): UIO[Unit]
    }

    def live(day: Int): URLayer[Console, AdventOutput] = ZLayer.fromService { console =>
      new Service {
        override def outputString(part: Int, result: String): UIO[Unit] =
          console.putStrLn(f"Result of Day $day%02d Part $part:\n$result")
        override def outputInt(part: Int, result: Int): UIO[Unit] =
          console.putStrLn(f"Result of Day $day%02d Part $part: $result")
        //override def error(part: Int, error: String): UIO[Unit] =
        //  console.putStrLn(f"Error in Day $day%02d Part $part: $error")
      }
    }
  }
}
