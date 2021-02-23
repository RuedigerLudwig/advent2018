package savinien.aoc18.advent

import zio._
import zio.console.Console

type AdventOutput = Has[AdventOutput.Service]

object AdventOutput:
  trait Service:
    def outputInt(part: Int, result: Int): UIO[Unit]
    def outputSingleLine(part: Int, result: String): UIO[Unit]
    def outputMultiLine(part: Int, result: String): UIO[Unit]
    def error(part: Int, error: AdventException): UIO[Unit]

  def live(day: Int): URLayer[Console, AdventOutput] = ZLayer.fromService { console =>
    new Service {
      override def outputInt(part: Int, result: Int): UIO[Unit] =
        console.putStrLn(f"Result of Day $day%02d Part $part: $result")

      override def outputSingleLine(part: Int, result: String): UIO[Unit] =
        console.putStrLn(f"Result of Day $day%02d Part $part: $result")

      override def outputMultiLine(part: Int, result: String): UIO[Unit] =
        console.putStrLn(f"Result of Day $day%02d Part $part:") *>
        console.putStrLn(result)

      override def error(part: Int, error: AdventException): UIO[Unit] =
        console.putStrLn(f"Error in Day $day%02d Part $part: $error")
    }
  }
