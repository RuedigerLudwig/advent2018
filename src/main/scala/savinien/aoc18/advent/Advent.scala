package savinien.aoc18
package advent

import zio._
import zio.console._

object Advent extends App:
  def run(args: List[String]) =
    def loop(day: Int): UIO[Unit] =
      if day > Days.MAX_DAY then
        UIO.succeed(())
      else if args.isEmpty || args.contains(f"day$day%02d") then
        runSingleDay(day) *> loop(day + 1)
      else 
        loop(day + 1)

    loop(1).exitCode

  private def runSingleDay(day: Int) = 
    (runSinglePart(SingleDay.part1, day, 1) *> runSinglePart(SingleDay.part2, day, 2))
    .provideLayer( AdventInput.live(day) >>> Days.getDay(day) ++ Console.live)

  private def runSinglePart(effect: ZIO[SingleDay, AdventException, AdventResult], day: Int, part: Int) =
    (for
      result <- effect
      _      <- putStrLn(f"Result for day $day Part $part: $result")
    yield ())
    .catchAll { e => putStrLn(f"Error on day $day Part $part: $e") } 