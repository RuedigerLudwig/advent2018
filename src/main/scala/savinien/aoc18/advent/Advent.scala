package savinien.aoc18
package advent

import zio._
import zio.console._

object Advent extends App:
  def run(args: List[String]) =
    loop(1, args).exitCode

  private def loop(day: Int, args: List[String]): UIO[Unit] =
    if day > Days.MAX_DAY then
      Task.succeed(())
    else if args.isEmpty || args.contains(f"day$day%02d") then
      singleDay(day).provideLayer(prepareEnvironment(day)) *> loop(day + 1, args)
    else 
      loop(day + 1, args)

  private def prepareEnvironment(day: Int) = 
    val io: ULayer[AdventInput] = AdventInput.live(day) 

    val singleDay = io >>> Days.getDay(day) ++ Console.live

    singleDay

  private def singleDay(day: Int) = 
    singlePart(SingleDay.part1, day, 1) *> 
    singlePart(SingleDay.part2, day, 2)

  private def singlePart(effect: ZIO[SingleDay, AdventException, AdventResult], day: Int, part: Int) =
    (for
      result <- effect
      _      <- putStrLn(f"Result for day $day Part $part: $result")
    yield ())
    .catchAll { e => putStrLn(f"Error for day $day Part $part: $e") } 