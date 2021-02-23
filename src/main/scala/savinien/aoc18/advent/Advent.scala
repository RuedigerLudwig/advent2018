package savinien.aoc18
package advent

import zio._
import zio.console.Console

object Advent extends App:
  def run(args: List[String]) =
    loop(1, args).exitCode

  private def loop(day: Int, args: List[String]): UIO[Unit] =
    if day > Days.MAX_DAY then
      Task.succeed(())
    else if args.isEmpty || args.contains(f"day$day%02d") then
      singleDay.provideLayer(prepareEnvironment(day)) *> loop(day + 1, args)
    else 
      loop(day + 1, args)

  private val singleDay = 
    SingleDay.part1 *> SingleDay.part2

  private def prepareEnvironment(day: Int): ULayer[SingleDay] =
    val output: ULayer[AdventOutput]              = Console.live >>> AdventOutput.live(day)
    val io: ULayer[AdventInput with AdventOutput] = AdventInput.live(day) ++ output

    val singleDay = io >>> Days.getDay(day)

    singleDay
