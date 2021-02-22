package savinien.aoc18
package advent

import advent.days
import days.SingleDay
import input.AdventInput
import output.AdventOutput
import zio._
import zio.console.Console

object Advent extends App {
  def run(args: List[String]) =
    loop(1, args).exitCode

  private def loop(day: Int, args: List[String]): Task[Unit] =
    if day > days.MAX_DAY then
      Task.succeed(())
    else if args.isEmpty || args.contains(f"day$day%02d") then
      singleDay.provideLayer(prepareEnvironment(day)) *> loop(day + 1, args)
    else 
      loop(day + 1, args)
    

  private val singleDay =
    SingleDay.part1 *> SingleDay.part2

  private def prepareEnvironment(day: Int): ULayer[SingleDay] = {
    val output: ULayer[AdventOutput]              = Console.live >>> AdventOutput.live(day)
    val io: ULayer[AdventInput with AdventOutput] = AdventInput.live(day) ++ output

    val singleDay = io >>> days.getDay(day)

    singleDay
  }
}
