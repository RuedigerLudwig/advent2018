package savinien.aoc18
package advent

import advent.Days
import days.SingleDay
import input.AdventInput
import output.AdventOutput
import zio._
import zio.console.Console

object Advent extends App {
  def run(args: List[String]) = {
    def loop(day: Int): Task[Unit] =
      if (day > Days.MAX_DAY)
        Task.succeed(())
      else if (args.isEmpty || args.contains(f"day$day%02d")) {
        singleDay.provideLayer(prepareEnvironment(day)) *> loop(day + 1)
      } else {
        loop(day + 1)
      }

    loop(1).exitCode
  }

  val singleDay =
    SingleDay.part1 *> SingleDay.part2

  private def prepareEnvironment(day: Int): ULayer[SingleDay] = {
    val output: ULayer[AdventOutput]              = Console.live >>> AdventOutput.live(day)
    val io: ULayer[AdventInput with AdventOutput] = AdventInput.live(day) ++ output

    val singleDay = io >>> Days.getDay(day)

    singleDay
  }
}
