package savinien.aoc18.advent

import zio._
import zio.console.Console
import savinien.aoc18.advent.days.SingleDay
import savinien.aoc18.advent.input.AdventInput
import savinien.aoc18.advent.output.AdventOutput
import savinien.aoc18.day01.Day01

object Advent extends App {
  def MAX_DAY = 1

  def getDay(day: Int) = day match {
    case 1 => Day01.live
  }

  def run(args: List[String]) = {
    def loop(day: Int): Task[Unit] =
      if (day > MAX_DAY)
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

    val singleDay = io >>> getDay(day)

    singleDay
  }
}
