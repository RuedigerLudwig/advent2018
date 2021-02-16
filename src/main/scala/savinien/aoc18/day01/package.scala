package savinien.aoc18

import savinien.aoc18.advent.days.SingleDay
import savinien.aoc18.advent.input.AdventInput
import savinien.aoc18.advent.output.AdventOutput
import zio._

package object Day01 {

  val live: ZLayer[AdventOutput with AdventInput, Nothing, SingleDay] =
    ZLayer.fromServices[AdventInput.Service, AdventOutput.Service, SingleDay.Service] { (input, output) =>
      new SingleDay.Service {

        override def part1: Task[Unit] =
          for {
            numbers <- getNumbers
            _       <- output.output(1, numbers.sum)
          } yield ()

        override def part2: Task[Unit] =
          for {
            numbers <- getNumbers
            repeat  <- getRepeat(numbers)
            _       <- output.output(2, repeat)
          } yield ()

        private def getNumbers =
          for {
            lines   <- input.getData
            numbers <- ZIO.effect(lines.map(_.toInt).toList)
          } yield numbers

        private def getRepeat(list: List[Int]) =
          ZIO.succeed(
              LazyList
              .continually(list)
              .flatten
              .scanLeft((0, Set[Int]())) {
                case ((last, set), next) => ((last + next), set + last)
              }
              .dropWhile {
                case (last, set) => !set.contains(last)
              }
              .head
              ._1
          )
      }
    }
}
