package savinien.aoc18
package day01

import advent.output.AdventOutput
import advent.input.AdventInput
import advent.days.SingleDay
import zio._

object ChronalCalibration {
    def live: URLayer[AdventInput with AdventOutput, SingleDay] =
      ZLayer.fromServices[
          AdventInput.Service
        , AdventOutput.Service
        , SingleDay.Service
      ] { (input, output) =>
        new SingleDay.Service {

          override def part1: Task[Unit] =
            for 
              numbers <- getNumbers
              _       <- output.outputInt(1, numbers.sum)
            yield ()

          override def part2: Task[Unit] =
            for 
              numbers <- getNumbers
              repeat  <- getRepeat(numbers)
              _       <- output.outputInt(2, repeat)
            yield ()

          private def toInt(s: String) =
            Task.effect(s.toInt)

          private def getNumbers =
            for
              lines   <- input.getData
              numbers <- ZIO.collectAll(lines.map(toInt))
            yield numbers

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