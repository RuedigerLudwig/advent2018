package savinien.aoc18
package day01

import advent.output.AdventOutput
import advent.input.AdventInput
import advent.days.SingleDay
import advent.AdventException
import zio._

sealed trait ChronalException extends AdventException { self =>
  override def toString() = self match
    case MultiError(l) => l.length match
      case 0 => "Empty Error list"
      case 1 => l(0).toString()
      case _ => f"MultiError: $l"
    case NumberFormatWrong(s) => f"NumberFormatWrong: $s"
}
case class MultiError(l: List[ChronalException]) extends ChronalException
case class NumberFormatWrong(s: String) extends ChronalException

object ChronalCalibration:
  def live: URLayer[AdventInput with AdventOutput, SingleDay] =
    ZLayer.fromServices[
        AdventInput.Service
      , AdventOutput.Service
      , SingleDay.Service
    ] { (input, output) =>
      new SingleDay.Service {



        override def part1: UIO[Unit] =
          (for 
            numbers <- getNumbers
            _       <- output.outputInt(1, numbers.sum)
          yield ())
          .catchAll { e => output.error(1, f"Got Error: $e") } 

        override def part2: UIO[Unit] =
          (for 
            numbers <- getNumbers
            repeat  <- getRepeat(numbers)
            _       <- output.outputInt(2, repeat)
          yield ())
          .catchAll { e => output.error(2, f"Got Error: $e") } 

        private def toInt(s: String) =
          ZIO.effect(s.toInt)
          .catchAll(e => ZIO.fail(NumberFormatWrong(s)))

        private def getNumbers: IO[AdventException, Iterable[Int]] =
          (for
            lines   <- input.getData
            numbers <- ZIO.partition(lines)(toInt).flatMap {(errors, numbers) =>
              if !errors.isEmpty then
                IO.fail(MultiError(errors.toList))
              else
                UIO.succeed(numbers)
            }
          yield numbers)
          .refineToOrDie[AdventException]

        private def getRepeat(list: Iterable[Int]) =
          ZIO.succeed(
              LazyList
              .continually(list)
              .flatten
              .scanLeft((0, Set[Int]())) {
                case ((last, set), next) => ((last + next), set + last)
              }
              .dropWhile {
                (last, set) => !set.contains(last)
              }
              .head
              ._1
          )
      }
    }