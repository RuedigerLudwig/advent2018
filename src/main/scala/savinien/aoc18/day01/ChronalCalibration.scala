package savinien.aoc18
package day01

import advent._
import zio._

class ChronalService(input: AdventInput.Service, output: AdventOutput.Service) extends SingleDay.Service:
  override def part1 =
    (for 
      numbers <- ChronalService.getNumbers(input)
      _       <- output.outputInt(1, numbers.sum)
    yield ())
    .refineToOrDie[AdventException]
    .catchAll { e => output.error(1, e) } 

  override def part2 =
    (for 
      numbers <- ChronalService.getNumbers(input)
      repeat  <- ChronalService.getRepeat(numbers)
      _       <- output.outputInt(2, repeat)
    yield ())
    .refineToOrDie[AdventException]
    .catchAll { e => output.error(2, e) } 

object ChronalService:
  def getNumbers(input: AdventInput.Service): IO[AdventException, Iterable[Int]] =
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

  def toInt(s: String) =
    ZIO.effect(s.toInt)
    .catchAll(e => ZIO.fail(NumberFormatWrong(s)))

  def getRepeat(list: Iterable[Int]) =
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