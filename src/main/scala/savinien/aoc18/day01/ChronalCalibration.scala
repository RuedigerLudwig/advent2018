package savinien.aoc18
package day01

import common._
import zio._

class ChronalService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 =
    for 
      numbers <- ChronalService.getNumbers(input)
    yield AdventIntResult(numbers.sum)

  override def part2 =
    for 
      numbers <- ChronalService.getNumbers(input)
      repeat  <- ChronalService.getRepeat(numbers)
    yield AdventIntResult(repeat)

object ChronalService:
  def getNumbers(input: AdventInput.Service) =
    for
      data   <- input.getData
      numbers <- toInt(data)
    yield numbers

  def toInt(s: String) =
    import TokenParser._

    val parser = rep(signedInteger)
    parseAll(parser, s) match
      case Success(value, _) => ZIO.succeed(value)
      case Failure(msg, _) => ZIO.fail(ReadError(msg))
      case Error(msg, _) => ZIO.fail(ReadError(msg))

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