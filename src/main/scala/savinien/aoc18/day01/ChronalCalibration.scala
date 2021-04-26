package savinien.aoc18
package day01

import common.*
import parser.TokenParsers.*
import zio.*

class ChronalService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 =
    for 
      numbers <- ChronalService.getNumbers(input)
    yield AdventNumResult(numbers.sum)

  override def part2 =
    for 
      numbers <- ChronalService.getNumbers(input)
      repeat  <- ChronalService.getRepeat(numbers)
    yield AdventNumResult(repeat)

private object ChronalService:
  def getNumbers(input: AdventInput.Service) =
    for
      data    <- input.getData
      numbers <- toIntList(data)
    yield numbers

  def toIntList = ZioParse.parseAllToZio(integer.lines)

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