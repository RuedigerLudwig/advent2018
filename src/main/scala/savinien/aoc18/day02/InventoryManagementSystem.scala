package savinien.aoc18
package day02

import advent._
import zio._

class InventoryService(input: AdventInput.Service, output: AdventOutput.Service) extends SingleDay.Service:
  override def part1 = 
    (for
      lines        <- input.getData
      (two, three) <- InventoryService.count_two_three(lines)
      _            <- output.outputInt(1, two * three)
    yield ())
    .refineToOrDie[AdventException]
    .catchAll { e => output.error(1, e)}

  override def part2 = 
    (for
      lines  <- input.getData
      ticket <- InventoryService.checkDouble(lines)
      _      <- output.outputSingleLine(2, ticket)
    yield())
    .refineToOrDie[AdventException]
    .catchAll { e => output.error(1, e)}

object InventoryService:
  def count_chars(line: String) =
    def count_chars_(line: List[Char], result: Map[Char, Int]): UIO[Map[Char, Int]] =
      line match
        case Nil           => UIO.succeed(result)
        case first :: rest => {
          val map = result + (first -> (result.getOrElse(first, 0) + 1))
          count_chars_(rest, map)
        }
    count_chars_(line.toList, Map())

  def has_two_three(line: String): UIO[(Boolean, Boolean)] =
    count_chars(line).map(_.values.foldLeft((false, false)) { (tup, value) =>
      value match {
        case 2 => (true, tup._2)
        case 3 => (tup._1, true)
        case _ => tup
      }
    })

  def count_two_three(lines: List[String]): UIO[(Int, Int)] =
    lines.map(has_two_three).foldLeft(ZIO.succeed(0, 0)) {
      (count, next) => count.zipWith(next) { (count, next) => next match
        case (true, true)   => (count._1 + 1, count._2 + 1)
        case (false, true)  => (count._1, count._2 + 1)
        case (true, false)  => (count._1 + 1, count._2)
        case (false, false) => count
      }
    }

  def checkCommon(line1: String, line2: String): UIO[Option[String]] =
    val (found_differ, result) = line1.toList.zip(line2.toList).foldLeft[(Boolean, Option[String])]((false, Some(""))) {
      (acc, next) => (acc, next) match
        case ((_,      None),    (_, _))           => (false, None)
        case ((true,   _),       (a, b)) if a != b => (false, None)
        case ((false,  result),  (a, b)) if a != b => (true, result)
        case ((double, Some(s)), (a, _))           => (double, Some(s+a))
    }
    ZIO.succeed(if !found_differ then None else result)

  def checkDouble(lines: List[String]): IO[InventoryException, String] =
    def checkDouble_(first: String, rest: List[String]): UIO[Option[String]] =
      rest match
        case Nil          => UIO.succeed(None)
        case next :: tail =>
          checkCommon(first, next).flatMap { _.match
            case Some(result) => UIO.succeed(Some(result))
            case None         => checkDouble_(first, tail)
          }
    end checkDouble_

    lines match
      case Nil           => ZIO.fail(NoTicketFound())
      case first :: rest => 
        checkDouble_(first, rest).flatMap { _.match
          case Some(result) => ZIO.succeed(result)
          case None         => checkDouble(rest)
        }