package savinien.aoc18
package day05

import common.*
import zio.*
import parser.StringParsers.*
import parser.*

class PolymerService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 =
    for
      data   <- input.getData
      result <- PolymerService.scanAll(data)
    yield AdventNumResult(result)

  override def part2 = 
    for
      data   <- input.getData
      result <- PolymerService.scanRemoved(data)
    yield AdventNumResult(result)

object PolyParsers:
  private def checkPolymer(use: Char => Boolean): Parser[Int] =
    def toggleCase(c: Char): Char = if c.isLower then c.toUpper else c.toLower

    def nextValid: Parser[Char] = item.flatMap {
      case c if use(c) => pure(c)
      case _           => nextValid
    }

    def checkNext(toMatch: Option[Char], level: Int): Parser[Option[Int]] = 
      nextValid.flatMap { c =>
        if toMatch.map(_==c).getOrElse(false) then pure(None)
        else checkNext(Some(toggleCase(c)), level + 1).flatMap {
          case None   => checkNext(toMatch, level)
          case result => pure(result)
        }
      } | pure(Some(level))

    checkNext(None, 0).map { _.getOrElse(0) }

  def polymer: Parser[Int] = checkPolymer(_ => true)
  def filteredPolymer(c: Char): Parser[Int] = 
    val ignore = c.toLower
    checkPolymer(_.toLower != ignore)

object PolymerService:
  def scanAll = ZioParse.parseAllToZio(PolyParsers.polymer)

  def scanRemoved(data: String): IO[AdventException, Int] =
    for
      ref    <- Ref.make(Option.empty[Int])
      _      <- ZIO.foreachPar('a' to 'z') {
                  c => parse(PolyParsers.filteredPolymer(c))(data) match
                    case Success(curr) => ref.update {
                      _.map(_.min(curr)).orElse(Some(curr))
                    }
                    case _ => ZIO.unit
                }
      min    <- ref.get
      result <- ZIO.getOrFailWith(NoMinimum)(min)
    yield result