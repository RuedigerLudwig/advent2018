package savinien.aoc18.parser

import scala.util.Try
import java.time.{LocalDateTime, LocalDate, LocalTime}

import StringParsers.given

object TokenParsers extends TokenParsers

trait TokenParsers extends StringParsers:
  private def checkedTry[A, B](f: (A => B)): PartialFunction[A, B] = new PartialFunction:
    private val cache                   = collection.mutable.Map.empty[A, Try[B]]
    private def check(input: A): Try[B] = cache.getOrElseUpdate(input, Try(f(input)))
    override def apply(input: A): B     = check(input).get
    override def isDefinedAt(input: A)  = check(input).isSuccess

  private def checkedOption[A, B](f: (A => Option[B])): PartialFunction[A, B] = new PartialFunction:
    private val cache                      = collection.mutable.Map.empty[A, Option[B]]
    private def check(input: A): Option[B] = cache.getOrElseUpdate(input, f(input))
    override def apply(input: A): B        = check(input).get
    override def isDefinedAt(input: A)     = check(input).isDefined

  private def checkedInt  = checkedTry[String, Int](_.toInt)
  private def checkedTime = checkedTry[(Int, Int), LocalTime]((h, m) => LocalTime.of(h, m).nn)
  private def checkedDate = checkedTry[(Int, Int, Int), LocalDate]((y, m, d) => LocalDate.of(y, m, d).nn)

  def unsignedInteger: Parser[Int] = digits ^? checkedInt
  def integer: Parser[Int] = oneOf("+-").? ~: digits ^^ { 
    case (Some('-'), num) => s"-$num"
    case (_,         num) => num
  } ^? checkedInt

  def timeParser: Parser[LocalTime] = 
    unsignedInteger.tupSep2(char(':')).token ^? checkedTime

  def dateParser: Parser[LocalDate] = 
    unsignedInteger.tupSep3(char('-')).token ^? checkedDate

  def dateTimeParser: Parser[LocalDateTime] = dateParser ~: timeParser ^^ {
    case (date, time) => LocalDateTime.of(date, time).nn
  }

  extension [A](p: Parser[A])
    def token:     Parser[A] = p.between(hspace, hspace)
    def inSquares: Parser[A] = p.between(char('['), char(']'))
    def inParens:  Parser[A] = p.between(char('('), char(')'))
    def inCurly:   Parser[A] = p.between(char('{'), char('}'))

    def lines: Parser[List[A]] = p.sepby(endOfLine) <* endOfLine.?
end TokenParsers