package savinien.aoc18.parsers

import scala.util.Try
import java.time.{LocalDateTime, LocalDate, LocalTime}

object TokenParsers extends TokenParsers

trait TokenParsers extends StringParsers:
  def checkedTry[A, B](f: (A => B)): PartialFunction[A, B] = new PartialFunction:
    private val cache                   = collection.mutable.Map.empty[A, Try[B]]
    private def check(input: A): Try[B] = cache.getOrElseUpdate(input, Try(f(input)))
    override def apply(input: A): B     = check(input).get
    override def isDefinedAt(input: A)  = check(input).isSuccess

  def checkedOption[A, B](f: (A => Option[B])): PartialFunction[A, B] = new PartialFunction:
    private val cache                      = collection.mutable.Map.empty[A, Option[B]]
    private def check(input: A): Option[B] = cache.getOrElseUpdate(input, f(input))
    override def apply(input: A): B        = check(input).get
    override def isDefinedAt(input: A)     = check(input).isDefined

  private def checkedIntegral[T: Integral] = checkedOption[String, T](summon[Integral[T]].parseString)
  private def checkedTime = checkedTry[(Int, Int), LocalTime]((h, m) => LocalTime.of(h, m).nn)
  private def checkedDate = checkedTry[(Int, Int, Int), LocalDate]((y, m, d) => LocalDate.of(y, m, d).nn)

  def unsignedIntegral[T: Integral]: Parser[T] = digits ^? checkedIntegral
  def integral[T: Integral]: Parser[T] = oneOf("+-").? ~: digits ^^ { 
    case (Some('-'), num) => s"-$num"
    case (_,         num) => num
  } ^? checkedIntegral

  def integer: Parser[Int] = integral[Int]
  def unsignedInteger: Parser[Int] = unsignedIntegral[Int]
  def long: Parser[Long] = integral[Long]
  def unsignedLong: Parser[Long] = unsignedIntegral[Long]

  def timeParser: Parser[LocalTime] = 
    unsignedInteger.tupSep2(char(':')).token ^? checkedTime

  def dateParser: Parser[LocalDate] = 
    unsignedInteger.tupSep3(char('-')).token ^? checkedDate

  def dateTimeParser: Parser[LocalDateTime] = dateParser ~: timeParser ^^ {
    case (date, time) => LocalDateTime.of(date, time).nn
  }

  def configValue[T](name: String, parser: Parser[T]): Parser[T] =
    ((string(name) ~: char('=').token) *> parser).token

  extension [A](p: Parser[A])
    def token:     Parser[A] = p.inside(hspace, hspace)
    def inSquares: Parser[A] = p.inside(char('['), char(']'))
    def inParens:  Parser[A] = p.inside(char('('), char(')'))
    def inCurly:   Parser[A] = p.inside(char('{'), char('}'))
    def inAngles:  Parser[A] = p.inside(char('<'), char('>'))

    def lines: Parser[List[A]] = p.sepMany(endOfLine) <* endOfLine.?
end TokenParsers