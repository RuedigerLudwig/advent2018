package savinien.aoc18
package parsers

import java.time.{LocalDateTime, LocalDate, LocalTime}
import common.ParticalHelper.*

object TokenParsers extends TokenParsers

trait TokenParsers extends StringParsers:
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
  def configValue2[T](name: String, parser: Parser[T]): Parser[T] =
    ((string(name) ~: char(':').token) *> parser).token

  extension [A](p: Parser[A])
    def trim:      Parser[A] = p.inside(space, space)
    def token:     Parser[A] = p.inside(hspace, hspace)
    def inSquares: Parser[A] = p.inside(char('['), char(']'))
    def inParens:  Parser[A] = p.inside(char('('), char(')'))
    def inCurly:   Parser[A] = p.inside(char('{'), char('}'))
    def inAngles:  Parser[A] = p.inside(char('<'), char('>'))

    def line: Parser[A] = p <* endOfLine.?
    def lines: Parser[List[A]] = p.sepMany(endOfLine) <* endOfLine.?
end TokenParsers