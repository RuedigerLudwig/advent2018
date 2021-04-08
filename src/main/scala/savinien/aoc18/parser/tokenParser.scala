package savinien.aoc18.parser

import scala.util.Try
import java.time.{LocalDateTime, LocalDate, LocalTime}

object TokenParsers extends TokenParsers

trait TokenParsers extends StringParsers:
  private def checkedFun[A, B](f: (A => B)): PartialFunction[A, B] = new PartialFunction {
    def apply(input: A): B = f(input)
    def isDefinedAt(input: A) = Try(f(input)).isSuccess
  }

  private def checkedInt  = checkedFun[String, Int](_.toInt)
  private def checkedTime = checkedFun[(Int, Int), LocalTime]((h, m) => LocalTime.of(h, m).nn)
  private def checkedDate = checkedFun[(Int, Int, Int), LocalDate]((y, m, d) => LocalDate.of(y, m, d).nn)

  def token[A](t: Parser[A]): Parser[A] = t.bracket(hspace, hspace)

  def unsignedInteger: Parser[Int] = digits ^? checkedInt
  def signedInteger: Parser[Int] = oneOf("+-").? ~: digits ^^ { (minus, num) => minus.map(c => s"$c$num").getOrElse(num) } ^? checkedInt

  def timeParser: Parser[LocalTime] = 
    token(unsignedInteger ~: (char(':') *> unsignedInteger) ^? checkedTime)

  def dateParser: Parser[LocalDate] = 
    token(unsignedInteger ~: (char('-') *> unsignedInteger) ~: (char('-') *> unsignedInteger) ^? checkedDate)

  def dateTimeParser: Parser[LocalDateTime] = dateParser ~: timeParser ^^ {
    case (date, time) => LocalDateTime.of(date, time).nn
  }

  def lines[A](p: Parser[A]): Parser[List[A]] = sepby(p)(endOfLine) <* endOfLine.?
end TokenParsers