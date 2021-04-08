package savinien.aoc18
package common

import parser.ParserError
import parser.StringParsers
import scala.util.Try
import java.time.{LocalDateTime, LocalDate, LocalTime}

object AdventParsers extends AdventParsers

trait AdventParsers extends StringParsers:
  def timeParser: Parser[LocalTime] = 
    unsignedInteger ~: (char(':') *> unsignedInteger) ^?? {
      case (hour, minute) =>
        Try(LocalTime.of(hour, minute).nn)
          .fold(_ => Left(ParserError(s"Not a correct time $hour $minute")), v => Right(v))
    }

  def dateParser: Parser[LocalDate] = 
    unsignedInteger ~: (char('-') *> unsignedInteger) ~: (char('-') *> unsignedInteger) ^?? {

      case (year, month, day) =>
        Try(LocalDate.of(year, month, day).nn)
          .fold(_ => Left(ParserError(s"Not a correct date $year $month $day")), v => Right(v))
    }
  
  def dateTimeParser: Parser[LocalDateTime] = dateParser ~: (space *> timeParser) ^^ {
    case (date, time) => LocalDateTime.of(date, time).nn
  }